(ns beotf.core
  (:gen-class)
  (:require clojure.string clojure.zip))

(declare beotf-file)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (empty? args)
    (println (str "Usage: <file-name>"))
    (let [file-name (first args)]
      (println (str "Converting " file-name "..."))
      (beotf-file file-name)
      (println "Done!"))))

(declare parse)
(declare tree-walk)

(defn beotf
  "Simple string manipulation based implementation of bare specs, transforms plain blog syntax to html"
  [plain]
  (let [tree (parse {nil [[:line]]} plain)]
    (tree-walk tree {:document-root (fn [ls] 
                                      (clojure.string/join 
                                        (map ; wrap in p tags
                                             #(str "<p>" % "</p>")
                                             (filter ; filter empty
                                                     #(seq %) 
                                                     (map ; trim
                                                          clojure.string/trim
                                                          (filter ; filter non null
                                                                  #(not (nil? %))  
                                                                  ls))))))
                     :parenthesis (fn [i] (str \( (first (first i)) \)))
                     :join (fn [v] (clojure.string/join v)) })))

(defn layout
  "Makes a nice, complete html page, you need to supply a title, and the plain blog syntax"
  [title plain]
  (str "<!DOCTYPE html>
       <html lang=\"en\">
       <head>
       <meta charset=\"utf-8\">
       <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
       <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
       <title>" 
       title 
       "</title>
       <link href=\"//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css\" rel=\"stylesheet\">
       </head>
       <body>
       <div class=\"container\">
       <h1>" 
       title
       "</h1>"
       (beotf plain)
       "</div>
       <script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>
       <script src=\"//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js\"></script>
       </body>
       </html>"))

(defn beotf-file
  "Takes a beotf plain input file, and makes a HTML output file"
  [file-name]
  (spit (str file-name ".html") (layout file-name (slurp file-name))))

(defn signature-fn-var
  "Given function var, it determines the signature for the corresponding blo plain text form"
  [function-var]
  (let [args  (first (:arglists (meta function-var)))
        token (fn [arg]
                (first (first (select-keys (meta arg) [:word :line :block]))))
        next  (fn [sig n] 
                (if (= '& n)
                  (conj sig []) ; create nested list, when we reach &
                  (if (vector? (peek sig)) ; are we already in nested list?
                    (conj (pop sig) (conj (peek sig) (token n))) ; append to nested list
                    (conj sig (token n))))) ; append to outer list
        sig   (reduce next [] args)]
    ; our signature is nearly ready, what is left, is to apply default values, if no meta data was specified
    (if (= [nil] sig)
      [:block] ; a func with a single arg will accept whole block
      (mapv #(if (vector? %) ; handle nested [] again, we want to replace nil with :line
               (mapv (fnil identity :line) %) ; fnil identity is like coalesce
               ((fnil identity :line) %)) sig))))

(defmacro signature
  "Given a function, with properly annotated arguments, it determines the signature for the corresponding blog plain text form"
  [f]
  `(signature-fn-var #'~f))
  
(defn- normalize-signature
  "transforms signature in a normalform, which helps at parsing,
  e.g. [[:word]] -normalize-> [:word [:word]] -normalize-> [:word [:word]]"
  [sig]
  (if (vector? (first sig))
    (reduce into [(first sig) [(first sig)] (subvec sig 1)])
    sig))

(defn parse
  "parses a plain text string given signatures of to be parsed symbols"
  [sigs plain]
  (let [conj-nil (fn [coll e] (if (nil? e)
                                coll
                                (conj coll e)))
        flatten-singleton (fn [coll] (case (count coll) 
                                       (0 1) (first coll) 
                                       coll))
        conj-char-buffer (fn [buffer c] (let [l (peek buffer)]
                                          (if (string? l)
                                            (conj (pop buffer) (str l c))
                                            (conj buffer (str c)))))]
    (loop [[c & cs] plain
           buffers nil
           buffer []
           modes nil
           mode (normalize-signature (get sigs nil [:block]))
           forms nil
           form []]
      (case c
        ; on ( prepare to start parsing the next function
        \( (recur cs
                  (conj buffers buffer) 
                  []
                  (conj modes mode)
                  nil ; need to wait for the function symbol, before mode is known
                  (conj forms form) ; put current form on hold
                  []) ; new form is empty for now

        ; on ) and EOF complete parsed function/form, and only on ) continue parsing
        (\) nil) (let [form-vector (conj-nil form (flatten-singleton buffer))] ; construct completed form 
                   (if (nil? c)
                     form-vector ; got EOF
                     (recur cs   ; got ) 
                            (rest buffers)
                            (conj (first buffers) (sequence form-vector)) ; add completed form to previous buffer
                            (rest modes)
                            (first modes)    ; restore the previous mode
                            (rest forms)     ; restore the previous forms                  
                            (first forms)))) ; restore previous form

        ; whitespace may be end of buffer/token
        (\space 
          \tab 
          \newline 
          \return) (let [m (first mode),
                         needs-buffer (some #(= m %) [nil :word])
                         is-break (some #(= c %) [\newline \return])]
                     (cond
                       (and needs-buffer (empty? buffer))
                       (recur cs buffers buffer modes mode forms form)

                       (nil? m) ; got function symbol   
                       (let [s (apply str buffer)
                             k (keyword s)]
                         (if (k sigs) ; get signature of corresponding function
                           (recur
                             cs
                             buffers
                             []
                             modes
                             (normalize-signature (k sigs))
                             forms
                             [(symbol s)])
                           (throw (Exception. (str "no such operation: " buffer)))))

                       (or 
                         (= m :word) ; got word parameter
                         (and is-break (= m :line))) ; got line param
                       (recur 
                         cs
                         buffers 
                         []
                         modes
                         (normalize-signature (subvec mode 1))
                         forms
                         (conj form (flatten-singleton buffer)))

                       :else ; continue parsing
                       (recur cs buffers (conj-char-buffer buffer c) modes mode forms form)))

        ; otherwise, just continue parsing
        (recur cs buffers (conj-char-buffer buffer c) modes mode forms form)))))

(defn tree-walk 
  "gets a tree and transform instructions, walks that tree and collects all transformation requests, then transforms the tree"
  ([tree instructions] (tree-walk tree instructions nil))
  ([tree instructions label]
   (let [is-form? (fn [f] (and (seq? f) (symbol? (first f))))
         can-have-children? (fn [b] (or (seq? b) (vector? b)))
         get-children (fn [b] (if (is-form? b) (next b) (seq b))) ; care () != nil
         set-children (fn [b cs] (if (is-form? b)
                                   (list* (first b) cs)
                                   (into (empty b) cs)))

         z (clojure.zip/zipper can-have-children? get-children set-children tree)

         select (fn [func-key] ((fnil identity identity) (get instructions func-key)))

         transform (fn [node is-root?]
                     (cond
                       is-root? 
                       ((select :document-root) node)

                       (string? node) 
                       ((select :string) node)

                       (vector? node) 
                       ((select :join) node)

                       (and (seq? node) (seq node) (seq? (first node))) ; double ((...))
                       ((select :parenthesis) node)

                       (and (seq? node) (seq node) (contains? instructions (first node))) ; applicable form
                       (apply (select (first node)) (rest node))

                       :else
                       node))]
     (loop [forward z
            prev nil]
       (if (not (clojure.zip/end? forward))

         ; go forward
         (recur (clojure.zip/next forward) 
                forward)
         ; go backward  
         (loop [backward prev
                prev2 nil]
           (if (not (nil? backward))
             ; now we join the tree
             (let [n (clojure.zip/node backward)
                   r (clojure.zip/replace backward (transform n (not (clojure.zip/up backward))))]
               (recur (clojure.zip/prev r) r))

             ; we are finished!
             (clojure.zip/node prev2))))))))                           
