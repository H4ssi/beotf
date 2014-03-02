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

(declare parse tree-walk beotf-html)

(defn beotf
  "Simple string manipulation based implementation of bare specs, transforms plain blog syntax to html"
  [plain]
  (beotf-html plain))

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
                                            (conj buffer (str c)))))
        document (concat [\( :document-root] plain [\) :eof])] ; we wrap the whole document in an additional form, so that the parse logic gets easier
    (loop [[c & cs] document
           buffers nil ; when parsing nested structure, current state (outer data) is put onto stack, if inner data is parsed 
           buffer [] ; parsed data is stored in the buffer, until its kind is actually known (e.g. symbol, parameter, complete form)
           modes nil ; mode stores the signature of arglist to parse (e.g. [:line], [:block]), the array is popped during parsing, its first element is the type of next arg
           mode nil
           forms nil ; stores parsed forms
           form []
           prev-was-paren? false] ; true if the previous symbol was a open parenthesis
      (case c
        ; artificial start of document, just set the overall document mode (nil in sigs)
        :document-root 
        (recur cs buffers buffer modes (normalize-signature (get sigs nil [:block])) forms form false)

        ; on eof, there is exactly :document-root form in our buffer
        :eof 
        (vec (first buffer))

        ; on ( prepare to start parsing the next function
        \( 
        (recur cs
               (conj buffers buffer) 
               []
               (conj modes mode)
               (if prev-was-paren?
                 (normalize-signature (get sigs () [:block])) ; got double parenthesis, continue parsing args (skip function symbol)
                 [:symbol]) ; otherwise, need to wait for the function symbol of the form, before mode is known
               (conj forms form) ; put current form on hold
               [] ; new form is empty for now
               (not prev-was-paren?))

        ; on ) complete parsed function/form, and only on ) continue parsing
        :form
        (let [form-vector (conj-nil form (flatten-singleton buffer))] ; construct completed form 
          (recur cs 
                 (rest buffers)
                 (conj (first buffers) (sequence form-vector)) ; add completed form to previous buffer
                 (rest modes)
                 (first modes)    ; restore the previous mode
                 (rest forms)     ; restore the previous forms                  
                 (first forms)    ; restore previous form
                 false))

        ; whitespace  and ) may be end of buffer/token
        (\) \space \tab \newline \return)
        (let [m (first mode)
              end-of-form (= c \))
              needs-buffer (contains? #{:symbol :word} m)
              is-break (contains? #{\newline \return} c)
              next-cs (if end-of-form (conj cs :form) cs)] ; sneak in a special iteration ":form" when ) is parsed
          (cond
            (and (not end-of-form) needs-buffer (empty? buffer)) ; just skip spaces if buffer is needed but still empty
            (recur next-cs buffers buffer modes mode forms form false)

            (and 
              (= m :symbol) 
              (not (empty? buffer))
              (not (seq? (first buffer)))) ; if we have a sequence on the buffer its a double parenthesis of some kind 
            (let [s (apply str buffer)
                  k (keyword s)]
              (if (contains? sigs k) ; get signature of corresponding function
                (recur
                  next-cs
                  buffers
                  []
                  modes
                  (normalize-signature (k sigs))
                  forms
                  [(symbol s)]
                  false)
                (throw (Exception. (str "no such operation: " buffer)))))

            (or 
              end-of-form
              (= m :word) ; got word parameter
              (and is-break (= m :line))) ; got line param
            (recur 
              next-cs
              buffers 
              []
              modes
              (normalize-signature (subvec mode 1))
              forms
              ((if end-of-form conj-nil conj) form (flatten-singleton buffer))
              false)

            :else ; continue parsing
            (recur next-cs buffers (conj-char-buffer buffer c) modes mode forms form false)))

        ; otherwise, just continue parsing
        (recur cs buffers (conj-char-buffer buffer c) modes mode forms form false)))))

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
                       (apply (select :document-root) node)

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

(defn emit-ps 
  "emits html paragraphs"
  [& ^:line ps]
  (clojure.string/join 
    (map ; wrap in p tags
         #(str "<p>" % "</p>")
         (filter ; filter empty
                 #(seq %) 
                 (map ; trim
                      clojure.string/trim
                      (filter ; filter non null
                              #(not (nil? %))  
                              ps))))))

(defn emit-h
  "emits html headers"
  [^:line h & ^:line ps]
  (str "<h2>" h "</h2>" (apply emit-ps ps)))

(defn emit-b
  "emits html bold"
  [^:block b]
  (str "<b>" b "</b>"))

(defn emit-link
  "emits html a href link"
  [^:word url ^:block link]
  (str "<a href=\"" url "\">" link "</a>"))

(defn emit-parenthesis
  "emits () when (()) is used in plain text"
  [i] (str \( (first (first i)) \)))

(defn emit-join
  "since transformation is done into string, join will just concat them"
  [v] (clojure.string/join v))

(defn beotf-parse 
  "given parse signatures and transform instructions, parses and transforms given plain text"
  [sigs instructions plain]
  (let [tree (parse sigs plain)]
    (tree-walk tree instructions)))

(defmacro beotf-parser
  "given a list of transform functions, generates a proper beotf-parser for those transformations"
  [document-root join parenthesis & fn-vars]
  (let [var-list (map (fn [x] (list 'var x)) fn-vars)] ; transforms (x y) to ((var x) (var y))
    `(let [~'get-name (fn [~'v] (clojure.string/replace-first (:name (meta ~'v)) #"(.*-)" ""))]
       #(beotf-parse 
          (reduce (fn [~'h ~'i] (assoc ~'h (keyword (~'get-name ~'i)) (signature-fn-var ~'i))) {nil (signature ~document-root)} (list ~@var-list)) 
          (reduce (fn [~'h ~'i] (assoc ~'h (symbol  (~'get-name ~'i)) (var-get ~'i))) {:document-root ~document-root :join ~join :parenthesis ~parenthesis} (list ~@var-list))  
          %))))

(def beotf-html (beotf-parser emit-ps emit-join emit-parenthesis emit-h emit-b emit-link)) 

