(ns beotf.parser
  (:require (clojure string zip)))

(defn signature-fn
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
  `(signature-fn #'~f))

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

(defn parse-and-walk
  "given parse signatures and transform instructions, parses and transforms given plain text"
  [sigs instructions plain]
  (let [tree (parse sigs plain)]
    (tree-walk tree instructions)))

(defn parser-fn
  "given a list of transform function variables generates a proper beotf-parser for those transformations"
  [document-root-var join-var parenthesis-var & more]
  (let [[document-root-fn join-fn parenthesis-fn] (map var-get [document-root-var join-var parenthesis-var])
        get-name   (fn [v] (clojure.string/replace-first (:name (meta v)) #"(.*-)" ""))
        to-keyword (fn [v] (keyword (get-name v)))
        to-symbol  (fn [v] (symbol  (get-name v)))
        p          (fn [wrapped-doc-root-fn plain] 
                     (parse-and-walk 
                       (reduce (fn [h i] (assoc h (to-keyword i) (signature-fn i))) ; keyword -> signature 
                               {nil (signature-fn document-root-var)} 
                               more) 
                       (reduce (fn [h i] (assoc h (to-symbol  i) (var-get i)))      ; symbol -> function
                               {:document-root wrapped-doc-root-fn 
                                :join join-fn
                                :parenthesis parenthesis-fn} 
                               more)  
                       plain))]
    (fn
      ([plain]         (p document-root-fn plain))
      ([context plain] (p (fn [& more] (apply document-root-fn context more)) plain)))))

(defmacro parser
  "given a list of transform functions, generates a proper beotf-parser for those transformations"
  [document-root join parenthesis & fn-vars]
  (let [var-list (map #(list 'var %) fn-vars)]
    `(parser-fn #'~document-root #'~join #'~parenthesis ~@var-list)))

