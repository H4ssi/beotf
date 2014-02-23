(ns beotf.core
  (:gen-class)
  (:require clojure.string))

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

(defn beotf
  "Simple string manipulation based implementation of bare specs, transforms plain blog syntax to html"
  [plain]
  (let [lines           (clojure.string/split-lines plain)
        trimmed-lines   (map clojure.string/trim lines)
        non-empty-lines (filter #(seq %) trimmed-lines)
        parenthesis     (map #(clojure.string/replace (clojure.string/replace % #"\(\(" "(") #"\)\)" ")") non-empty-lines)
        paragraphs      (map #(str "<p>" % "</p>") parenthesis)]
    (clojure.string/join paragraphs)))

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

(defn parse [])
