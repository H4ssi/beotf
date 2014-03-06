(ns beotf.core
  (:gen-class)
  (:require (clojure string zip))
  (:require (beotf [parser :as p])))

(declare beotf-file beotf-html)

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
  ([plain]           (beotf-html plain))
  ([file-name plain] (beotf-html {:file-name file-name} plain)))

(defn beotf-file
  "Takes a beotf plain input file, and makes a HTML output file"
  [file-name]
  (spit (str file-name ".html") (beotf file-name (slurp file-name))))



(defn- helper-ps 
  "emits html paragraphs"
  [ps]
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

(defn doc-root
  "Makes a nice, complete html page, you need to supply a title, and the plain blog syntax"
  [context & ^:line doc]
  (if (map? context)
    (str "<!DOCTYPE html>
         <html lang=\"en\">
         <head>
         <meta charset=\"utf-8\">
         <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
         <title>" 
         (:file-name context) 
         "</title>
         <link href=\"//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css\" rel=\"stylesheet\">
         </head>
         <body>
         <div class=\"container\">
         <h1>" 
         (:file-name context)
         "</h1>"
         (helper-ps doc)
         "</div>
         <script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>
         <script src=\"//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js\"></script>
         </body>
         </html>")
    (helper-ps (apply list context doc))))

(defn emit-h
  "emits html headers"
  [^:line h & ^:line ps]
  (str "<h2>" h "</h2>" (helper-ps ps)))

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

(def beotf-html (p/parser doc-root emit-join emit-parenthesis emit-h emit-b emit-link)) 

