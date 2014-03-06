(ns beotf.core
  (:gen-class)
  (:require (clojure string zip))
  (:require (beotf [parser :as p] [html :as h])))

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

(def beotf-html (p/parser h/doc-root h/emit-join h/emit-parenthesis h/emit-h h/emit-b h/emit-link)) 

