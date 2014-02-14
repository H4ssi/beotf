(ns beotf.core
  (:gen-class)
  (:require clojure.string))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn beotf
  "Simple string manipulation based implementation of bare specs, transforms plain blog syntax to html"
  [plain]
  (let [lines           (clojure.string/split-lines plain)
        trimmed-lines   (map clojure.string/trim lines)
        non-empty-lines (filter #(seq %) trimmed-lines)
        parenthesis     (map #(clojure.string/replace (clojure.string/replace % #"\(\(" "(") #"\)\)" ")") non-empty-lines)
        paragraphs      (map #(str "<p>" % "</p>") parenthesis)]
    (clojure.string/join paragraphs)))
