(ns beotf.core-spec
  (:require [speclj.core :refer :all]
            [beotf.core :refer :all]
            clojure.string))

(defn i
  "html identity hack: TODO proper solution"
  [l r]
  (let [trim (fn [s] (clojure.string/join (map clojure.string/trim (clojure.string/split-lines s))))]
    (= (trim l) (trim r))))

(describe "transformation from plain text to html"
          (it "transforms lines to paragraphs"
              (should (i "<p>as df</p>" (beotf "as df")))
              (should (i "<p>as df</p>
                         <p>bs df</p>" (beotf "as df
                                              bs df")))
              (should (i "<p>as df</p>
                         <p>bs df</p>" (beotf "as df

                                              bs df"))))
          (it "transforms double parenthesis to actual parenthesis"
              (should (i "<p>as(bs)df</p>" (beotf "as((bs))df")))))

(run-specs)
