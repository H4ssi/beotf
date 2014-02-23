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

; these functions will be used in the next tests
(defn- signature-test-block          [b] 1)
(defn- signature-test-lineline       [a b] 1)
(defn- signature-test-lines          [& a] 1)
(defn- signature-test-linelinelines  [a b & c] 1)
(defn- signature-test-blocke         [^:block b] 1)
(defn- signature-test-worde          [^:word w] 1)
(defn- signature-test-linee          [^:line l] 1)
(defn- signature-test-wordse         [& ^:word w] 1)
(defn- signature-test-linese         [& ^:line l] 1)
(defn- signature-test-wordlinelines  [^:word w ^:line l & ls] 1)

(describe "extraction of signature of function definitions"
          (it "applies sensible defaults"
              (should= [:block]              (signature signature-test-block))
              (should= [:line :line]         (signature signature-test-lineline))
              (should= [[:line]]             (signature signature-test-lines))
              (should= [:line :line [:line]] (signature signature-test-linelinelines)))

          (it "honors meta data information"
              (should= [:block]              (signature signature-test-blocke))
              (should= [:word]               (signature signature-test-worde))
              (should= [:line]               (signature signature-test-linee))
              (should= [[:word]]             (signature signature-test-wordse))
              (should= [[:line]]             (signature signature-test-linese))
              (should= [:word :line [:line]] (signature signature-test-wordlinelines))))

; todo: some negative tests! 
(let [sigs {nil [:block]
            :a [:block]
            :b [:line]
            :c [:line :line]
            :d [:word :block]
            :e [:word :line [:line]]}]
  (describe "parsing of plain text"
            (it "can parse a single form" (pending)
                (should= '[(a "block\nblock\nblock")] (parse sigs "(a block\nblock\nblock)")))

            (it "can parse nested forms" (pending)
                (should= '[(a (a "block\nblock"))] (parse sigs "(a (a block\nblock))")))

            (it "can parse consecutive forms" (pending)
                (should= '[[(a "a\na") (a "b\nb")]] (parse sigs "(a a\na)(a b\nb)")))

            (it "can parse mixed forms and strings" (pending)
                (should= '[["pre " (a "a") " post"]] (parse sigs "pre (a a) post"))
                (should= '[(b ["pre " (b "in") " post"])] (parse sigs "(b pre (b in) post)")))

            (it "respects form signature" (pending)
                (should= '[(c "asdf bsdf" "csdf dsdf")] (parse sigs "(c asdf bsdf\ncsdf dsdf)"))
                (should= '[(d "word" "asdf\nbsdf")] (parse sigs "(d word asdf\nbsdf)"))
                (should= '[(d (a "form word") "asdf\nbsdf")] (parse sigs "(d (a form word) asdf\nbsdf)"))
                (should= '[(e "word" "line asdf" "line bsdf" "line csdf")] (parse sigs "(e word line asdf\nline bsdf\nline csdf)")))

            (it "respects spaces" (pending)
                (should= '[(d ["pre" (a "form word") "post"] "asdf\nbsdf")] (parse sigs "(d pre(a form word)post asdf\nbsdf)")))

            (it "respects top level signature" (pending)
                (should= '["one" "two" "three"] (parse {nil [[:word]]} "one two three")))

            (it "parses double parenthesis" (pending)
                (should= '[(())] (parse sigs "(())")))))

(run-specs)
