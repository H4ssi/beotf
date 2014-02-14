(ns beotf.core-spec
  (:require [speclj.core :refer :all]
            [beotf.core :refer :all]))

(describe "transformation from plain text to html"
          (it "transforms lines to paragraphs" (pending)
              (should= "<p>as df</p>" (beotf "as df"))
              (should= "<p>as df</p>
                       <p>bs df</p>"  (beotf "as df
                                             bs df"))
              (should= "<p>as df</p>
                       <p>bs df</p>"  (beotf "as df

                                             bs df")))
          (it "transforms double parenthesis to actual parenthesis" (pending)
              (should= "<p>as(bs)df</p>" (beotf "as((bs))df"))))

(run-specs)
