(defproject beotf "0.1.0-SNAPSHOT"
  :description "Blog Engine Of The Future"
  :url "https://github.com/H4ssi/beotf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot beotf.core
  :target-path "target/%s"
  :plugins [[speclj "2.9.1"]]
  :profiles {:dev {:dependencies [[speclj "2.9.1"]]}
             :uberjar {:aot :all}}
  :test-paths ["spec"])
