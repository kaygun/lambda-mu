(defproject lambda-mu "0.1.0-SNAPSHOT"
  :description "A λμ-calculus interpreter in Clojure"
  :url "https://example.org/lambda-mu"
  :license {:name "MIT License"
            :url  "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [instaparse "1.5.0"]]

  :main lambda-mu.repl
  :source-paths ["src"]
  :profiles {:uberjar {:aot :all}})
