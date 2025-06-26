(defproject lambda_mu "0.1.0-SNAPSHOT"
  :description "Lambda-Mu Calculus Interpreter in Clojure"
  :url "https://example.com/lambdamu"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [org.clojure/test.check "1.1.1"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :main lambda_mu.repl)
