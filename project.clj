(defproject lambda-mu "0.1.0-SNAPSHOT"
  :description "A λμ-calculus interpreter with parser, evaluator, and REPL"
  :url "https://github.com/yourname/lambda-mu"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [instaparse "1.4.12"]] ; For parser.clj

  :source-paths ["src"]
  :test-paths ["test"]

  :main lambda_mu.repl ; Launches the REPL via -main

  :repl-options {:init-ns lambda_mu.repl}

  :aliases {"run"  ["trampoline" "run" "-m" "lambda_mu.repl"]})

