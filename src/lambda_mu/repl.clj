(ns lambda_mu.repl
  (:require [lambda_mu.parser :refer [parse]]
            [lambda_mu.evaluator :as eval]
            [lambda_mu.expr :refer [->Var ->Lam ->App ->Freeze ->Mu]])
  (:import  [lambda_mu.expr Var Lam App Freeze Mu]))

(defn pretty-print [expr]
  (cond
    (instance? Var expr) (:name expr)
    (instance? Lam expr) (str "λ" (:param expr) "." (pretty-print (:body expr)))
    (instance? Mu expr) (str "μ" (:alpha expr) "." (pretty-print (:expr expr)))
    (instance? Freeze expr) (str "[" (:alpha expr) "]" (pretty-print (:expr expr)))
    (instance? App expr) (str "(" (pretty-print (:fun expr)) " " (pretty-print (:arg expr)) ")")
    :else (str expr)))

(defn print-steps [exprs]
  (doseq [[i e] (map-indexed vector exprs)]
    (println (str "[" i "] " (pretty-print e)))))

(defn read-eval-print [line]
  (cond
    (and (.startsWith line "let ") (.contains line "="))
    (let [[_ name rhs] (re-matches #"let (\w+)\s*=\s*(.*)" line)]
      (try
        (let [parsed (parse rhs)]
          (swap! eval/env assoc name (eval/resolve-expr parsed))
          (println (str "Defined " name ".")))
        (catch Exception e
          (println "Definition error:" (.getMessage e)))))

    (.startsWith line "save \"")
    (let [filename (second (re-matches #"save \"(.*)\"" line))]
      (try
        (spit filename
              (apply str (map (fn [[k v]] (str "let " k " = " (pretty-print v) "\n")) @eval/env)))
        (println (str "Environment saved to '" filename "'."))
        (catch Exception e
          (println "Could not save:" (.getMessage e)))))

    (.startsWith line "load \"")
    (let [filename (second (re-matches #"load \"(.*)\"" line))]
      (try
        (doseq [l (line-seq (java.io.BufferedReader. (java.io.FileReader. filename)))]
          (println ">" l)
          (read-eval-print l))
        (catch Exception e
          (println "Could not load:" (.getMessage e)))))

    :else
    (try
      (let [parsed (parse line)
            resolved (eval/resolve-expr parsed)]
        (print-steps (eval/eval-steps resolved)))
      (catch Exception e
        (println "Error:" (.getMessage e))))))

(defn -main []
  (println "λμ-Calculus REPL (Clojure). Type :q to quit.")
  (loop []
    (print "λμ> ") (flush)
    (let [line (read-line)]
      (when (not= line ":q")
        (read-eval-print line)
        (recur))))
  (println "Goodbye."))

