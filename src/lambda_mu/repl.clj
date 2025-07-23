(ns lambda_mu.repl
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [lambda_mu.parser :refer [parse-expr]]
            [lambda_mu.evaluator :refer [eval-expr]]
            [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]])
  (:import  [lambda_mu.types Var Lam Appl Cont Mu]))

;; === REPL ===
(defn resolve-expr [expr env]
  (condp instance? expr
    Var  (if (some #(= (:name %) (:name expr)) env)
           expr
           (get env (:name expr) expr))
    
    Appl (->Appl (resolve-expr (:head expr) env)
                 (resolve-expr (:arg expr) env))

    Cont (->Cont (resolve-expr (:head expr) env)
                 (resolve-expr (:arg expr) env))

    Lam  (->Lam (:param expr)
                (resolve-expr (:body expr) (conj env (:param expr))))

    Mu   (->Mu (:param expr)
               (resolve-expr (:body expr) (conj env (:param expr))))

    :else expr))

(defn pretty-print [expr]
  (condp instance? expr
    Var  (:name expr)

    Lam  (str "λ" (:name (:param expr)) "." (pretty-print (:body expr)))

    Mu   (str "μ" (:name (:param expr)) "." (pretty-print (:body expr)))
    
    Cont (str "[" (pretty-print (:head expr)) "] " (pretty-print (:arg expr)))

    Appl (str (pretty-print (:head expr))
              (if (instance? Var (:arg expr))
                " "
                " (")
              (pretty-print (:arg expr))
              (if (instance? Var (:arg expr)) "" ")"))

    :else (str expr)))

(declare handle-line handle-expr handle-let)

;; REPL
(defn show-help []
  (println "Commands:")
  (println "  let <n> = <expr>    - Define a variable")
  (println "  :load \"<file>\"     - Load definitions from file")
  (println "  :save \"<file>\"     - Save environment to file")
  (println "  :env               - Show current environment")
  (println "  :help              - Show this help")
  (println "  :q                 - Quit")
  (println "  <expr>             - Evaluate expression"))

(defn show-env [env]
  (if (empty? env)
    (println "Empty environment")
    (doseq [[name expr] env]
      (println (str name " = " (pretty-print expr))))))

(defn handle-let [line env]
  (let [parts (str/split (subs line 4) #"=" 2)]
    (if (= 2 (count parts))
      (let [[name expr-str] (map str/trim parts)]
        (try
          (let [expr (parse-expr expr-str)
                resolved (resolve-expr expr env)]
            (println (str "let " name " = " (pretty-print resolved)))
            (assoc env name resolved))
          (catch Exception e
            (println "Error:" (.getMessage e))
            env)))
      (do (println "Error: malformed let statement. Use: let <n> = <expr>")
          env))))

(defn handle-load [filename env]
  (try
    (with-open [reader (io/reader filename)]
      (reduce (fn [env line]
                (let [trimmed-line (str/trim line)]
                  (if (seq trimmed-line)  ; Check if non-empty after trimming
                    (do (println ">" trimmed-line)
                        (handle-line trimmed-line env))
                    env)))
              env
              (line-seq reader)))
    (catch Exception e
      (println "Error loading" filename ":" (.getMessage e))
      env)))

(defn handle-save [filename env]
  (try
    (with-open [writer (io/writer filename)]
      (doseq [[name expr] env]
        (.write writer (str "let " name " = " (pretty-print expr) "\n")))
      (println "Environment saved to" filename))
    (catch Exception e
      (println "Error saving to" filename ":" (.getMessage e))))
  env)

(defn handle-expression [line env]
  (try
    (-> line parse-expr (resolve-expr env) eval-expr pretty-print println)
    (catch Exception e
      (println "Error:" (.getMessage e))))
  env)

(defn handle-line [line env]
  (let [line (str/trim line)]
    (cond
      (empty? line) env
      (= ":help" line) (do (show-help) env)
      (= ":env" line) (do (show-env env) env)
      (and (str/starts-with? line ":load \"") (str/ends-with? line "\""))
      (handle-load (subs line 7 (dec (count line))) env)
      (and (str/starts-with? line ":save \"") (str/ends-with? line "\""))
      (handle-save (subs line 7 (dec (count line))) env)
      (and (str/starts-with? line "let ") (str/includes? line "="))
      (handle-let line env)
      :else (handle-expression line env))))

(defn -main []
  (println "λμ-Calculus REPL (Clojure). Type :q to quit, :help for commands.")
  (loop [env {}]
    (print "λμ> ")
    (flush)
    (let [input (read-line)]
      (cond
        (or (nil? input) (= ":q" input))
        (println "Goodbye!")
        :else
        (recur (handle-line input env))))))
