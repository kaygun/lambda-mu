(ns lambda_mu.repl
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [lambda_mu.parser :refer [parse-expr]]
            [lambda_mu.evaluator :refer [eval-expr]]
            [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]])
  (:import [lambda_mu.types Var Lam Appl Cont Mu]))

(defn resolve-expr [expr env]
  (condp instance? expr
    Var   (get env (:name expr) expr)
    Appl  (let [h' (resolve-expr (:head expr) env)
                a' (resolve-expr (:arg expr) env)]
            (if (and (identical? h' (:head expr)) (identical? a' (:arg expr)))
              expr
              (->Appl h' a')))
    Cont  (let [h' (resolve-expr (:head expr) env)
                a' (resolve-expr (:arg expr) env)]
            (if (and (identical? h' (:head expr)) (identical? a' (:arg expr)))
              expr
              (->Cont h' a')))
    Lam   (let [param-name (:name (:param expr))
                inner-env (dissoc env param-name)
                b' (resolve-expr (:body expr) inner-env)]
            (if (identical? b' (:body expr))
              expr
              (->Lam (:param expr) b')))
    Mu    (let [param-name (:name (:param expr))
                inner-env (dissoc env param-name)
                b' (resolve-expr (:body expr) inner-env)]
            (if (identical? b' (:body expr))
              expr
              (->Mu (:param expr) b')))
    expr))

(defn pretty-print [expr]
  (condp instance? expr
    Var   (:name expr)
    Lam   (str "λ" (:name (:param expr)) "." (pretty-print (:body expr)))
    Mu    (str "μ" (:name (:param expr)) "." (pretty-print (:body expr)))
    Cont  (str "[" (pretty-print (:head expr)) "] " (pretty-print (:arg expr)))
    Appl  (let [head-str (pretty-print (:head expr))
                arg-str (pretty-print (:arg expr))]
            (if (instance? Var (:arg expr))
              (str head-str " " arg-str)
              (str head-str " (" arg-str ")")))
    (str expr)))

(defn show-env [env]
  (if (empty? env)
    (println "Empty environment")
    (doseq [[name expr] env]
      (println (str name " = " (pretty-print expr))))))

(defn handle-let [line env]
  (let [[_ rest] (str/split line #"let ")
        [name expr-str] (map str/trim (str/split rest #"=" 2))]
    (try
      (let [expr (parse-expr expr-str)
            resolved (resolve-expr expr env)]
        (println (str "let " name " = " (pretty-print resolved)))
        (assoc env name resolved))
      (catch Exception e
        (println "Error:" (.getMessage e))
        env))))

(declare handle-line)

(defn handle-load [filename env]
  (try
    (let [content (slurp filename)
          lines (str/split-lines content)]
      (reduce (fn [current-env line]
                (let [trimmed-line (str/trim line)]
                  (if (empty? trimmed-line)
                    current-env
                    (handle-line trimmed-line current-env))))
              env
              lines))
    (catch Exception e
      (println "Error loading file:" (.getMessage e))
      env)))

(defn handle-expression [line env]
  (try
    (let [parsed (parse-expr line)
          resolved (resolve-expr parsed env)
          result (eval-expr resolved)]
      (println (pretty-print result)))
    (catch Exception e
      (println "Error:" (.getMessage e))))
  env)

(defn handle-save [filename env]
  (try
    (let [env-lines (map (fn [[name expr]]
                           (str "let " name " = " (pretty-print expr)))
                         env)
          content (str/join "\n" env-lines)]
      (spit filename content)
      (println "Environment saved to" filename)
      env)
    (catch Exception e
      (println "Error saving file:" (.getMessage e))
      env)))

(defn handle-clear [env]
  (println "Environment cleared.")
  {})

(defn handle-delete [symbol-name env]
  (if (contains? env symbol-name)
    (do
      (println "Deleted:" symbol-name)
      (dissoc env symbol-name))
    (do
      (println "Symbol not found:" symbol-name)
      env)))

(defn handle-help [env]
  (println "λμ-Calculus REPL Commands:")
  (println "  :help                 - Show this help message")
  (println "  :q                    - Quit the REPL")
  (println "  :env                  - Show current environment")
  (println "  :clear                - Clear all definitions")
  (println "  :delete <name>        - Delete a specific definition")
  (println "  :load <file>          - Load definitions from file")
  (println "  :save <file>          - Save environment to file")
  (println "  let <name> = <expr>   - Define a variable")
  (println "  <expr>                - Evaluate an expression")
  (println)
  (println "Lambda-mu syntax:")
  (println "  λx.M  or  \\x.M       - Lambda abstraction")
  (println "  μα.M  or  #α.M        - Mu abstraction")
  (println "  [α]M                  - Continuation")
  (println "  M N                   - Application")
  env)

(defn parse-command [line]
  "Parse a command line into [command args]. Returns nil if not a command."
  (when (str/starts-with? line ":")
    (let [parts (str/split line #"\s+" 2)
          cmd (first parts)
          args (str/trim (or (second parts) ""))]
      [cmd args])))

(defn handle-line [line env]
  (let [line (str/trim line)]
    (if-let [[cmd args] (parse-command line)]
      (case cmd
        ":q" (do (println "Goodbye!") (System/exit 0))
        ":help" (handle-help env)
        ":clear" (handle-clear env)
        ":env" (do (show-env env) env)
        ":load" (if (empty? args)
                  (do (println "Usage: :load <filename>") env)
                  (handle-load args env))
        ":save" (if (empty? args)
                  (do (println "Usage: :save <filename>") env)
                  (handle-save args env))
        ":delete" (if (empty? args)
                    (do (println "Usage: :delete <name>") env)
                    (handle-delete args env))
        ;; default case for unknown commands
        (do (println "Unknown command:" cmd "Type :help for available commands") env))
      (cond
        (str/starts-with? line "let ") (handle-let line env)
        :else (handle-expression line env)))))

(defn -main []
  (println "λμ-Calculus REPL")
  (println "Commands: :q (quit), :env (show), :clear (reset), :delete <name>, :load <file>, :save <file>")
  (loop [env {}]
    (print "λμ> ") (flush)
    (let [input (read-line)]
      (if input
        (recur (handle-line input env))
        (do (println "Goodbye!") (System/exit 0))))))
