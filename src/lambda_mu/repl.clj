(ns lambda-mu.repl
  (:require [clojure.core.match :refer [match]]
            [lambda-mu.parser :refer [parse]]
            [lambda-mu.interpreter :refer [eval-expr]])
  (:gen-class))

(defn pretty [expr]
  (match expr
    {:type :var, :name x} x
    {:type :lam, :param x, :body b} (str "λ" (pretty x) "." (pretty b))
    {:type :mu, :alpha a, :expr e} (str "μ" (pretty a) "." (pretty e))
    {:type :app, :fun f, :arg a} (str "(" (pretty f) " " (pretty a) ")")
    {:type :freeze, :alpha a, :expr e} (str "[" (pretty a) "] " (pretty e))))

(defn dump-env [env path]
  (with-open [w (clojure.java.io/writer path)]
    (doseq [[name expr] env]
      (.write w (str "let " name " = " (pretty expr) "\n")))))

(defn load-env [path]
  (with-open [r (clojure.java.io/reader path)]
    (reduce
     (fn [env line]
       (if (clojure.string/blank? line)
         env
         (let [[new-env _] (eval-expr env (parse line))]
           new-env)))
     {} ;; start from empty env
     (line-seq r))))

(defn handle-command [env line]
  (cond
    (clojure.string/starts-with? line ":q")
    (do (println "Goodbye.") :quit)

    (clojure.string/starts-with? line ":load ")
    (let [path (clojure.string/trim (subs line 6))]
      (try
        (let [e (load-env path)]
          (println (str "Loaded from " path))
          e)
        (catch Exception e
          (println "Load error:" (.getMessage e))
          env)))

    (clojure.string/starts-with? line ":save ")
    (let [path (clojure.string/trim (subs line 6))]
      (try
        (do (dump-env env path)
            (println (str "Saved to " path)))
        (catch Exception e
          (println "Save error:" (.getMessage e))))
      env)

    :else
    (try
      (let [[new-env nf] (eval-expr env (parse line))]
        (when nf (println (pretty nf)))
        new-env)
      (catch Exception e
        (println "Error:" (.getMessage e))
        env))))

(defn -main [& _]
  (println "λμ-Calculus REPL (Clojure). Type :q to quit.")
  (loop [env {}]
    (print "λμ> ") (flush)
    (if-let [line (read-line)]
      (let [res (handle-command env line)]
        (if (= res :quit)
          nil
          (recur res)))
      (println "Goodbye."))))
