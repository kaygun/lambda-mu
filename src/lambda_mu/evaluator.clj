(ns lambda_mu.evaluator
  (:require [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]]
            [lambda_mu.parser :refer [parse-expr]]
            [clojure.set :as set])
  (:import [lambda_mu.types Var Lam Appl Cont Mu])
  (:import [clojure.lang Atom]))

;; Fresh variable generation
(defn fresh-name [base avoid]
  (if (avoid base)
    (->> (iterate inc 1)
         (map #(->Var (str (:name base) %)))
         (remove avoid)
         first)
    base))

;; Alpha equivalence?
(defn alpha-equal?
  ([e1 e2] (alpha-equal? e1 e2 {}))
  ([e1 e2 env]
   (cond
     (and (instance? Var e1) (instance? Var e2))
     (= (env (:name e1) (:name e1)) (env (:name e2) (:name e2)))
     
     (or (and (instance? Appl e1) (instance? Appl e2))
         (and (instance? Cont e1) (instance? Cont e2)))
     (and (alpha-equal? (:head e1) (:head e2) env)
          (alpha-equal? (:arg e1) (:arg e2) env))
     
     (or (and (instance? Lam e1) (instance? Lam e2))
         (and (instance? Mu e1) (instance? Mu e2)))
     (let [fresh-var (gensym "var")
           env' (assoc env 
                       (:name (:param e1)) fresh-var
                       (:name (:param e2)) fresh-var)]
       (alpha-equal? (:body e1) (:body e2) env'))
     
     :else false)))

;; === Free Vars ===
(defn free-vars [expr]
  (cond
    (instance? Var expr) #{expr}

    (or (instance? Appl expr) (instance? Cont expr))
    (clojure.set/union (free-vars (:head expr)) (free-vars (:arg expr)))

    (or (instance? Lam expr) (instance? Mu expr))
    (disj (free-vars (:body expr)) (:param expr))

    :else #{}))

;; Substitute
(defn substitute [bind repl expr]
  (let [free-in-repl (free-vars repl)]
    (letfn [(subst-binary [head arg ctor]
              (ctor (substitute bind repl head)
                    (substitute bind repl arg)))
            (subst-binding [param body ctor]
              (cond
                (= param bind) expr
                (contains? free-in-repl param)
                (let [fresh (fresh-name param free-in-repl)]
                  (ctor fresh (substitute bind repl (substitute param fresh body))))
                :else (ctor param (substitute bind repl body))))]
      (condp instance? expr
        Var  (if (= expr bind) repl expr)
        Appl (subst-binary (:head expr) (:arg expr) ->Appl)
        Cont (subst-binary (:head expr) (:arg expr) ->Cont)
        Lam  (subst-binding (:param expr) (:body expr) ->Lam)
        Mu   (subst-binding (:param expr) (:body expr) ->Mu)
        :else expr))))

(defn reduce-binary-head [f a]
  (cond
    ;; Mu elimination via Var
    (and (instance? Var f) (instance? Mu a) (= (:name f) (:name (:param a))))
    (:body a)

    ;; Cont passthrough via Var
    (and (instance? Var f) (instance? Cont a) (= (:name f) (:name (:head a))))
    a
    ;; Beta reduction
    (instance? Lam f)
    (substitute (:param f) a (:body f))

    ;; Mu substitution
    (instance? Mu f)
    (letfn [(subst [m]
              (cond
                (and (instance? Cont m)
                     (= (:name (:head m)) (:name (:param f))))
                (->Appl (subst (:arg m)) a)

                (instance? Cont m) (->Cont (subst (:head m)) (subst (:arg m)))
                (instance? Appl m) (->Appl (subst (:head m)) (subst (:arg m)))
                (instance? Lam  m) (->Lam (:param m) (subst (:body m)))
                (instance? Mu   m) (->Mu  (:param m) (subst (:body m)))
                :else m))]
      (subst (:body f)))

    :else nil))

;; CONSOLIDATED main reduction function
(defn reduce-expr [expr]
  (letfn [(reduce-binary-expr [expr ctor]
            (let [{:keys [head arg]} expr]
              ;; 1. Try to reduce the head
              (if-let [r (reduce-binary-head head arg)]
                (reduce-expr r)
                ;; 2. If not, reduce children and check for changes
                (let [head' (reduce-expr head)
                      arg'  (reduce-expr arg)]
                  (if (or (not (alpha-equal? head' head))
                          (not (alpha-equal? arg' arg)))
                    (reduce-expr (ctor head' arg'))
                    expr)))))
          (reduce-binding-expr [expr ctor]
            (let [{:keys [param body]} expr
                  body' (reduce-expr body)]
              (if (not (alpha-equal? body' body))
                (ctor param body')
                expr)))]
  (cond
    ;; Special case reduction for Mu
    (and (instance? Mu expr)
         (instance? Cont (:body expr))
         (= (:name (:param expr)) (:name (:head (:body expr)))))
    (reduce-expr (:arg (:body expr)))
    ;; Dispatch to helpers for general cases
    (instance? Appl expr) (reduce-binary-expr  expr ->Appl)
    (instance? Cont expr) (reduce-binary-expr  expr ->Cont)
    (instance? Mu expr)   (reduce-binding-expr expr ->Mu)
    (instance? Lam expr)  (reduce-binding-expr expr ->Lam)
    ;; Default case
    :else expr)))

;; CORRECTED eval-expr with better variable name
(defn eval-expr
  "Repeatedly reduces expr up to max-steps or until no change in alpha-equivalence."
  ([expr] (eval-expr expr 1000))
  ([expr max-steps]
   (loop [current expr
          count 0]
     (let [next-expr (reduce-expr current)] ; Renamed 'next' to 'next-expr'
       (if (or (>= count max-steps) (alpha-equal? next-expr current))
         current
         (recur next-expr (inc count)))))))
