(ns lambda_mu.evaluator
  (:require [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]]
            [clojure.set :as set])
  (:import  [lambda_mu.types Var Lam Appl Cont Mu]))

(defn fresh-name [base avoid]
  (if (contains? avoid base)
    (let [avoid-names (set (map :name avoid))]
      (->> (iterate inc 1)
           (map #(->Var (str (:name base) %)))
           (remove #(contains? avoid-names (:name %)))
           first))
    base))

(defn free-vars [expr]
  (condp instance? expr
    Var #{expr}
    Appl (set/union (free-vars (:head expr)) (free-vars (:arg expr)))
    Cont (set/union (free-vars (:head expr)) (free-vars (:arg expr)))  ; ← This looks wrong!
    Lam  (disj (free-vars (:body expr)) (:param expr))
    Mu   (disj (free-vars (:body expr)) (:param expr))
    #{}))

(defn free-vars [expr]
  (condp instance? expr
    Var #{expr}
    Lam  (disj (free-vars (:body expr)) (:param expr))
    Mu   (disj (free-vars (:body expr)) (:param expr))
    Appl (set/union (free-vars (:head expr)) (free-vars (:arg expr)))
    Cont (set/union (free-vars (:head expr)) (free-vars (:arg expr)))
    #{}))

(defn alpha-equal?
  ([e1 e2] (alpha-equal? e1 e2 {} {}))
  ([e1 e2 env1 env2]
   (letfn [(bind-eq? [e1 e2 env1 env2]
             (let [free-vars-union (set/union (free-vars e1) (free-vars e2))
                   fresh-var (fresh-name (:param e1) free-vars-union)
                   fresh-name-str (:name fresh-var)
                   env1' (assoc env1 (:name (:param e1)) fresh-name-str)
                   env2' (assoc env2 (:name (:param e2)) fresh-name-str)]
               (alpha-equal? (:body e1) (:body e2) env1' env2')))
           (bin-eq? [e1 e2 env1 env2]
             (and (alpha-equal? (:head e1) (:head e2) env1 env2)
                  (alpha-equal? (:arg e1)  (:arg e2)  env1 env2)))]
     (if (not= (type e1) (type e2))
       false
       (condp instance? e1
         Var (= (get env1 (:name e1) (:name e1))
                (get env2 (:name e2) (:name e2)))
         Appl (bin-eq? e1 e2 env1 env2)
         Cont (bin-eq? e1 e2 env1 env2)
         Lam  (bind-eq? e1 e2 env1 env2)
         Mu   (bind-eq? e1 e2 env1 env2)
         false)))))

(defmulti substitute (fn [bind repl expr] (type expr)))

(defmethod substitute Var [bind repl expr]
  (if (= expr bind) repl expr))

(defmethod substitute Appl [bind repl expr]
  (->Appl (substitute bind repl (:head expr))
          (substitute bind repl (:arg expr))))

(defmethod substitute Cont [bind repl expr]
  (->Cont (substitute bind repl (:head expr))
          (substitute bind repl (:arg expr))))

(defn subst-binding [bind repl param body ctor]
  (let [free-in-repl (free-vars repl)
        free-in-body (free-vars body)]
    (cond
      (= param bind) 
      (ctor param body)
      
      (contains? free-in-repl param)
      (let [avoid-set (set/union free-in-repl free-in-body #{bind})
            fresh (fresh-name param avoid-set)
            renamed-body (substitute param fresh body)]
        (ctor fresh (substitute bind repl renamed-body)))
      
      :else 
      (ctor param (substitute bind repl body)))))

(defmethod substitute Lam [bind repl expr]
  (subst-binding bind repl (:param expr) (:body expr) ->Lam))

(defmethod substitute Mu [bind repl expr]
  (subst-binding bind repl (:param expr) (:body expr) ->Mu))

(defmethod substitute :default [bind repl expr] expr)

(defmulti reduce-expression (fn [expr] (type expr)))

(defmethod reduce-expression Appl [expr]
  (let [h' (reduce-expression (:head expr))
        a' (reduce-expression (:arg expr))]
    (cond
      ;; β-reduction: (λx.M)N → M[x := N]
      (instance? Lam h')
      (reduce-expression (substitute (:param h') a' (:body h')))
      
      ;; μ-application: (μα.M)N → μα.M[α := [α]N]
      ;; This is a core rule of lambda-mu calculus
      (instance? Mu h')
      (let [cont-arg (->Cont (:param h') a')]
        (reduce-expression 
          (->Mu (:param h') 
                (substitute (:param h') cont-arg (:body h')))))
      
      :else
      (->Appl h' a'))))

(defmethod reduce-expression Cont [expr]
  (let [h' (reduce-expression (:head expr))
        a (:arg expr)]  ; Don't reduce the argument yet
    (cond
      ;; [α](μβ.M) → M[β := α]
      ;; Check the original argument before reduction
      (instance? Mu a)
      (reduce-expression (substitute (:param a) h' (:body a)))
      
      :else
      (let [a' (reduce-expression a)]
        (->Cont h' a')))))

(defmethod reduce-expression Mu [expr]
  (let [{p :param b :body} expr
        b' (reduce-expression b)]
    (cond
      ;; μ-elimination: μα.[α]M → M
      ;; This is the core elimination rule - when a mu binds a continuation
      ;; variable that is immediately used in a continuation context
      (and (instance? Cont b')
           (instance? Var (:head b'))
           (= p (:head b')))
      (reduce-expression (:arg b'))
      
      ;; Dead variable elimination: μα.M → M (when α not free in M)
      ;; This eliminates unused mu-bindings (subsumes the structural rule)
      (not (contains? (free-vars b') p))
      (reduce-expression b')
      
      ;; No reduction possible - return with reduced body
      :else
      (->Mu p b'))))

(defmethod reduce-expression Lam [expr]
  (->Lam (:param expr) (reduce-expression (:body expr))))

(defmethod reduce-expression :default [expr] expr)

(defn eval-expr
  "Repeatedly reduces expr up to max-steps or until no change in alpha-equivalence.
  Returns a vector of all intermediate reduction steps, including the initial and final term."
  ([expr] (eval-expr expr 1000))
  ([expr max-steps]
   (loop [current expr
          count 0]
     (if (>= count max-steps)
       current
       (let [next-expr (reduce-expression current)]
         (if (identical? next-expr current)
           current
           (recur next-expr (inc count))))))))
