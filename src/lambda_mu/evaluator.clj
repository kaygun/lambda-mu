(ns lambda_mu.evaluator
  (:require [lambda_mu.expr :refer [->Var ->Lam ->App ->Freeze ->Mu]]
            [clojure.set :as set])
  (:import [lambda_mu.expr Var Lam App Freeze Mu])
  (:import [clojure.lang Atom]))

(declare eval-steps app-subst fresh-name subst resolve-expr)

(defonce env (atom {}))

(defn resolve-expr [expr]
  (cond
    (instance? Var expr)    (or (resolve-expr (get @env (:name expr))) expr)
    (instance? App expr)    (->App    (resolve-expr (:fun expr)) (resolve-expr (:arg expr)))
    (instance? Freeze expr) (->Freeze (:alpha expr) (resolve-expr (:expr expr)))
    (instance? Lam expr)    (->Lam    (:param expr) (resolve-expr (:body expr)))
    (instance? Mu expr)     (->Mu     (:alpha expr) (resolve-expr (:expr expr)))
    :else expr))

(defn free-vars [expr]
  (cond
    (instance? Var expr)    #{(:name expr)}
    (instance? App expr)    (clojure.set/union (free-vars (:fun expr)) (free-vars (:arg expr)))
    (instance? Freeze expr) (free-vars (:expr expr))
    (instance? Lam expr)    (disj (free-vars (:body expr)) (:param expr))
    (instance? Mu expr)     (disj (free-vars (:expr expr)) (:alpha expr))
    :else #{}))

(defn fresh-name [base used]
  (let [candidates (map #(str base %) (cons "" (range)))]
    (first (remove used candidates))))

(defn subst
  [binder replace expr & {:keys [preserve-free on-freeze] :or {preserve-free false on-freeze identity}}]
  (letfn [(walk [expr used]
            (cond
              (instance? Var expr)
              (if (= binder (:name expr)) replace expr)

              (instance? App expr)
              (->App (walk (:fun expr) used) (walk (:arg expr) used))

              (instance? Lam expr)
              (let [{:keys [param body]} expr
                    used-vars (clojure.set/union used (if preserve-free #{} (free-vars replace)) (free-vars body))
                    param' (if (= binder param) (fresh-name param used-vars) param)
                    body' (if (= param param') body (subst param (->Var param') body :preserve-free preserve-free))]
                (->Lam param' (walk body' (conj used param'))))

              (instance? Mu expr)
              (let [{:keys [alpha expr] :as mu-expr} expr
                    mu-body expr
                    used-vars (clojure.set/union used (if preserve-free #{} (free-vars replace)) (free-vars mu-body))
                    alpha' (if (= binder alpha) (fresh-name alpha used-vars) alpha)
                    body' (if (= alpha alpha') mu-body
                              (subst alpha (->Var alpha') mu-body :preserve-free true))]
                (->Mu alpha' (walk body' (conj used alpha'))))
              
              ;; (instance? Mu expr)
              ;; (let [{:keys [alpha expr]} expr
              ;;       used-vars (clojure.set/union used (if preserve-free #{} (free-vars replace)) (free-vars expr))
              ;;       alpha' (if (= binder alpha) (fresh-name alpha used-vars) alpha)
              ;;       expr' (if (= alpha alpha') expr (subst alpha (->Var alpha') expr :preserve-free true))]
              ;;   (->Mu alpha' (walk expr' (conj used alpha'))))

              (instance? Freeze expr)
              (on-freeze expr #(walk % used))

              :else expr))]
    (walk expr (free-vars expr))))


(defn app-subst [beta v expr]
  (subst beta v expr
         :on-freeze
         (fn [{:keys [alpha expr]} recur-fn]
           (->Freeze alpha
                     (if (= alpha beta)
                       (->App (recur-fn expr) v)
                       (recur-fn expr))))))

(defn reduce-step [expr]
  (cond
    ;; [a][a]e => [a]e
    (and (instance? Freeze expr)
         (let [inner (:expr expr)]
           (and (instance? Freeze inner)
                (= (:alpha expr) (:alpha inner)))))
    [(->Freeze (:alpha expr) (:expr (:expr expr))) true]

    ;; [a](μb.u) => rename b -> a in u (capture-avoiding)
    (and (instance? Freeze expr)
         (instance? Mu (:expr expr)))
    [(subst (:alpha (:expr expr)) (->Var (:alpha expr)) (:expr (:expr expr)) :preserve-free true) true]

    ;; μa.[a]e => e
    (and (instance? Mu expr)
         (instance? Freeze (:expr expr))
         (= (:alpha expr) (:alpha (:expr expr))))
    [(:expr (:expr expr)) true]

    ;; (μb.u) v => μb.(app-subst b v u)
    (and (instance? App expr)
         (instance? Mu (:fun expr)))
    [(->Mu (:alpha (:fun expr)) (app-subst (:alpha (:fun expr)) (:arg expr) (:expr (:fun expr)))) true]
    
    ;; (λx.u) v => subst
    (and (instance? App expr)
         (instance? Lam (:fun expr)))
    [(subst (:param (:fun expr)) (:arg expr) (:body (:fun expr)) :preserve-free false) true]

    (instance? Lam expr)
    (let [[b changed] (reduce-step (:body expr))] [(->Lam (:param expr) b) changed])

    (instance? App expr)
    (let [[f cf] (reduce-step (:fun expr))
          [a ca] (reduce-step (:arg expr))]
      [(->App f a) (or cf ca)])

    (instance? Freeze expr)
    (let [[e changed] (reduce-step (:expr expr))] [(->Freeze (:alpha expr) e) changed])

    (instance? Mu expr)
    (let [[e changed] (reduce-step (:expr expr))] [(->Mu (:alpha expr) e) changed])

    :else [expr false]))

(defn eval-steps [expr]
  (loop [e expr acc [expr]]
    (let [[e2 changed] (reduce-step e)]
      (if changed
        (recur e2 (conj acc e2))
        acc))))


