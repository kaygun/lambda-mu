(ns lambda_mu.evaluator
  (:require [lambda_mu.expr :refer [->Var ->Lam ->App ->Freeze ->Mu]]
            [clojure.set :as set])
  (:import [lambda_mu.expr Var Lam App Freeze Mu])
  (:import [clojure.lang Atom]))

(declare eval-steps reduce-all substitute rename app-substitute fresh-name capture-avoiding-subst resolve-expr)

(defonce env (atom {}))

(defn resolve-expr [expr]
  (cond
    (instance? Var expr) (let [{:keys [name]} expr]
                              (if-let [resolved (@env name)]
                                (resolve-expr resolved)
                                expr))
    (instance? Lam expr) (->Lam (:param expr) (resolve-expr (:body expr)))
    (instance? App expr) (->App (resolve-expr (:fun expr)) (resolve-expr (:arg expr)))
    (instance? Freeze expr) (->Freeze (:alpha expr) (resolve-expr (:expr expr)))
    (instance? Mu expr) (->Mu (:alpha expr) (resolve-expr (:expr expr)))
    :else expr))

(defn free-vars [expr]
  (cond
    (instance? Var expr) #{(:name expr)}
    (instance? Lam expr) (disj (free-vars (:body expr)) (:param expr))
    (instance? App expr) (clojure.set/union (free-vars (:fun expr)) (free-vars (:arg expr)))
    (instance? Freeze expr) (free-vars (:expr expr))
    (instance? Mu expr) (disj (free-vars (:expr expr)) (:alpha expr))
    :else #{}))

(defn fresh-name [base used]
  (let [candidates (map #(str base %) (cons "" (range)))]
    (first (remove used candidates))))

(defn capture-avoiding-subst [binder pred replace expr & {:keys [preserve-free] :or {preserve-free false}}]
  (letfn [(sub [expr used]
            (cond
              (instance? Var expr)
              (if (pred (:name expr)) replace expr)

              (instance? Lam expr)
              (let [{:keys [param body]} expr
                    used-vars (clojure.set/union used (if preserve-free #{} (free-vars replace)) (free-vars body))
                    param' (if (pred param) (fresh-name param used-vars) param)
                    body' (if (= param param') body (capture-avoiding-subst param #(= % param) (->Var param') body :preserve-free preserve-free))]
                (->Lam param' (sub body' (conj used param'))))

              (instance? App expr)
              (->App (sub (:fun expr) used) (sub (:arg expr) used))

              (instance? Freeze expr)
              (->Freeze (:alpha expr) (sub (:expr expr) used))

              (instance? Mu expr)
              (let [{:keys [alpha expr]} expr
                    used-vars (clojure.set/union used (if preserve-free #{} (free-vars replace)) (free-vars expr))
                    alpha' (if (pred alpha) (fresh-name alpha used-vars) alpha)
                    expr' (if (= alpha alpha') expr (rename alpha alpha' expr))]
                (->Mu alpha' (sub expr' (conj used alpha'))))

              :else expr))]
    (sub expr (free-vars expr))))

(defn subst [x e expr]
  (capture-avoiding-subst x #(= % x) e expr))

(defn app-subst [beta v expr]
  (letfn [(sub [expr used]
            (cond
              (instance? Freeze expr)
              (let [{:keys [alpha expr]} expr]
                (->Freeze alpha (if (= alpha beta) (->App (sub expr used) v) (sub expr used))))
              :else (capture-avoiding-subst beta #(= % beta) v expr)))]
    (sub expr (free-vars expr))))

(defn rename [beta alpha expr]
  (capture-avoiding-subst beta #(= % beta) (->Var alpha) expr :preserve-free true))

(defn reduce-step [expr]
  (cond
    ;; [a][a]e => [a]e
    (and (instance? Freeze expr)
         (let [inner (:expr expr)]
           (and (instance? Freeze inner)
                (= (:alpha expr) (:alpha inner)))))
    [(->Freeze (:alpha expr) (:expr (:expr expr))) true]

    ;; (λx.u) v => subst
    (and (instance? App expr)
         (instance? Lam (:fun expr)))
    [(subst (:param (:fun expr)) (:arg expr) (:body (:fun expr))) true]

    ;; (μb.u) v => μb.(app-subst b v u)
    (and (instance? App expr)
         (instance? Mu (:fun expr)))
    [(->Mu (:alpha (:fun expr)) (app-subst (:alpha (:fun expr)) (:arg expr) (:expr (:fun expr)))) true]

    ;; [a](μb.u) => rename b -> a in u (capture-avoiding)
    (and (instance? Freeze expr)
         (instance? Mu (:expr expr)))
    [(rename (:alpha (:expr expr)) (:alpha expr) (:expr (:expr expr))) true]

    ;; μa.[a]e => e
    (and (instance? Mu expr)
         (instance? Freeze (:expr expr))
         (= (:alpha expr) (:alpha (:expr expr))))
    [(:expr (:expr expr)) true]

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

(defn reduce-all [expr]
  (loop [e expr]
    (let [[e2 changed] (reduce-step e)]
      (if changed (recur e2) e2))))

(defn eval-steps [expr]
  (loop [e expr acc [expr]]
    (let [[e2 changed] (reduce-step e)]
      (if changed
        (recur e2 (conj acc e2))
        acc))))


