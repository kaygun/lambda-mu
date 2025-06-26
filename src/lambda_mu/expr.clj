(ns lambda_mu.expr)

(defrecord Var [name])
(defrecord Lam [param body])
(defrecord App [fun arg])
(defrecord Freeze [alpha expr])
(defrecord Mu [alpha expr])

