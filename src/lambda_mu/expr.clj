(ns lambda_mu.expr)

(defrecord Var [name])
(defrecord App [fun arg])
(defrecord Lam [param body])
(defrecord Freeze [alpha expr])
(defrecord Mu [alpha expr])

