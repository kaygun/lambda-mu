(ns lambda_mu.types)

;; === Typedefs ===
(defrecord Var [name])
(defrecord Lam [param body])
(defrecord Mu  [param body])
(defrecord Appl [head arg])
(defrecord Cont [head arg])
