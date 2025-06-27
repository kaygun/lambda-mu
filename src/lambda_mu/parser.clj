(ns lambda_mu.parser
  (:require [instaparse.core :as insta]
            [lambda_mu.expr :refer [->Var ->Lam ->App ->Freeze ->Mu]]
            [clojure.test :refer :all]))

(def parser
  (insta/parser
   "expr    = term | app
    app     = term term+
    term    =  <'('> expr <')'> | lam | mu | freeze | var 
    freeze  = <'['> ident <']'> expr
    lam     = <('\\\\' | 'λ')> ident <'.'> expr
    mu      = <('mu' | 'μ')> ident <'.'> expr
    var     = ident
    ident   = #'[a-zA-Z_][a-zA-Z0-9_]*'
   "
   :auto-whitespace :standard))

(def transform
  {:expr   identity
   :term   identity
   :ident  identity
   :app    (fn [head & tail] (reduce (fn [f x] (->App f x)) head tail))
   :lam    (fn [param body] (->Lam param body))
   :mu     (fn [param body] (->Mu param body))
   :freeze (fn [alpha body] (->Freeze alpha body))
   :var    ->Var})

(defn parse [input]
  (let [tree (parser input)]
    (if (insta/failure? tree)
      (throw (ex-info "Parse error" {:input input :error tree}))
      (insta/transform transform tree))))

