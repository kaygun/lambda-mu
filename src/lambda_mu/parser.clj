(ns lambda_mu.parser
  (:require [instaparse.core :as insta]
            [lambda_mu.expr :refer [->Var ->Lam ->App ->Freeze ->Mu]]
            [clojure.test :refer :all]))

(def parser
  (insta/parser
   "expr    = app
    app     = term term*
    term    = lam | mu | freeze | var | parens
    lam     = <('\\\\' | 'λ')> identifier <'.'> expr
    mu      = <('mu' | 'μ')> identifier <'.'> expr
    freeze  = <'['> identifier <']'> expr
    parens  = <'('> expr <')'>
    var     = identifier
    identifier = #'[a-zA-Z_][a-zA-Z0-9_]*'
   "
   :auto-whitespace :standard))

(defn fold-app [head & tail]
  (reduce (fn [f x] (->App f x)) head tail))

(def transform
  {:expr identity
   :term identity
   :identifier identity
   :app (fn [& args] (apply fold-app args))
   :lam (fn [param body] (->Lam param body))
   :mu (fn [param body] (->Mu param body))
   :freeze (fn [alpha body] (->Freeze alpha body))
   :parens (fn [body] body)
   :var ->Var})

(defn parse [input]
  (let [tree (parser input)]
    (if (insta/failure? tree)
      (throw (ex-info "Parse error" {:input input :error tree}))
      (insta/transform transform tree))))

