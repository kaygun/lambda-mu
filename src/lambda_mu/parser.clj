(ns lambda_mu.parser
  (:require [instaparse.core :as insta]
            [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]]
            [clojure.test :refer :all]))

;; === Lambda-Mu Parser using Instaparse ===
(def parser
  (insta/parser 
    "expr = atom (atom)*
     atom = lam | mu | cont | var | <'('> expr <')'>
     lam = <('\\\\' | 'λ')> var <'.'> expr
     mu = <('#' | 'μ')> var <'.'> expr
     cont = <'['> var <']'> atom
     var = #'[a-zA-Z_][a-zA-Z0-9_]*'"
    :auto-whitespace :standard))

(defn to-ast [tree]
  (insta/transform
   {:var  ->Var
    :lam  ->Lam
    :mu   ->Mu
    :cont ->Cont
    :atom identity
    :expr #(reduce ->Appl %1 %&)}
   tree))

(defn parse-expr [s]
  (let [result (parser s)]
    (if (insta/failure? result)
      (throw (ex-info "Parse error" {:error (str result)}))
      (to-ast result))))
