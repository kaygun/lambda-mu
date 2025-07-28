(ns lambda_mu.parser
  (:require [instaparse.core :as insta]
            [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]]
            [clojure.test :refer :all]))

;; === Lambda-Mu Parser using Instaparse ===
(def parser
  (insta/parser 
     "expr = appl
     appl = term (term)*
     term = cont | atom
     atom = lam | mu | var | <'('> expr <')'>
     lam = <('\\\\' | 'λ')> var <'.'> expr
     mu  = <('#' | 'μ')> var <'.'> expr
     cont = <'['> var <']'> atom
     var = #'[a-zA-Z_][a-zA-Z0-9_]*'
     ws = #'\\s+'
     "
    :auto-whitespace :standard))

(ns lambda_mu.parser
  (:require [instaparse.core :as insta]
            [lambda_mu.types :refer [->Var ->Lam ->Appl ->Cont ->Mu]]
            [clojure.test :refer :all]))

;; === Lambda-Mu Parser using Instaparse ===
(def parser
  (insta/parser 
     "expr = appl
     appl = term (term)*
     term = cont | atom
     atom = lam | mu | var | <'('> expr <')'>
     lam = <('\\\\' | 'λ')> var <'.'> expr
     mu  = <('#' | 'μ')> var <'.'> expr
     cont = <'['> var <']'> atom
     var = #'[a-zA-Z_][a-zA-Z0-9_]*'
     ws = #'\\s+'
     "
    :auto-whitespace :standard))

(defn to-ast [tree]
  (insta/transform
   {:var  ->Var
    :lam  ->Lam
    :mu   ->Mu
    :cont ->Cont
    :atom identity
    :term identity
    :appl #(if (= (count %&) 1) 
             (first %&) 
             (reduce ->Appl (first %&) (rest %&)))
    :expr identity}
   tree))

(defn parse-expr [s]
  (let [result (parser s)]
    (if (insta/failure? result)
      (throw (ex-info "Parse error" {:input s :error (with-out-str (println result))}))
      (to-ast result))))
