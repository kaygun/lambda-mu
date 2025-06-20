(ns lambda-mu.parser (:require [instaparse.core :as insta]
                               [clojure.core.match :refer [match]]))

(def parser
  (insta/parser
   "top    = let | expr
    let    = <'let'> <ws+> var <ws+> <'='> <ws+> expr
    expr   = app
    app    = app <ws*> term | term
    term   = freeze | lam | mu | var | <'('> expr <')'>
    lam    = <'λ'> var <ws+> expr
    mu     = <'μ'> var <ws+> expr
    freeze = <'['> var <']'> <ws*> expr
    var    = #'[a-zA-Z0-9_]+'
    ws     = #'[ \\s\\.]'"))

(defn make-expr
  ([type arg1] (make-expr type arg1 nil))
  ([type arg1 arg2]
   (case type
     :top      {:type type :expr arg1}
     :var      {:type type :name arg1}
     :lam      {:type type :param arg1 :body arg2}
     :app      {:type type :fun arg1 :arg arg2}
     :freeze   {:type type :alpha arg1 :expr arg2}
     :mu       {:type type :alpha arg1 :expr arg2}
     :let      {:type type :name arg1 :expr arg2})))

(defn to-ast [tree]
  (match tree
    [:top e] (to-ast e)
    [:let [:var name] expr] {:type :let :name name :expr (to-ast expr)}
    [:expr e] (to-ast e)
    [:term e] (to-ast e)
    [:app t & ts] (reduce (fn [a b] (make-expr :app a b)) (to-ast t) (map to-ast ts))
    [:lam [:var x] e] (make-expr :lam (make-expr :var x) (to-ast e))
    [:mu [:var a] e] (make-expr :mu (make-expr :var a) (to-ast e))
    [:freeze [:var a] e] (make-expr :freeze (make-expr :var a) (to-ast e))
    [:var name] (make-expr :var name)
    :else (throw (ex-info "Malformed parse tree" {:tree tree}))))

(defn parse [s]
  (let [tree (parser s)]
    (if (insta/failure? tree)
      (throw (ex-info "Parse error" {:failure tree}))
      (to-ast tree))))
