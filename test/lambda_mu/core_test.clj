(ns lambda-mu.core-test
  (:require [clojure.test :refer :all]
            [lambda_mu.parser :refer [parse-expr]]
            [lambda_mu.evaluator :refer [alpha-equal? eval-expr free-vars substitute]]
            [lambda_mu.repl :refer [pretty-print resolve-expr]]
            [lambda_mu.types :refer [->Var ->Lam ->Mu ->Appl ->Cont]]))

(deftest test-parsing
  (is (= (->Var "x") (parse-expr "x")))
  (is (= (->Lam (->Var "x") (->Var "x")) (parse-expr "λx.x")))
  (is (= (->Mu (->Var "k") (->Var "k")) (parse-expr "μk.k")))
  (is (= (->Appl (->Var "f") (->Var "x")) (parse-expr "f x")))
  (is (= (->Cont (->Var "k") (->Var "x")) (parse-expr "[k] x"))))

(deftest test-pretty-print
  (is (= "x" (pretty-print (->Var "x"))))
  (is (= "λx.x" (pretty-print (->Lam (->Var "x") (->Var "x")))))
  (is (= "μk.k" (pretty-print (->Mu (->Var "k") (->Var "k")))))
  (is (= "f x" (pretty-print (->Appl (->Var "f") (->Var "x")))))
  (is (= "[k] x" (pretty-print (->Cont (->Var "k") (->Var "x"))))))

(deftest test-alpha-equal
  (is (alpha-equal? (parse-expr "λx.x") (parse-expr "λy.y")))
  (is (not (alpha-equal? (parse-expr "λx.x") (parse-expr "λx.y"))))
  (is (alpha-equal? (parse-expr "μk.[k] x") (parse-expr "μz.[z] x"))))

(deftest test-substitute
  (let [x (->Var "x")
        y (->Var "y")
        f (->Lam x x)]
    (is (alpha-equal? y (substitute x y x)))
    (is (alpha-equal? f (substitute y x f)))
    (is (alpha-equal? (->Lam y x) (substitute x y (->Lam y x))))))

(deftest test-eval-simple
  (is (alpha-equal? (eval-expr (parse-expr "(λx.x) y")) (parse-expr "y")))
  (is (alpha-equal? (eval-expr (parse-expr "μk.[k] y")) (parse-expr "y")))
  (is (alpha-equal? (eval-expr (parse-expr "(μk.[k] x) y")) (parse-expr "x y"))))

(deftest test-free-vars
  (is (= #{(->Var "x") (->Var "y")} (free-vars (parse-expr "x y"))))
  (is (= #{} (free-vars (parse-expr "λx.x"))))
  (is (= #{(->Var "z")} (free-vars (parse-expr "λx.z")))))

(deftest test-resolve-env
  (let [env {"id" (parse-expr "λx.x")
             "val" (parse-expr "z")}
        expr (parse-expr "id val")]
    (is (alpha-equal? (eval-expr (resolve-expr expr env)) (parse-expr "z")))))
