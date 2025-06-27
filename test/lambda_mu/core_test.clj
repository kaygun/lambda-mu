(ns lambda_mu.core_test
  (:require [clojure.test :refer :all]
            [lambda_mu.parser :refer [parse]]
            [lambda_mu.evaluator :as eval]
            [lambda_mu.expr :refer :all]))

(deftest parser-tests
  (testing "Parser correctness"
    (is (= (->Var "x") (parse "x")))
    (is (= (->Lam "x" (->Var "x")) (parse "λx.x")))
    (is (= (->Mu "a" (->Var "a")) (parse "μa.a")))
    (is (= (->Freeze "a" (->Var "x")) (parse "[a]x")))
    (is (= (->App (->Var "f") (->Var "x")) (parse "f x")))
    (is (= (->App (->App (->Var "f") (->Var "x")) (->Var "y")) (parse "f x y")))
    (is (thrown? Exception (parse "λ.")))))

(deftest free-vars-tests
  (testing "Free variable detection"
    (is (= #{"x"} (eval/free-vars (->Var "x"))))
    (is (= #{} (eval/free-vars (->Lam "x" (->Var "x")))))
    (is (= #{"y"} (eval/free-vars (->Lam "x" (->Var "y")))))
    (is (= #{"x" "f"} (eval/free-vars (->App (->Var "f") (->Var "x")))))
    (is (= #{"x"} (eval/free-vars (->Freeze "a" (->Var "x")))))
    (is (= #{"x"} (eval/free-vars (->Mu "a" (->App (->Var "a") (->Var "x"))))))))

(deftest resolve-expr-tests
  (testing "Environment resolution"
    (reset! eval/env {"x" (->Var "y") "y" (->Var "z")})
    (is (= (->Var "z") (eval/resolve-expr (->Var "x"))))))

(deftest reduce-step-tests
  (testing "One-step reductions"
    (let [id (->Lam "x" (->Var "x"))
          expr (->App id (->Var "y"))]
      (is (= [(->Var "y") true] (eval/reduce-step expr))))))

(deftest reduce-all-tests
  (testing "Full reduction sequence"
    (let [expr (parse "(λx.x) y")]
      (is (= (->Var "y") (first (eval/reduce-step (eval/resolve-expr expr))))))))

(deftest subst-tests
  (testing "Capture-avoiding substitution"
    (let [expr (->Lam "x" (->App (->Var "x") (->Var "y")))
          sub  (eval/subst "y" (->Var "z") expr)]
      (is (= (->Lam "x" (->App (->Var "x") (->Var "z"))) sub)))))

(run-tests)

