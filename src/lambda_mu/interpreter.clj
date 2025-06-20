(ns lambda-mu.interpreter
  (:require [clojure.core.match :refer [match]]
            [lambda-mu.parser :refer [parse]]))


(defn resolve-vars [env expr]
  (match expr
    {:type :var :name x}
    (get env x expr)

    {:type :lam :param x :body b}
    {:type :lam :param x :body (resolve-vars env b)}

    {:type :app :fun f :arg a}
    {:type :app :fun (resolve-vars env f) :arg (resolve-vars env a)}

    {:type :mu :alpha a :expr e}
    {:type :mu :alpha a :expr (resolve-vars env e)}

    {:type :freeze :alpha a :expr e}
    {:type :freeze :alpha a :expr (resolve-vars env e)}

    :else expr))

(defn subst [x v]
  (fn [e]
    (match e
      {:type :var :name y}
      (if (= e x) v e)
      
      {:type :lam :param a :body b}
      (if (= a x)
        e
        {:type :lam :param a :body ((subst x v) b)})
      
      {:type :app :fun f :arg a}
      {:type :app :fun ((subst x v) f) :arg ((subst x v) a)}
      
      {:type :freeze :alpha a :expr t}
      {:type :freeze :alpha a :expr ((subst x v) t)}
      
      {:type :mu :alpha a :expr u}
      {:type :mu :alpha a :expr ((subst x v) u)})))


(defn rename [from to]
  (fn [e]
    (match e
      {:type :lam :param x :body b}
      {:type :lam :param x :body ((rename from to) b)}
      
      {:type :app :fun f :arg a}
      {:type :app :fun ((rename from to) f) :arg ((rename from to) a)}
      
      {:type :freeze :alpha a :expr t}
      {:type :freeze :alpha (if (= a from) to a) :expr ((rename from to) t)}
      
      {:type :mu :alpha a :expr u}
      (if (= a from)
        e
        {:type :mu :alpha a :expr ((rename from to) u)})
      
      {:type :var :name _} e)))

(defn apply-named [beta v]
  (fn [e]
    (match e
      {:type :lam :param x :body b}
      {:type :lam :param x :body ((apply-named beta v) b)}
      
      {:type :app :fun f :arg a}
      {:type :app :fun ((apply-named beta v) f) :arg ((apply-named beta v) a)}
      
      {:type :freeze :alpha a :expr t}
      {:type :freeze
       :alpha a
       :expr (if (= a beta)
               {:type :app :fun ((apply-named beta v) t) :arg v}
               ((apply-named beta v) t))}
      
      {:type :mu :alpha a :expr u}
      (if (= a beta)
        e
        {:type :mu :alpha a :expr ((apply-named beta v) u)})
      
      {:type :var :name _} e)))



(defn normalize [expr]
  (letfn [(reduce-once [e]
            (match e
              ;; Reduction rules
              {:type :freeze :alpha a1 :expr {:type :freeze :alpha a2 :expr e2}}
              (if (= a1 a2)
                {:type :freeze :alpha a1 :expr e2}
                e)
              
              {:type :app :fun {:type :lam :param x :body u} :arg v}
              ((subst x v) u)
              
              {:type :app :fun {:type :mu :alpha b :expr u} :arg v}
              {:type :mu :alpha b :expr ((apply-named b v) u)}
              
              {:type :freeze :alpha a :expr {:type :mu :alpha b :expr u}}
              ((rename b a) u)
              
              {:type :mu :alpha a :expr {:type :freeze :alpha b :expr u}}
              (if (= a b) u e)
              
              ;; Recursive normalization
              {:type :lam :param x :body b}
              {:type :lam :param x :body (normalize b)}
              
              {:type :app :fun f :arg a}
              (let [f' (normalize f)
                    a' (normalize a)]
                {:type :app :fun f' :arg a'})
              
              {:type :freeze :alpha a :expr t}
              {:type :freeze :alpha a :expr (normalize t)}
              
              {:type :mu :alpha a :expr u}
              {:type :mu :alpha a :expr (normalize u)}
              
              {:type :var :name _} e))]
    (loop [e expr]
      (let [e' (reduce-once e)]
        (if (= e e')
          e
          (recur e'))))))

(defn eval-expr [env expr]
  (match expr
         {:type :let :name name :expr value}
         [(assoc env name (->> value (resolve-vars env) normalize)) nil]
  
         _
         [env (->> expr (resolve-vars env) normalize)]))
