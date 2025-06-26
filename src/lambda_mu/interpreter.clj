(ns lambda-mu.interpreter
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [lambda-mu.parser :refer [parse]]))

(defn free-vars [expr]
  (match expr
    {:type :var :name x} #{x}
    {:type :lam :param {:type :var :name x} :body b} (disj (free-vars b) x)
    {:type :mu :alpha {:type :var :name a} :expr e} (disj (free-vars e) a)
    {:type :freeze :alpha {:type :var :name a} :expr e} (disj (free-vars e) a)
    {:type :app :fun f :arg a} (set/union (free-vars f) (free-vars a))
    :else #{}))

(defn gensym-var [x]
  {:type :var :name (gensym (str (:name x) "-"))})

(defn rename [from to]
  (fn walk [e]
    (match e
      {:type :var :name x} (if (= x (:name from)) to e)
      {:type :lam :param x :body b}
      (let [x' (if (= x from) to x)]
        {:type :lam :param x' :body (walk b)})

      {:type :app :fun f :arg a}
      {:type :app :fun (walk f) :arg (walk a)}

      {:type :freeze :alpha a :expr t}
      (let [a' (if (= a from) to a)]
        {:type :freeze :alpha a' :expr (walk t)})

      {:type :mu :alpha a :expr u}
      (let [a' (if (= a from) to a)]
        {:type :mu :alpha a' :expr (walk u)})

      :else e)))

(defn subst [x v]
  (fn walk [e]
    (match e
      {:type :var :name y} (if (= e x) v e)

      {:type :lam :param a :body b}
      (if (= a x)
        e
        (let [fv-v (free-vars v)
              a-name (:name a)]
          (if (contains? fv-v a-name)
            (let [fresh (gensym-var a)
                  renamed-b ((rename a fresh) b)]
              {:type :lam :param fresh :body (walk renamed-b)})
            {:type :lam :param a :body (walk b)})))

      {:type :mu :alpha a :expr b}
      (if (= a x)
        e
        (let [fv-v (free-vars v)
              a-name (:name a)]
          (if (contains? fv-v a-name)
            (let [fresh (gensym-var a)
                  renamed-b ((rename a fresh) b)]
              {:type :mu :alpha fresh :expr (walk renamed-b)})
            {:type :mu :alpha a :expr (walk b)})))

      {:type :freeze :alpha a :expr b}
      (if (= a x)
        e
        (let [fv-v (free-vars v)
              a-name (:name a)]
          (if (contains? fv-v a-name)
            (let [fresh (gensym-var a)
                  renamed-b ((rename a fresh) b)]
              {:type :freeze :alpha fresh :expr (walk renamed-b)})
            {:type :freeze :alpha a :expr (walk b)})))

      {:type :app :fun f :arg a}
      {:type :app :fun (walk f) :arg (walk a)}

      :else e)))

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
