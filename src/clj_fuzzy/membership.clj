(ns clj-fuzzy.membership
  "Fuzzy membership functions"
  (:require [clj-fuzzy.utils :as u]
            [clojure.math.numeric-tower :as math]))

(defn- alpha-cut
  [a b c d alpha]
  (let [phi1 (/ 1.0 (- b a))
        phi2 (/ 1.0 (- d c))
        f1 (* phi1 b)
        f2 (* phi2 c)
        n1 (- (+ f1 alpha) 1.0)
        n2 (- (+ f2 1.0) alpha)
        l (if (= a b) a (/ n1 phi1))
        r (if (= b c) b (/ n2 phi2))]
    [l r]))

(defprotocol IFuzzySet
  (-fuzzify [this coll])
  (-support [this])
  (-nucleus [this])
  (-alpha-cut [this alpha]))

(defn- replace-nan
  ([v] (replace-nan v 0.0))
  ([v x]
   (if (Double/isNaN v)
     x
     v)))

(defrecord Triangular [a b c]
  IFuzzySet
  (-fuzzify [this coll]
    (let [set-name (:name (meta this))]
      (map
       (fn [x]
         (let [s1 (/ (float (- x a)) (float (- b a)))
               s2 (/ (float (- c x)) (float (- c b)))]
           {:set-title  set-name
            :value      x
            :membership (replace-nan (max (min s1 s2) 0.0))}))
       coll)))

  (-support [_this] [a c])

  (-nucleus [_this] b)

  (-alpha-cut [this alpha]
    (alpha-cut a b b c alpha)))

(defrecord Trapezoidal [a b c d]
  IFuzzySet
  (-fuzzify [this coll]
    (let [set-name (:name (meta this))]
      (map
       (fn [x]
         (let [s1 (/ (float (- x a)) (float (- b a)))
               s2 (/ (float (- d x)) (float (- d c)))]
           {:set-title  set-name
            :value      x
            :membership (replace-nan (max (min s1 1.0 s2) 0.0))}))
       coll)))

  (-support [_this] [a d])

  (-nucleus [_this] [b c])

  (-alpha-cut [_this alpha]
    (alpha-cut a b c d alpha)))

(defrecord Gaussian [mean sd]
  IFuzzySet
  (-fuzzify [this coll]
    (letfn [(gf [x] {:set-title  (:name (meta this))
                     :value      x
                     :membership (u/ln (/ (- (math/expt (- x mean) 2)) (* 2 (math/expt sd 2))))})]
      (map gf coll))))

(defrecord GeneralizedBell [w s c]
  IFuzzySet
  (-fuzzify [_this coll]
    (letfn [(gbf [x] 
              (/ 1.0 
                 (+ 1.0 
                    (math/expt (math/abs (/ (- x c) w)) (* 2 s)))))]
      (map gbf coll))))

(defrecord Sigmoid [c w]
  IFuzzySet
  (-fuzzify [_this coll]
    (map
     (fn [x] (/ 1.0 (+ 1.0 (u/ln (* (* c -1) (- x w))))))
     coll)))

(defrecord PiFunction [left-foot left-ceiling right-foot right-ceiling]
  IFuzzySet
  (-fuzzify [_this coll]
    (map
     (fn [x]
       (cond
         (<= x left-foot)
         0.0

         (and (>= x left-foot) (<= x (/ (+ left-foot left-ceiling) 2)))
         (* 2.0 (math/expt (/ (- x left-foot) (- left-ceiling left-foot)) 2.0))
       
         (and (>= x (/ (+ left-foot left-ceiling) 2.0)) (<= x left-ceiling))
         (- 1.0 (* 2.0 (math/expt (/ (- x left-ceiling) (- left-ceiling left-foot)) 2.0)))

         (and (>= x left-ceiling) (<= x right-ceiling))
         1.0

         (and (>= x right-ceiling) (<= x (/ (+ right-ceiling right-foot) 2.0)))
         (- 1.0 (* 2.0 (math/expt (/ (- x right-ceiling) (- right-foot right-ceiling)) 2.0)))
       
         (and (>= x (/ (+ x right-foot) 2.0)) (<= x right-foot))
         (* 2.0 (math/expt (/ (- x right-foot) (- right-foot right-ceiling)) 2.0))

         (>= x right-foot)
         0.0))
     coll)))

(defrecord ZFunction [foot ceiling]
  IFuzzySet
  (-fuzzify [_this coll]
    (map
     (fn [x]
       (cond
         (<= x ceiling)                              1.0
         (and (>= x ceiling) (<= x (/ (+ ceiling foot) 2.0))) (- 1 (* 2.0 (math/expt (/ (- x ceiling) (- foot ceiling)) 2.0)))
         (and (>= x (/ (+ ceiling foot) 2.0)) (<= x foot)) (* 2.0 (math/expt (/ (- x foot) (- foot ceiling)) 2.0))
         (>= x foot) 0.0))
     coll)))

(defrecord SShaped [foot ceiling]
  IFuzzySet
  (-fuzzify [_this coll]
    (map
     (fn [x]
       (cond
         (<= x foot)
         0.0
       
         (and (>= x foot) (<= x (/ (+ foot ceiling) 2.0)))
         (* 2.0 (math/expt (/ (- x foot) (- ceiling foot)) 2.0))

         (and (>= x (/ (+ foot ceiling) 2.0)) (<= x ceiling))
         (- 1 (* 2.0 (math/expt (/ (- x ceiling) (- ceiling foot)) 2.0)))
       
         (>= x ceiling) 1.0))
     coll)))

(defn trapezoidal
  [name a b c d]
  (with-meta (Trapezoidal. a b c d) {:name name}))

(defn triangular
  [name a b c]
  (with-meta (Triangular. a b c) {:name name}))

(defn gaussian
  [name mean sd]
  (with-meta (Gaussian. mean sd) {:name name}))
