(ns clj-fuzzy.membership
  "Fuzzy membership functions"
  (:require [clj-fuzzy.utils :as u]
            [clojure.math.numeric-tower :as math]))

(defn triangular
  "Triangular membership function.
  
  **params**

  * `xs`           independent variable, vector of numbers
  * `a`, `b`, `c`  define the shape of triangular function
  "
  [xs a b c]
  {:pre [(number? a)
         (number? b)
         (number? c)
         (<= a b c)]}
  (map
   (fn [x]
     (let [s1 (/ (float (- x a)) (float (- b a)))
           s2 (/ (float (- c x)) (float (- c b)))]
       (max (min s1 s2) 0.0)))
   xs))

(defn trapezoidal
  "Trapezoidal membership function.
  
  **params**
  
  * `xs`                independent variable, vector of numbers
  * `a`, `b`, `c`, `d`  define the shape or trapezoidal function"
  [xs a b c d]
  {:pre [(every? number? xs)
         (number? a)
         (number? b)
         (number? c)
         (number? d)
         (<= a b c d)]}
  (map
   (fn [x]
     (let [s1 (/ (float (- x a)) (float (- b a)))
           s2 (/ (float (- d x)) (float (- d c)))]
       (max (min s1 1.0 s2) 0.0)))
   xs))

(defn gaussian
  "Gaussian membership function.

  **params**

  * `xs`       independent variable, vector
  * `mean`     mean value
  * `sd`       standard deviation
  "
  [xs mean sd]
  {:pre [(every? number? xs)
         (number? mean)
         (and (number? sd) (pos? sd))]}
  (letfn [(gf [x] (u/ln (/ (- (math/expt (- x mean) 2)) (* 2 (math/expt sd 2)))))]
    (map gf xs)))

(defn gaussian-combined
  "Return Gaussian for two combined gaussians.
  
  **params**

  * `xs`      independent variable, vector of numbers
  * `mean1`   mean 1
  * `sd1`     standard deviation 1
  * `mean2`   mean 2
  * `sd2`     standard deviation 2
  "
  [xs mean1 sd1 mean2 sd2]
  {:pre [(number? mean1)
         (number? mean2)
         (>= mean1 mean2)]}
  (letfn [(decide [x a b]
            (cond
              (<= x mean1) a
              (> x mean2)  b
              :else        1.0))]
    (map
     (fn [x]
       (let [[a & _] (gaussian [x] mean1 sd1)
             [b & _] (gaussian [x] mean2 sd2)]
         (decide x a b)))
     xs)))

(defn generalized-bell
  "Generalized Bell membership function.

  **params**

  * `xs`      independent variable, vector of numbers
  * `w`       bell function width
  * `s`       bell function slope
  * `c`       bell function center"
  [xs w s c]
  {:pre [(and (number? w) (pos? w))]}
  (letfn [(gbf [x] 
            (/ 1.0 
               (+ 1.0 
                  (math/expt (math/abs (/ (- x c) w)) (* 2 s)))))]
    (map gbf xs)))

(defn sigmoid
  "Sigmoid membership function.

  **params**

  * `xs`      independent variable, vector of numbers
  * `c`       sigmoid center
  * `w`       sigmoid width"
  [xs c w]
  (map
   (fn [x] (/ 1.0 (+ 1.0 (u/ln (- (* w (- x w)))))))
   xs))

(defn s-shaped
  "S-shaped membership function.
  
  **params**
  
  * `xs`       independent variable, vector of numbers
  * `foot`     where the function begins to climb from zero.
  * `ceiling`  where the function levels off at 1.
 "
  [xs foot ceiling]
  {:pre [(number? foot)
         (number? ceiling)
         (> foot ceiling)]}
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
   xs))

(defn z-function
  "Z-function membership function.
 
  **params**

  * `xs`       independent variable, vector
  * `foot`     where the function reattains zero.
  * `ceiling`  where the function levels off at 1.
  "
  [xs foot ceiling]
  {:pre [(> ceiling foot)]}
  (map
   (fn [x]
     (cond
       (<= x ceiling)                              1.0
       (and (>= x ceiling) (<= x (/ (+ ceiling foot) 2.0))) (- 1 (* 2.0 (math/expt (/ (- x ceiling) (- foot ceiling)) 2.0)))
       (and (>= x (/ (+ ceiling foot) 2.0)) (<= x foot)) (* 2.0 (math/expt (/ (- x foot) (- foot ceiling)) 2.0))
       (>= x foot) 0.0))
   xs))

(defn pi-function
  "Pi-function membership function.

  **params**
  
  * `xs`               independent variable, vector of numbers
  * `left-foot`        left 'foot', where the function begins to climb from zero.
  * `left-ceiling`     left 'ceiling', where the function levels off at 1.
  * `right-foot`       right 'foot', where the function reattains zero.
  * `right-ceiling`    right 'ceiling', where the function begins falling from 1.
  "
  [xs left-foot left-ceiling right-foot right-ceiling]
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
   xs))


