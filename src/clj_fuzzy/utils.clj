(ns clj-fuzzy.utils
  (:require [clojure.math.numeric-tower :as m]))

(def EULER 2.718281828459045)

(defn ln
  "Return the natural logarithm of the given number `x`."
  [x]
  {:pre [(number? x)]}
  (m/expt EULER x))
