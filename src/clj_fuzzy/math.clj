(ns clj-fuzzy.math
  (:require
    [clj-fuzzy.membership :as m]))

(defn- merge-memberships
  "Merge the memberships of a collection according to a given function.

  Takes a function `f` and a collection `coll` containing membership maps.
  Each membership map should have the following keys:
    - :set-title    - The title of the fuzzy set.
    - :value        - The value associated with the membership.
    - :membership   - The membership value.

  Returns a merged hash-map containing the same input shape

  Example:
  (merge-memberships max
    [{:set-title \"young\" :value 7 :membership 0.1}
     {:set-title \"young+\" :value 23 :membership 0.7}])
  => {:set-title \"young ∪ young+\", :membership 0.7}

  Note: The function `f` should take two membership values and return the result
  of merging them according to the desired logic (e.g., max, min, sum, etc.)."
  [f coll]
  (reduce (fn [res m]
            (-> res
               (update :membership #(f (:membership m) %))
               (assoc :set-title (str (:set-title res) " ∪ " (:set-title m)))))
          (first coll)
          (rest coll)))

(defn- fuzzify-merge
  [f coll mfs]
  (->> mfs
       (map #(m/-fuzzify % coll))
       (apply (partial map vector))
       (map (partial merge-memberships f))))

(defn fz-union
  "Perform fuzzy union on a collection of numerical values with arbitrary memberships.
  
  Takes a collection `coll` of numerical values and an arbitrary number of membership functions `mfs`.
  Each membership function should be a function that takes a numerical value and returns a membership map.
  A membership map should have the following keys:
    - :set-title    - The title of the membership set.
    - :membership   - The membership value.

  Returns a collection of merged membership maps, where each map represents the fuzzy union of the values in `coll`
  with the corresponding membership functions `mfs`. The fuzzy union is calculated using the 'max' function.

  Example:
  (fz-union [10 20 30]
   (clj-fuzzy.membership/trapezoidal \"young\" 0 0 10 15)
   (clj-fuzzy.membership/trapezoidal \"young+\" 10 15 25 30))

  => ({:set-title \"young ∪ young+\", :value 10, :membership 1.0}
      {:set-title \"young ∪ young+\", :value 20, :membership 1.0}
     {:set-title \"young ∪ young+\", :value 30, :membership 0.0})

  Note: The 'max' function is used to merge the memberships of the values in `coll` according to each membership function
  "
  [coll & mfs]
  (fuzzify-merge max coll mfs))

(defn fz-intersect
  "Perform fuzzy intersection on a collection of numerical values with arbitrary memberships.
  
  Takes a collection `coll` of numerical values and an arbitrary number of membership functions `mfs`.
  Each membership function should be a function that takes a numerical value and returns a membership map.
  A membership map should have the following keys:
    - :set-title    - The title of the membership set.
    - :membership   - The membership value.

  Returns a collection of merged membership maps, where each map represents the fuzzy union of the values in `coll`
  with the corresponding membership functions `mfs`. The fuzzy union is calculated using the 'max' function.

  Example:
  (fz-union [10 20 30]
   (clj-fuzzy.membership/trapezoidal \"young\" 0 0 10 15)
   (clj-fuzzy.membership/trapezoidal \"young+\" 10 15 25 30))

  => ({:set-title \"young ∪ young+\", :value 10, :membership 0.0}
      {:set-title \"young ∪ young+\", :value 20, :membership 0.0}
     {:set-title \"young ∪ young+\", :value 30, :membership 0.0})

  Note: The 'min' function is used to merge the memberships of the values in `coll` according to each membership function
  "
  [coll & mfs]
  (fuzzify-merge min coll mfs))

(defn fz-complement
  [coll mf]
  (->> (m/-fuzzify mf coll)
       (map #(-> %
                (update :membership (partial - 1))
                (update :set-title (partial str "complement "))))))
