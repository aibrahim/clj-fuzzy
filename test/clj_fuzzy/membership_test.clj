(ns clj-fuzzy.membership-test
  (:require [clj-fuzzy.membership :as m]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest triangular-test
  (testing "with empty collection"
    (is (empty? (m/triangular [] 1 2 3))))

  (testing "with a > b > c"
    (is (thrown? AssertionError (m/triangular [1 2 3] 3 2 1))))

  (testing "with invalid triangular shape parameters"
    (is (thrown? AssertionError (m/triangular [1 2 3] :a :b :c))))

  (testing "with valid inputs"
    (is (=
         '(0.0 1.0 0.0)
         (m/triangular [1 2 3] 1 2 3)))))

(deftest trapezoidal-test
  (testing "with empty collection"
    (is (empty? (m/trapezoidal [] 1 2 3 4))))

  (testing "with a > b > c > d"
    (is (thrown? AssertionError (m/trapezoidal [1 2 3] 4 3 2 1))))

  (testing "with invalid trapezoidal shape params"
    (is (thrown? AssertionError (m/trapezoidal [1 2 3] :a :b :c :d))))

  (testing "with valid inputs"
    (is (=
         '(0.0 1.0 1.0 0.0)
         (m/trapezoidal [1 2 3 4] 1 2 3 4)))))

(deftest gaussian-test
  (testing "with empty collection"
    (is (empty? (m/gaussian [] 0 1))))

  (testing "with negative standard deviation"
    (is (thrown? AssertionError (m/gaussian [1 2] 0 -1))))
  
  (testing "with valid inputs"
    (is (=
         '(1.0)
         (m/gaussian [0] 0.0 1.0)))))
