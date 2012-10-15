(ns spam.test
  (:use [spam.core] [clojure.test])
  (:import spam.core.WordFeature))

(def a (train
        (train (get-clean-state) "This is a spam sentence" :spam)
        "This is also a spam sentence" :spam))

(def b (conj
        (conj a {"test" (WordFeature. "test" 10 0)})
        { :total-spam-count (+ 10 (a :total-spam-count))}))

(def c (conj
        (conj a {"test" (WordFeature. "test" 1000 0)})
        { :total-spam-count (+ 1000 (a :total-spam-count))}))

(defn about [x y & precision]
  (let [p (or precision 0.001)]
    (and (> x (- y p)) (< x (+ y p)))))

(deftest test-bayesian-probability
  (is (= 0.75 (bayesian-spam-probability a "also")))
  (is (about 0.955 (bayesian-spam-probability b "test")))
  (is (about 0.9995 (bayesian-spam-probability c "test"))))

(deftest test-spam-count
  (is (= 2 (spam-count a "spam"))))

(deftest test-ham-count
  (is (= 0 (ham-count a "spam"))))

(deftest test-factorial
  (are [x y] (= y (factorial x))
       3 6
       4 24
       5 120
       6 720))

(deftest inverse-chi-test
  (are [x y z] (about z (inverse-chi-square x y))
       2 10 0.9963
       10 16 0.8666
       4 6 0.6766
       590 600 0.6068
       1 2 0.6065))

(run-all-tests)