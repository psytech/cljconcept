(ns cljconcept.test.match
  (:require cljconcept.match)
  (:use clojure.test))


(deftest test-1
  (def match-fn (cljconcept.match/match [x _ z & y]))

  (def a (match-fn 1 2 3 4 5 6))
  (def b (match-fn 1 2 [3 4] [5 6]))

  (is (= a [1 3 '(4 5 6)]))
  (is (= b [1 [3 4] '([5 6])])))


(deftest test-2
  (def match-fn (cljconcept.match/match [x _ z & [y j]]))

  (def a (match-fn 1 2 3 4 5 6))
  (def b (match-fn 1 2 [3 4] [5 6] [7 8]))
  
  (is (= a [1 3 4 5]))
  (is (= b [1 [3 4] [5 6] [7 8]])))


(deftest test-3
  (def match-fn (cljconcept.match/match [x _ z & [y _ j]]))

  (def a (match-fn 1 2 3 4 5 6))
  (def b (match-fn 1 2 [3 4] [5 6] [7 8] 9))
  
  (is (= a [1 3 4 6]))
  (is (= b [1 [3 4] [5 6] 9])))
