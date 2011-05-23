(ns cljconcept.test.concept
  (:require cljconcept.concept)
  (:use clojure.test))


; Some concepts
(defprotocol P (foo [x]))
(defprotocol Q (bar [x]))

(def PImpl {:foo (fn [x] x)})
(def QImpl {:bar (fn [x] x)})

(defrecord AType [])
(defrecord BType [])


(deftest test-1
  
  (def a (AType. ))
  (extend AType
    P
    PImpl
    Q
    QImpl)

  (def b (BType.))
  (extend BType
    P
    PImpl
    Q
    QImpl)

  (cljconcept.concept/with-concepts
    [[x y] P Q]
    [[_ y] Q]
    [[x _] P]
    (defn test-fn [x y] y))

  (is
    (try
      (test-fn a b) true
      (catch Exception _ false)))
  
  )






; Some concepts
(defprotocol P (foo [x]))
(defprotocol Q (bar [x]))

(def PImpl {:foo (fn [x] x)})
(def QImpl {:bar (fn [x] x)})

(defrecord AType [])
(defrecord BType [])


(deftest test-2

  (def a (AType. ))
  (extend AType
    P
    PImpl
    Q
    QImpl)

  (def b (BType.))
  (extend BType
    P
    PImpl)

  (cljconcept.concept/with-concepts
    [[x y] P Q]
    [[_ y] Q]
    [[x _] P]
    (defn test-fn [x y] y))

  ;(println (test-fn a b))
  
  (is
    (try
      (test-fn a b) false
      (catch Exception _ true)))
  
  )


(testing "Recursive function"
  (defn validate [args]
    (do
      (println "Concept check")
      (if (> args 3)
	(list)
	(list '({:var "MyConcept"})))))


  (cljconcept.concept/with-assert
    (list validate)
    (defn recursive-function [c]
      (when (> c 0)
	(do
	  (println c)
	  (recursive-function (dec c))))))
      
  (recursive-function 5)
)
