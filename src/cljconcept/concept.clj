(ns ^{:doc "Concept checking facility for function arguments"
      :author "psytech"
      :email "psytech@psycho-technique.net"}
      cljconcept.concept
  (:require cljconcept.match))


(defn- recursive-replace [old new coll]
  "Replace all item which are equal to old with new. coll might be a
  nested sequence."
  (let [match (fn [x] (if (= x old) new x))]
    (loop [c coll out (list)]
      (if (empty? c)
	out
        (if (seq? (first c))
	  (recur (rest c) (concat out (list (recursive-replace old new (first c)))))
	  (recur (rest c) (concat out (list (match (first c))))))))))


(defn- get-renamed-function [expr & [name]]
  "Renames a function, which is a list of symbols and therefore can be
  modified. expr is a list of symbols representing the function. name
  is an optional new name. If no name is explicitly given, a unique name
  is automatically generated."
  (let [head (first expr)                                                       ; should be the symbol 'defn'
        fname (if (nil? name) (gensym) (symbol name))
	new-fn (recursive-replace (first (rest expr)) fname expr)]              ; generate unique name if no 
     new-fn))                                                                   ;   name was explicitly given
    
(defn- check-against-all-concepts [concepts]
  "Returns a funtion, which checks if the argument satisfies all given
  concepts. On success an empty list is returned. Otherwise the list
  contains all concepts which are not satisfied. concept is a list of
  protocols."
  (fn [x]
    (let [get-if-failed (fn [p arg] (when-not (satisfies? p arg) (list p)))]    
      (loop [c concepts failed (list)]
        (if (empty? c)
          failed
          (recur (rest c) (concat failed (get-if-failed (first c) x))))))))
  
(defn get-concept-verifier [arg-access-fn & concepts]
  "Returns a function to check if the arguments satisfies all given
  concepts (protocols). arg-access-fn is a function which returns the
  arguments from the function arguments to be verified. concepts is a
  list of protocols."
  (fn [& x]
    (let [check (check-against-all-concepts concepts)]
      (map check (apply arg-access-fn x)))))       

(defn- single-error-msg [failure]
  (let [f (filter #(not (empty? %)) failure)
        g (fn [x] (map #(if (empty? %) (list) (str (:var %))) x))]
    (map g f)))

(defn create-formatted-error-msg [failures]
  (let [cr "\n     "]
    (loop [f failures errmsg nil]
      (if (empty? f)
        (print-str errmsg)
        (recur (rest f)
	       (concat errmsg (list cr (single-error-msg (first f)))))))))
  

(defmacro with-assert [validate-fns expr]
  "Macro to wrap an assertion around a function. fns-validate is a sequence
  of function every a single concept checking statement. expr is a function
  to be wrapped. This macro returns a new function with the name of expr.
  The original expr is stored in the macro-scope with a new name."
  (let [fname  (first (rest expr))                                              ; store the name of the function/expr
        -expr- (get-renamed-function expr)]                                     ; store renamed function/expr
    `(defn ~fname [& args#]                                                     ; return a new function with the old name
       (let [failed# (map (fn [x#] (apply x# args#)) ~validate-fns)             ; all failed concepts
             valid# (every? #(every? empty? %) failed#)]                        
         (if valid#                                        
           (apply ~-expr- args#)                                                ; if valid, then evaluate function/expr
           (throw (Exception.                                                   
		    (str "\nFollowing concepts are not fulfilled:\n"
		      (create-formatted-error-msg failed#)))))))))
                                                      
(defmacro make-concept-verifier [pattern & concepts]
  "This macro creates a single concept ckecking function."
  `(let [arg-fn# (cljconcept.match/match ~pattern)                              ; get function to map arguments
         -concepts-# (list ~@concepts)]                                     
     (apply get-concept-verifier (concat (list arg-fn#) -concepts-#))))         ; return single concept checking function

(defmacro with-concepts [& x]
  "Macro to apply a concept checking facility to a given function. x is a
  list of concept checking statements. The last item of x is the function
  itself.

  Example:
  ========

  (with-concepts
    [[x y] P Q]
    [[_ y] R]
    (defn test-fn [x y] ...))

  This construct checks, if x and y satisfy the concepts P and Q. y should
  also fulfill concept R.

  If any check failes, an exception is thrown with additional information
  indicating the arguments and the protocols failing.

  Example:
  ========

  (with-concepts
    [[x y] P Q]
    [[_ y] Q]
    [[x _] P]
    (defn test-fn [x y] ...))

   Assume x implements P and Q and y only implements Q. Then the exception
   looks like this:

   java.lang.Exception: 
   Following concepts are not fulfilled:
   (
          ((#'cljconcept.concept/Q) (#'cljconcept.concept/Q)) 
          ((#'cljconcept.concept/Q)) 
          ())

   Here you can clearly see the errors.


   NOTE: It is important to note, that the concept checking is only done in
         the very first call to the function. If the fuinction is recursive
         then the concept check is only done once."
  (let [concepts (butlast x)
        expr (last x)]
   `(let [f# (list ~@(map (fn [n] `(make-concept-verifier ~@n)) concepts))]      
      (with-assert f# ~expr))))

