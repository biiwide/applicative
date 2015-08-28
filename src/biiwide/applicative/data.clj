;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns biiwide.applicative.data
  (:refer-clojure :exclude [-> = * + - / > >= < <=
                            some->
                            and or not
                            integer? float? number?
                            ratio? rational? string?
                            filter filterv
                            map mapv remove
                            when when-not])
  (:require [clojure.core :as core]))

(defn- fconj-some
  [f]
  (fn [result arg]
    (if-some [v (f arg)]
      (core/conj result v)
      result)))

(defn constant [x]
  (vary-meta (constantly x)
    assoc ::constant true))

(defn constant? [x]
  (true? (::constant (meta x))))

(def always-nil (constant nil))

(defn always-nil? [x]
  (core/= always-nil x))

(def always-true (constant true))

(defn- pair [k f]
  (cond (always-nil? f) always-nil
        (constant? f)   (constant [k (f nil)])
        :else
        (fn [arg]
          (when-some [v (f arg)]
            [k v]))))

(defn- accumulate
  [seed f's]
  (fn [arg]
    (not-empty
      (loop [result seed
             f's f's]
        (if (core/nil? f's)
          result
          (let [[f & more-fs] f's]
            (recur (f result arg) more-fs)))))))

(defn ->transform
  "Transforms an applicative expression into a function.
Expressions may consist of functions, primitive values, and
collections of functions and primitive values"
  [x]
  (letfn [(->transform-pair [[k v]]
            (pair k (->transform v)))
          (->transform-coll [seed transform-item x]
            (let [parts (core/remove always-nil?
                          (core/map transform-item x))]
              (if (empty? parts)
                always-nil
                (let [accumulator (accumulate seed
                                    (core/mapv fconj-some parts))]
                  (if (every? constant? parts)
                    (constant (accumulator nil))
                    accumulator)))))]
    (cond
      ;; If x is a function, directly return it.
      (fn? x)       x

      ;; If x is a map, recursively transform all values, and accumulate "assoc's".
      (map? x)      (->transform-coll {} ->transform-pair x)

      ;; If x is a vector, recursively transform all elements
      (vector? x)   (->transform-coll [] ->transform x)

      ;; If x can behave like a function, directly return it.
      (ifn? x)      x

      ;; If x is any other seq, reverse it and recursively transform all elements
      (seq? x)      (->transform-coll nil ->transform x)

      ;; If x is nil, return the constant, static always-nil
      (core/nil? x) always-nil

      ;; Otherwise, return a constant function of x
      :else         (constant x)
      )))


(defn ->
  "Compose a series of expressions left-to-right."
  [& exprs]
  (core/reduce core/comp
    (core/reverse (core/map ->transform exprs))))

(defn some->
  "Compose a series of expressions from left to right that
will abort when one expression returns nil"
  [& exprs]
  (core/reduce (fn [f1 f2]
                 (fn [arg]
                   (core/when (some? arg)
                     (core/when-some [v (f1 arg)]
                       (f2 v)))))
    (core/map ->transform exprs)))

(defn or
  "Constructs a function that returns either the first
truthy result from applying each expressions to the same
argument or nil if no truthy results where returned."
  [& exprs]
  (apply some-fn (core/map ->transform exprs)))

(defn and
  "Constructs a function that returns either the first
falsey result from applying each expressions to the same
argument or the last truthy result if all results where
truthy."
  ([] always-true)
  ([expr] (->transform expr))
  ([expr1 expr2]
    (let [f1 (->transform expr1)
          f2 (->transform expr2)]
      (fn [arg]
        (core/and (f1 arg) (f2 arg)))))
  ([expr1 expr2 & more-exprs]
    (core/reduce and
      expr1 (core/cons expr2 more-exprs))))

(defn not
  "Construct a function that returns the boolean complement
of the expression."
  [expr]
  (complement (->transform expr)))

(defn when
  "When pred-expr returns truthy value for the argument
then apply expr to the argument."
  [pred-expr expr]
  (let [p (->transform pred-expr)
        f (->transform expr)]
    (fn [arg]
      (core/when (p arg)
        (f arg)))))

(defn when-not
  "When pred-expr does not return a truthy value for
the argument then apply expr to the argument."
  [pred-expr expr]
  (when (not pred-expr) expr))


(defn- lift-fn
  "Convert a function of n arguments into a function of
one argument and produce each argument to the original
function by applying the corresponding positional
transformation to the outer argument."
  ([f]
    (partial lift-fn f))
  ([f arg-expr]
    (let [arg-f (->transform arg-expr)]
      (fn [arg] (f (arg-f arg)))))
  ([f arg-expr1 arg-expr2]
    (let [arg-f1 (->transform arg-expr1)
          arg-f2 (->transform arg-expr2)]
      (fn [arg]
        (f (arg-f1 arg) (arg-f2 arg)))))
  ([f arg-expr1 arg-expr2 arg-expr3]
    (let [arg-f1 (->transform arg-expr1)
          arg-f2 (->transform arg-expr2)
          arg-f3 (->transform arg-expr3)]
      (fn [arg]
        (f (arg-f1 arg) (arg-f2 arg) (arg-f3 arg)))))
  ([f arg-expr1 arg-expr2 arg-expr3 & arg-exprs]
   (let [arg-fs (core/mapv ->transform
                  (core/list* arg-expr1 arg-expr2 arg-expr3 arg-exprs))]
     (fn [arg]
       (apply f (for [af arg-fs] (af arg)))))))

(defn- lift-sym
  ([f]
    (partial lift-sym))
  ([f & arg-exprs]
    (let [f's (core/map ->transform arg-exprs)]
      (eval `(fn [~'arg]
               (~f ~@(core/map #(core/list % 'arg) f's)))))))

(defn lift
  "Lift a function of any number of arguments into a function
of one argument where each argument to the original function is
obtained by applying the single argument to each argument-expression.

In other words:
  (lift f expr1 expr2)
  => (fn [arg] (f ((->transform expr1) arg)
                  ((->transform expr2) arg)))"
  [f-or-sym & arg-exprs]
  (cond (symbol? f-or-sym)
        (apply lift-sym f-or-sym arg-exprs)
        :else
        (apply lift-fn f-or-sym arg-exprs)))

(def * "Lifted form of clojure.core/*" (lift core/*))
(def + "Lifted form of clojure.core/+" (lift core/+))
(def / "Lifted form of clojure.core//" (lift core//))
(def - "Lifted form of clojure.core/-" (lift core/-))


(defn guard
  "Given a unary predicate function, returns a function which
returns it's argument if it satisfies the predicate"
  ([pred]
    (fn [arg]
      (core/when (pred arg) arg)))
  ([pred expr]
    (guard (-> expr pred))))

(defn binary-guard
  "Constructs a guard function using predicate of two or more arguments.
[pred constant]: A guard of the predicate curried with the constant.
[pred expr1 expr2 ...]: A guard of the predicate lifted with the expression arugments."
  ([pred expr1 expr2 & more-exprs]
    (apply (binary-guard pred) expr1 expr2 more-exprs))
  ([pred constant]
    ((binary-guard pred) constant))
  ([pred]
    (fn ([constant]
          (guard (partial pred constant)))
        ([expr1 expr2]
          (guard (lift pred expr1 expr2)))
        ([expr1 expr2 & more-exprs]
          (guard (apply lift pred expr1 expr2 more-exprs))))))

(def =  "Binary guard using clojure.core/="  (binary-guard core/=))
(def <  "Binary-guard using clojure.core/<"  (binary-guard core/<))
(def >  "Binary-guard using clojure.core/>"  (binary-guard core/>))
(def <= "Binary-guard using clojure.core/<=" (binary-guard core/<=))
(def >= "Binary-guard using clojure.core/>=" (binary-guard core/>=))


(defn- lift-fa
  "Lift a function of (fn -> any) to
   (xform -> (arg -> any))"
  [core-f]
  (fn self
    ([f-expr]
      (let [f (->transform f-expr)]
        (fn [arg]
          (core-f f arg))))
    ([f-expr arg-expr]
      (-> arg-expr (self f-expr)))))

(defn something?
  "Returns true when 'x is not nil or an empty collection"
  [x]
  (core/not
    (core/or (core/nil? x)
             (core/and (core/coll? x)
                       (core/empty? x)))))

(defn- filtered [filter-f seq-f]
  (fn [f coll]
    (core/not-empty
      (filter-f something?
        (seq-f f coll)))))

(def map     (lift-fa (filtered core/filter  core/map)))
(def mapv    (lift-fa (filtered core/filterv core/mapv)))
(def filter  (lift-fa (filtered core/filter  core/filter)))
(def filterv (lift-fa (filtered core/filterv core/filterv)))
(def remove  (lift-fa (filtered core/filter  core/remove)))
(def removev (lift-fa (filtered core/filterv core/remove)))


(defn default
  "Apply expr, return default-value if expr returns false or nil"
  [default-value expr]
  (or expr (constant default-value)))

(defn between
  "Constrain a numeric value between two limits. 
Both the upper and lower limits are inclusive.
If no default-value is provided, use the first
limit."
  ([limit-a limit-b default-value expr]
    (default default-value
      (-> expr
          (and core/number?
               (<= (core/max limit-a limit-b))
               (>= (core/min limit-a limit-b))))))
  ([limit-a limit-b expr]
    (between limit-a limit-b limit-a expr)))