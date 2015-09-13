;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns biiwide.applicative.data)

(defn- fconj-some
  [f]
  (fn [result arg]
    (if-some [v (f arg)]
      (conj result v)
      result)))

(defn constant [x]
  (vary-meta (constantly x)
    assoc ::constant true))

(defn constant? [x]
  (true? (::constant (meta x))))

(def always-nil (constant nil))

(defn always-nil? [x]
  (= always-nil x))

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
        (if (nil? f's)
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
            (let [parts (remove always-nil?
                          (map transform-item x))]
              (if (empty? parts)
                always-nil
                (let [accumulator (accumulate seed
                                    (mapv fconj-some parts))]
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
      (nil? x) always-nil

      ;; Otherwise, return a constant function of x
      :else         (constant x)
      )))


(defn ->a
  "Compose a series of expressions left-to-right."
  [& exprs]
  (reduce comp
    (reverse (map ->transform exprs))))

(defn some->a
  "Compose a series of expressions from left to right that
will abort when one expression returns nil"
  [& exprs]
  (reduce (fn [f1 f2]
                 (fn [arg]
                   (when (some? arg)
                     (when-some [v (f1 arg)]
                       (f2 v)))))
    (map ->transform exprs)))

(defn ora
  "Constructs a function that returns either the first
truthy result from applying each expressions to the same
argument or nil if no truthy results where returned."
  ([] always-nil)
  ([& exprs]
    (apply some-fn (map ->transform exprs))))

(defn anda
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
        (and (f1 arg) (f2 arg)))))
  ([expr1 expr2 & more-exprs]
    (reduce anda
      expr1 (cons expr2 more-exprs))))

(defn nota
  "Construct a function that returns the boolean complement
of the expression."
  [expr]
  (complement (->transform expr)))

(defn whena
  "When pred-expr returns truthy value for the argument
then apply expr to the argument."
  [pred-expr expr]
  (let [p (->transform pred-expr)
        f (->transform expr)]
    (fn [arg]
      (when (p arg)
        (f arg)))))

(defn when-nota
  "When pred-expr does not return a truthy value for
the argument then apply expr to the argument.

(when-nota pos? (*a -1))"
  [pred-expr expr]
  (whena (nota pred-expr) expr))

(defn conda
  "Test a value with a seriese of applicative expressions.
When an expression matches apply the corresponding applicative
expression to the original value.
Example:
(conda neg? 0
       odd? inc
       pos? (*a 2)
       0)"
  ([expr] expr)
  ([pred-expr expr]
    (whena pred-expr expr))
  ([pred-expr expr & more-pred-expr-pairs]
    (apply ora
      (cons (whena pred-expr expr)
        (map (partial apply conda)
          (partition-all 2 more-pred-expr-pairs))))))
  

(defprotocol Liftable
  (lift* [x] [x arg-exprs-coll]))

(extend clojure.lang.Fn
  Liftable
  {:lift* (fn self
            ([f] (partial self f))
            ([f arg-exprs-coll]
              (let [arg-fs (map ->transform arg-exprs-coll)]
                (case (count arg-fs)
                  1 (let [[arg-f] arg-fs]
                      (fn [arg] (f (arg-f arg))))
                  2 (let [[arg-f1 arg-f2] arg-fs]
                      (fn [arg]
                        (f (arg-f1 arg) (arg-f2 arg))))
                  3 (let [[arg-f1 arg-f2 arg-f3] arg-fs]
                      (fn [arg]
                        (f (arg-f1 arg) (arg-f2 arg) (arg-f3 arg))))
                  (fn [arg]
                    (apply f (for [af arg-fs] (af arg))))))))})

(extend clojure.lang.Symbol
  Liftable
  {:lift* (fn self
            ([s] (partial self s))
            ([s arg-exprs-coll]
              (let [f's (map ->transform arg-exprs-coll)]
                (eval `(fn [~'arg]
                         (~s ~@(map #(list % 'arg) f's)))))))})

(defn lift
  "Lift a function of two or more arguments into a function
of one argument where each argument to the original function is
obtained by applying the single argument to each argument-expression.

In other words:
  (lift f expr1 expr2)
  => (fn [arg] (f ((->transform expr1) arg)
                  ((->transform expr2) arg)))"
  ([f-or-sym]
    (partial lift f-or-sym))
  ([f-or-sym expr]
    (lift* f-or-sym [identity expr]))
  ([f-or-sym expr & more-exprs]
    (lift* f-or-sym (cons expr more-exprs))))

(defn unary-lift
  "Lift a function of one argument into a function of"
  ([f] (lift* f))
  ([f expr] (lift* f (list expr))))

(def *a   "Lifted applicative form of *" (lift *))
(def +a   "Lifted applicative form of +" (lift +))
(def -a   "Lifted applicative form of -" (lift -))
(def diva "Lifted applicative form of /" (lift /))

(def consa (lift (fn [a b] (cons b a))))
(def conja (lift conj))

(defn assoca
  ([key expr]
    (let [arg-f (->transform expr)]
      (fconj-some (pair key (->transform expr)))))
  ([key expr & more-key-expr-pairs]
    (->a (assoca key expr)
         (apply assoca more-key-expr-pairs))))

(defn guard
  "Given a unary predicate function, returns a function which
returns it's argument if it satisfies the predicate"
  ([pred]
    (whena pred identity))
  ([pred expr]
    (guard (->a expr pred))))

(defn guardn
  "Constructs a guard function using predicate of two or more arguments.
[pred constant]: A guard of the predicate curried with a constant.
[pred expr1 expr2 ...]: A guard of the predicate lifted with the expression arugments."
  ([pred expr1 expr2 & more-exprs]
    (apply (guardn pred) expr1 expr2 more-exprs))
  ([pred constant]
    ((guardn pred) constant))
  ([pred]
    (fn ([constant]
          (guard (lift pred constant)))
        ([expr1 expr2]
          (guard (lift pred expr1 expr2)))
        ([expr1 expr2 & more-exprs]
          (guard (apply lift pred expr1 expr2 more-exprs))))))

(def =a  "Guard using ="  (guardn =))
(def <a  "Guard using <"  (guardn <))
(def >a  "Guard using >"  (guardn >))
(def <=a "Guard using <=" (guardn <=))
(def >=a "Guard using >=" (guardn >=))


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
      (->a arg-expr (self f-expr)))))

(defn something?
  "Returns true when 'x is not nil or an empty collection"
  [x]
  (not
    (or (nil? x)
        (and (coll? x)
             (empty? x)))))

(letfn [(filtered [filter-f seq-f]
          (fn [f coll]
            (not-empty
              (filter-f something?
                (seq-f f coll)))))]

  (def mapa
    "Map an applicative expression over the results of another expression.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filter  map)))

  (def mapav
    "Map an applicative expression over the results of another expression,
and return the results in a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv mapv)))

  (def filtera
    "Filter the results of an applicative expression with an
applicative expression.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filter  filter)))

  (def filterav
    "Filter the results of an applicative expression with an
applicative expression, and returns a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv filterv)))

  (def removea
    "Remove the results of an applicative expression with an
applicative expression.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filter  remove)))

  (def removeav
    "Remove the results of an applicative expression with an
applicative expression, and returns a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv remove)))
  )


(defn defaulta
  "Apply expr, return default-value if expr returns false or nil"
  [default-value expr]
  (ora expr (constant default-value)))

(defn betweena
  "Constrain a numeric value between two limits. 
Both the upper and lower limits are inclusive.
If no default-value is provided, use the first
limit."
  ([limit-a limit-b default-value expr]
    (defaulta default-value
      (-> expr
          (anda number?
                (<=a (max limit-a limit-b))
                (>=a (min limit-a limit-b))))))
  ([limit-a limit-b expr]
    (betweena limit-a limit-b limit-a expr)))
