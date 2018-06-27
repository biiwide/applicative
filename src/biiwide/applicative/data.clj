;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns biiwide.applicative.data
  (:refer-clojure :exclude [constantly]))

(defn conj-some
  [coll v]
  (if (some? v)
    (conj coll v)
    coll))

(defn- fconj
  [conjoiner]
  (fn [f]
    (fn [result arg]
      (conjoiner result (f arg)))))

(def ^:private fconj-some
  (fconj conj-some))

(defn constantly [x]
  (vary-meta (clojure.core/constantly x)
    assoc ::constant true))

(def constant constantly)

(defn constant? [x]
  (true? (::constant (meta x))))

(def always-nil (constantly nil))

(defn always-nil? [x]
  (= always-nil x))

(def always-true (constantly true))


(defn- pair [k f]
  (cond (always-nil? f) always-nil
        (constant? f)   (constantly [k (f nil)])
        :else
        (fn [arg]
          (when-some [v (f arg)]
            [k v]))))

(defn- accumulate
  [seed f's]
  (fn [arg]
    (loop [result seed
           f's f's]
      (if (nil? f's)
        result
        (let [[f & more-fs] f's]
          (recur (f result arg) more-fs))))))


(defn transformation-builder
  [conjoiner]
  (let [fconjer (fconj conjoiner)]
    (fn transformer [x]
      (letfn [(pair-transformer [[k v]]
                (pair k (transformer v)))
              (coll-transformer [seed transformer x]
                (let [parts (remove always-nil?
                              (map transformer x))]
                  (cond
                    (empty? parts) always-nil
                    (every? constant? parts) (constantly x)
                    :else (comp not-empty
                                (accumulate seed
                                  (mapv fconjer parts))))))]
        (cond
          ;; If x is a function, directly return it.
          (fn? x)       x

          ;; If x is a map, recursively transform all values, and accumulate "assoc's".
          (map? x)      (coll-transformer {} pair-transformer x)

          ;; If x is a vector, recursively transform all elements
          (vector? x)   (coll-transformer [] transformer x)

          ;; If x is a set, recursively transform all elements
          (set? x)      (coll-transformer #{} transformer x)

          ;; If x can behave like a function, directly return it.
          (ifn? x)      x

          ;; If x is any other seq, reverse it and recursively transform all elements
          (seq? x)      (coll-transformer nil transformer x)

          ;; If x is nil, return the constant, static always-nil
          (nil? x)      always-nil

          ;; Otherwise, return a constant function of x
          :else         (constantly x)
          )))))


(def transformer
  "Transforms an applicative expression into a function.
Expressions may consist of functions, primitive values, and
collections of functions and primitive values"
  (transformation-builder conj-some))


(def transformer-with-nils
  "Transforms an applicative expression into a function.
Expressions may consist of functions, primitive values, and
collections of functions and primitive values"
  (transformation-builder conj))


(def ^:dynamic *transformer*
  transformer)


(defmacro with-transformer [transformer-fn & body]
  `(binding [*transformer* ~transformer-fn]
     ~@body))


(defn ->a
  "Compose a series of expressions left-to-right."
  [& exprs]
  (reduce comp
    (reverse (map *transformer* exprs))))

(defn some->a
  "Compose a series of expressions from left to right that
will abort when one expression returns nil"
  [& exprs]
  (reduce (fn [f1 f2]
                 (fn [arg]
                   (when (some? arg)
                     (when-some [v (f1 arg)]
                       (f2 v)))))
    (map *transformer* exprs)))

(defn ora
  "Constructs a function that returns either the first
truthy result from applying each expressions to the same
argument or nil if no truthy results where returned."
  ([] always-nil)
  ([expr] (*transformer* expr))
  ([expr1 & exprs]
    (apply some-fn (map *transformer* (cons expr1 exprs)))))

(defn anda
  "Constructs a function that returns either the first
falsey result from applying each expressions to the same
argument or the last truthy result if all results where
truthy."
  ([] always-true)
  ([expr] (*transformer* expr))
  ([expr1 expr2]
    (let [f1 (*transformer* expr1)
          f2 (*transformer* expr2)]
      (fn [arg]
        (and (f1 arg) (f2 arg)))))
  ([expr1 expr2 & more-exprs]
    (reduce anda
      expr1 (cons expr2 more-exprs))))

(defn nota
  "Construct a function that returns the boolean complement
of the expression."
  [expr]
  (complement (*transformer* expr)))

(defn whena
  "When pred-expr returns truthy value for the argument
then apply expr to the argument."
  [pred-expr expr]
  (let [p (*transformer* pred-expr)
        f (*transformer* expr)]
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
  ([expr] (*transformer* expr))
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
              (let [arg-fs (map *transformer* arg-exprs-coll)]
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
              (let [f's (map *transformer* arg-exprs-coll)]
                (eval `(fn [~'arg]
                         (~s ~@(map #(list % 'arg) f's)))))))})

(defn lift
  "Lift a function of two or more arguments into a function
of one argument where each argument to the original function is
obtained by applying the single argument to each argument-expression.

In other words:
  (lift f expr1 expr2)
  => (fn [arg] (f ((transformer expr1) arg)
                  ((transformer expr2) arg)))"
  ([f-or-sym]
    (partial lift f-or-sym))
  ([f-or-sym expr]
    (lift* f-or-sym [identity expr]))
  ([f-or-sym expr & more-exprs]
    (lift* f-or-sym (cons expr more-exprs))))

;(defn unary-lift
;  ([f-or-sym]
;    (partial unary-lift f-or-sym))
;  ([f-or-sym expr]
;    (lift* f-or-sym [expr])))


(def +a   "Lifted applicative form of +" (lift +))
(def -a   "Lifted applicative form of -" (lift -))
(def *a   "Lifted applicative form of *" (lift *))
(def diva "Lifted applicative form of /" (lift /))

(def consa (lift (fn [a b] (cons b a))))

(def conja (lift conj))

(defn assoca
  ([key expr]
    (let [arg-f (*transformer* expr)]
      (fconj-some (pair key (*transformer* expr)))))
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
      (let [f (*transformer* f-expr)]
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

  (def mapva
    "Map an applicative expression over the results of another expression,
and return the results in a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv mapv)))

  (def filtera
    "Filter the results of an applicative expression with an
applicative expression.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filter  filter)))

  (def filterva
    "Filter the results of an applicative expression with an
applicative expression, and returns a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv filterv)))

  (def removea
    "Remove the results of an applicative expression with an
applicative expression.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filter  remove)))

  (def removeva
    "Remove the results of an applicative expression with an
applicative expression, and returns a vector.
All nils or empty collections will be pruned from the result."
    (lift-fa (filtered filterv remove)))
  )


(defn defaulta
  "Apply expr, return default-value if expr returns false or nil"
  [default-value expr]
  (ora expr (constantly default-value)))

(defn betweena
  "Constrain a numeric value between two limits. 
Both the upper and lower limits are inclusive.
If no default-value is provided, use the first
limit."
  ([limit-a limit-b default-value expr]
    (defaulta default-value
      (anda expr number?
            (<=a (max limit-a limit-b))
            (>=a (min limit-a limit-b)))))
  ([limit-a limit-b expr]
    (betweena limit-a limit-b limit-a expr)))

(defn geta
  "Lookup a key in an associative collection"
  [k]
  (fn [arg]
    (get arg k)))

(defn selecta
  "Returns a map containing only the specified keys"
  [& keys]
  (*transformer* (zipmap keys (map geta keys))))

(defn withouta
  "Dissociate a group of keys from an associative collecion"
  [& keys]
  (apply lift dissoc identity keys))
