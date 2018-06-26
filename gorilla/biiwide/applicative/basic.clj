;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns biiwide.applicative.basic)

(defn fconj-some
  [f]
  (fn [result arg]
    (if-some [v (f arg)]
      (conj result v)
      result)))

(defn fassoc-some
  [k f]
  (fn [result arg]
    (if-some [v (f arg)]
      (assoc result k v)
      result)))

(defn accumulate
  [seed f's]
  (fn [arg]
    (not-empty
      (reduce (fn [result f] (f result arg))
              seed f's))))

(defn ->transform
  [x]
  (cond
    ;; If x is a map, recursively transform all values, and accumulate "assoc's".
    (map? x)     (accumulate nil
                   (map
                     (fn [[k v]]
                       (fassoc-some k (->transform v)))
                     x))

    ;; If x is a vector, recursively transform all elements
    (vector? x)  (accumulate []
                   (map
                     (comp fconj-some ->transform)
                     x))

    ;; If x is any other seq, reverse it and recursively transform all elements
    (seq? x)     (accumulate '()
                   (map
                     (comp fconj-some ->transform)
                     (reverse x)))

    ;; If x is a function, return it.
    (fn? x)      x

    ;; If x is a keyword, return it, because keywords are functions too.
    (keyword? x) x

    ;; Otherwise, return a constant function of x
    :else        (constantly x)
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/-&gt;transform</span>","value":"#'biiwide.applicative.basic/->transform"}
;; <=

;; @@
(defn lift
  ([f & args]
   (let [arg-fs (map ->transform args)]
     (fn [arg]
       (apply f (for [af arg-fs] (af arg))))))
  ([f] (partial lift f)))


(defn guard
  ([pred] (partial guard pred))
  ([pred xform-arg]
   (let [f (->transform xform-arg)]
     (fn [arg]
       (let [v (f arg)]
         (when (pred v)
           v))))))

(defn and-f
  [& args]
  (when-let [[a & more-as] (not-empty args)]
    (if (and a more-as)
      (recur more-as)
      a)))

(defn or-f
  [& args]
  (when (not-empty args)
    (if-let [a (first args)]
      a
      (recur (rest args)))))

(defn not-f
  [arg]
  (when-not arg true))

(defn default
  [v f]
  (lift or-f f v))

(def pos-number?
  (guard (every-pred number? pos?)))

(def build-query-with-defaults
  (->transform
    {:query {:bool {:must [{:match {:first_name :first-name}}
                           {:match {:last_name  :last-name}}
                           {:range {:lt :before
                                    :gt :after}}]}}
     :from (default 0  (pos-number? :from))
     :size (default 30 (pos-number? :size))
     }))


(build-query-with-defaults
  {:first-name "Joe"
   :after "2014-01-01"
   :from 30})
