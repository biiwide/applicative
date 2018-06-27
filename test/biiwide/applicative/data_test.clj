;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns biiwide.applicative.data-test
  (:refer-clojure :exclude [constantly])
  (:require [biiwide.applicative.data :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [one-of]]
            [clojure.test.check.properties :refer [for-all]]))

(def constant-primitives
  (one-of [g/int g/char g/string g/ratio g/boolean]))

(def constant-collections
  (one-of [(g/vector constant-primitives)
           (g/list   constant-primitives)
           (g/map    (g/one-of [constant-primitives
                                g/keyword])
                     constant-primitives)]))

(def constants
  (one-of [constant-primitives
           constant-collections
           (g/recursive-gen
             (fn [gitem]
               (let [gcoll (rand-nth [g/vector g/list #(g/map % %)])]
                 (gcoll gitem)))
             constant-primitives)]))

(defspec constantly-constant 20
  (for-all [constant-val g/any
                 arg-val g/any]
    (let [constf (constant constant-val)]
      (= constant-val (constf arg-val)))))

(defspec constant-is-constant? 20
  (for-all [constant-val g/any]
    (constant? (constant constant-val))))

(deftest not-constant?
  (are [v]
    (not (constant? v))

    "foo" 1 {:some "thing"}
    inc :keyword 'symbol))


(defspec optimized-constants 200
  (for-all [constant-val constants]
    (constant? (transformer constant-val))))

(defspec optimized-nils 100
  (for-all [empty-v (g/recursive-gen
                           (fn [t]
                             (let [collf (rand-nth [g/vector g/list #(g/map g/string %)])]
                               (collf t)))
                           (g/elements [nil]))]
    (always-nil? (transformer empty-v))))

(deftest test-transformer
  (are [expr value expected]
    (= expected
      ((transformer expr) value))

    :a   {:b "B"} nil
    :a   nil      nil
    [:a] {:b "B"} nil
    (list :a) {:c "C"} nil
    [:a :a] {:b "B"} nil
    [:a :b :a] {:b "B"} ["B"]
    [:a :b :a] {:a "eh"} ["eh" "eh"]
    [:a :b :c] {:b "B" :c "C"} ["B" "C"]
    (list :b :b) {:b "B"} ["B" "B"]
    {:aaa [:a :b]
     :bbb [:b :c]} {:b "B"} {:aaa ["B"] :bbb ["B"]}
    {:aaa [:a :b]
     :bbb [:b :c]} {:Z "ZZZ"} nil
    {:aaa [:a :b]
     :bbb [:b :c]
     :ccc [count]} {:Z "ZZZ"} {:ccc [1]}
    {:one [inc]
     :two (comp inc inc)} 0 {:one [1] :two 2}
    {:string str
     :double #(* 2 %)} 123 {:string "123" :double 246}
    ))

(deftest test-anda
  (are [exprs v expected]
    (= expected ((apply anda exprs) v))

    [:first :second :last]
    {:first "first" :second "second" :last "last"}
    "last"

    []
    "abc"
    true

    [1 2 3]
    nil
    3

    [1 nil 3]
    555
    nil

    [nil nil nil]
    123
    nil

    ["one"]
    nil
    "one"

    [false "a"]
    :random
    false

    [pos? neg?]
    1
    false

    [number? integer?]
    1
    true
    ))

(deftest test-ora
  (are [exprs v expected]
    (= expected ((apply ora exprs) v))

    []
    "foo"
    nil

    [:first :second :last]
    {:first "first" :second "second" :last "last"}
    "first"

    [:first :second :last]
    {:first false :second "second" :last "last"}
    "second"

    [:first :second :last]
    {:first false :last "last"}
    "last"

    [1 2 3]
    nil
    1

    [1 nil 3]
    555
    1

    [nil nil nil]
    123
    nil

    ["one"]
    nil
    "one"

    [false "a"]
    :random
    "a"

    [pos? neg?]
    1
    true

    [number? integer?]
    1
    true
    ))

(deftest test-nota
  (are [expr v expected]
    (= expected ((nota expr) v))

    "one"              nil     false
    "one"              123     false
    :foo               #{:foo} false
    :foo               #{}     true
    (constantly [])    nil     false
    (constantly [])    "abc"   false
    (constantly nil)   nil     true
    (constantly nil)   123     true
    (constantly false) nil     true
    (constantly false) true    true
    ))

(deftest test=a
  (are [pred expr data]
    (pred (expr data))

    map?  (=a :foo :bar) {:foo "one" :bar "one"}
    map?  (=a :a :b :c)  {:a 5 :b 5 :c 5}
    map?  (=a :a :b :c)  {}
    zero? (=a neg? pos?) 0
    nil?  (=a neg? pos?) 1
    nil?  (=a neg? pos?) -1
    nil?  (=a :a :b :c)  {:b "bbb"}
    nil?  (=a inc dec)   2

    zero? (=a 0) 0
    nil?  (=a "foo") 1
    nil?  (=a "bar") ["bar"]
    ))

(deftest test-compose-ltr
  (are [expr data expected]
    (= expected (expr data))

    (->a inc inc inc) 2 5
    (->a :parent :child) {:parent {:child "stuff"} :x "y"} "stuff"
    (->a :parent :child) {:parent {:other "stuff"}} nil
    (->a :parent :child) {} nil
    (->a :outer {:rewrap identity}) {:outer "thing"} {:rewrap "thing"}
    ))

(deftest test-compose-some-ltr
  (are [expr data expected]
    (= expected (expr data))

    (some->a inc (constantly nil) inc) 2 nil))

(deftest test-something?
  (are [pred v]
    (pred (something? v))

    true?  "one"
    true?  1
    true?  :one
    true?  [:one]
    true?  [:one :two]
    true?  {:one "one"}
    true?  #{:one}
    true?  #{:one :two}
    true?  (list "one")
    true?  (list "one" "two")
    false? nil
    false? []
    false? (list)
    false? {}
    false? #{}
    false? (filter neg? (range 5))))

(deftest test-whena
  (let [f (whena neg? (*a -1))]
    (are [v expected]
      (= expected (f v))

      -3 3
      -1 1
       0 nil
       1 nil
       3 nil
      )))

(deftest test-when-nota
  (let [f (when-nota neg? (*a -1))]
    (are [v expected]
      (= expected (f v))

      -3 nil
      -1 nil
       0 0
       1 -1
       3 -3
      )))

(deftest test-conda
  (let [f (conda neg? 0
                 odd? (*a 2)
                 pos? (diva 2)
                 0)]
    (are [v expected]
      (= expected (f v))

      -8 0
      -1 0
       0 0
       2 1
       1 2
       6 3
       )))


(deftest test-geta
  (are [m k expected]
    (= expected ((geta k) m))

    nil "foo" nil

    {} "foo" nil

    {:foo "bar"}
    "foo" nil

    {:foo "bar"}
    :foo "bar"

    {"one" 1 "two" 2}
    "one" 1
    
    {"one" 1 "two" 2}
    "three" nil
    
    {"one" 1 "two" 2}
    :one nil))

(defspec selecta-spec
  (let [keys ["one" "two" "three"]
        sf (apply selecta keys)
        k-gens (map g/return keys)]
    (for-all [m (g/map
                  (g/one-of (cons g/string-alphanumeric k-gens))
                  g/int)]
      (let [m' (sf m)]
        (is (<= (count m') (count m)))
        (is (>= 3 (count m')))
        (is (= (not-empty (select-keys m' keys)) m'))))))


#_(defspec withouta-spec
  (let [keys ["one" "two" "three"]
        wf (apply withouta keys)
        k-gens (map g/return keys)
        get-all-keys (transformer (map geta keys))]
    (for-all [m (g/map
                  (g/one-of (cons g/string-alphanumeric k-gens))
                  g/int)]
      (let [m' (wf m)]
        (is (nil? (get-all-keys m')))))))


(defspec betweena-spec 200
  (let [gen-number (g/one-of [g/int g/nat g/ratio])]
    (for-all [a gen-number
              b gen-number
              x (g/one-of [g/int g/nat g/ratio
                           g/string-alphanumeric
                           g/keyword g/boolean
                           g/any-printable])]
      (let [lower (min a b)
            upper (max a b)
            bf (betweena a b identity)
            safe-n (bf x)]
        (printf "A: %s  B: %s\n" a b)
        (is (number? safe-n))
        (is (<= lower safe-n))
        (is (>= upper safe-n))))))

(deftest test-+a
  (are [exprs value expected]
    (= expected ((apply +a exprs) value))

    [inc inc dec] 3 10
    ))

(deftest test-*a
  (are [exprs value expected]
    (= expected ((apply *a exprs) value))

    [inc dec inc] 3 32
    ))

(deftest test--a
  (are [exprs value expected]
    (= expected ((apply -a exprs) value))

    [inc dec] 9 2
    [inc dec] -2 2
    [inc dec 2] 20 0

    [dec inc] 7 -2
    ))

#_(deftest test-unary-lift
  (are [lifted value expected]
    (= expected (lifted value))

    (unary-lift inc) 2 1
    (unary-lift inc dec) 3 3
    (unary-lift 'even? (*a 2)) 5 true
    ))

(let [n 100000
      v ["abcd"]
      first (geta 0)
      ks [:ab :bc :cd :de :ef :fg :gh :hi :ij]
      base {:ab first :bc first :cd first
            :de first :ef first :fg first
            :gh first :hi first :ij first}
      always-true (transformer true)
      fn9 (transformer base)
      fn6 (transformer (dissoc base [:gh :hi :ij]))
      fn3 (transformer (select-keys base [:ab :bc :cd]))
      f9 (transformer-with-nils base)
      f6 (transformer-with-nils (dissoc base [:gh :hi :ij]))
      f3 (transformer-with-nils (select-keys base [:ab :bc :cd]))
      ]
  (are [label snippet]
       (do (println label)
           (time
             (dotimes [foo n]
               snippet))
           (println "")
           true)

       "Just true"
       true

       "Constantly true"
       always-true

       "9 keys: (hash-map ...)"
       (hash-map :ab v :bc v :cd v
                 :de v :ef v :fg v
                 :gh v :hi v :ij v)

       "9 keys: (hash-map key (f value) ...)"
       (apply hash-map
              [:ab (first v) :bc (first v) :cd (first v)
               :de (first v) :ef (first v) :fg (first v)
               :gh (first v) :hi (first v) :ij (first v)])

       "9 keys: nil-pruning applicative"
       (fn9 v)

       "9 keys: plain applicative"
       (f9 v)

       "6 keys: (hash-map ...)"
       (hash-map :ab v :bc v :cd v
                 :de v :ef v :fg v)

       "6 keys: (hash-map key (f value) ...)"
       (apply hash-map
              [:ab (first v) :bc (first v) :cd (first v)
               :de (first v) :ef (first v) :fg (first v)])

       "6 keys: nil-pruning applicative"
       (fn6 v)

       "6 keys: plain applicative"
       (f6 v)

       "3 keys: (hash-map ...)"
       (hash-map :ab v :bc v :cd v)

       "3 keys: (hash-map key (f value) ...)"
       (apply hash-map [:ab (first v) :bc (first v) :cd (first v)])

       "3 keys: nil-pruning applicative"
       (fn3 v)

       "3 keys: plain applicative"
       (f3 v)

       "9 zipmap"
       (zipmap ks (map first (repeat v)))
       ))

