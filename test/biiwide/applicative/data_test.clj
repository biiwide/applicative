;   Copyright (c) Theodore Cushman and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns biiwide.applicative.data-test
  (:require [biiwide.applicative.data :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [one-of]]
            [clojure.test.check.properties :refer [for-all]]))

(def constant-primitive-types
  (one-of [g/int g/char g/string g/ratio g/boolean]))

(def constant-collection-types
  (one-of [(g/vector constant-primitive-types)
           (g/list   constant-primitive-types)
           (g/map    constant-primitive-types constant-primitive-types)]))

(def constant-types
  (one-of [constant-primitive-types
           constant-collection-types
           (g/recursive-gen
             (fn [gitem]
               (let [gcoll (rand-nth [g/vector g/list #(g/map % %)])]
                 (gcoll gitem)))
             constant-primitive-types)]))

(defspec constantly-constant 20
  (for-all [constant-val g/any
                 arg-val      g/any]
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


(defspec optimized-constants 100
  (for-all [constant-val constant-types]
    (constant? (->transform constant-val))))

(defspec optimized-nils 100
  (for-all [empty-v (g/recursive-gen
                           (fn [t]
                             (let [collf (rand-nth [g/vector g/list #(g/map g/string %)])]
                               (collf t)))
                           (g/elements [nil]))]
    (always-nil? (->transform empty-v))))

(deftest test->transform
  (are [expr value expected]
    (= expected
      ((->transform expr) value))

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

(deftest test-and
  (are [exprs v expected]
    (= expected ((apply anda exprs) v))

    [:first :second :last]
    {:first "first" :second "second" :last "last"}
    "last"

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

(deftest test-or
  (are [exprs v expected]
    (= expected ((apply ora exprs) v))

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

(deftest test-not
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

(deftest test=
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
                 pos? (*a 1/2)
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
