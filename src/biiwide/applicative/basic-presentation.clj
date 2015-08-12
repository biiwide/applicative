;; gorilla-repl.fileformat = 1

;; **
;;; # Declarative Data Transformations
;;; 
;;; ## Working with Data
;; **

;; **
;;; <div align="right" style="
;;; background-image:url('/Data-Sins-of-the-Father.png');
;;; background-position: center center;
;;; background-repeat: no-repeat;
;;; background-size: contain;
;;; background-color: #222;
;;; width: 100%;
;;; height: 100vh;
;;; padding: 0;
;;; color: #FFF;
;;; text-shadow: #AAA 0px 1px 2px;
;;; text-valign: center;
;;; font-size: 2em;
;;; font-family: sans-serif;
;;; ">
;;; <div style="
;;; position: relative;
;;; top: 50%;
;;; margin: 0 0.5em;
;;; transform: translateY(-50%);">
;;; Data is great to work with
;;; </div>
;;; </div>
;;; 
;; **

;; **
;;; <div align="center" style="
;;; background-image:url('/Data-DontKnow2.gif');
;;; background-position: center center;
;;; background-repeat: no-repeat;
;;; background-size: contain;
;;; background-color: #222;
;;; width: 100%;
;;; height: 100vh;
;;; color: #FFF;
;;; padding: 0;
;;; text-shadow: #AAA 0px 1px 2px;
;;; text-valign: center;
;;; font-size: 2em;
;;; font-family: sans-serif;
;;; ">
;;; <div style="
;;; position: relative;
;;; background-color: #222;
;;; padding: 0.2em 0;
;;; width: 100%;
;;; align: center;
;;; ">What's so great about data?</div>
;;; 
;;; <div style="
;;; position: relative;
;;; text-align: right;
;;; line-height: 1.5em;
;;; margin: 0 0.3em;
;;; top: 40%;
;;; transform: translateY(-50%);">
;;; 
;;; Declarative<br>
;;; Composable<br/>
;;; Transportable<br/>
;;; Storable<br/>
;;; ...<br/>
;;; </div>
;;; </div>
;; **

;; **
;;; <div style="
;;; background-image:url('/Data-Geordi-working.gif');
;;; background-position: center center;
;;; background-repeat: no-repeat;
;;; background-size: contain;
;;; background-color: #222;
;;; width: 100%;
;;; height: 100vh;
;;; padding: 0;
;;; color: #FFF;
;;; text-shadow: #AAA 0px 1px 2px;
;;; text-valign: center;
;;; font-size: 1.7em;
;;; font-family: sans-serif;
;;; ">
;;; <div style="
;;; background-color: #222;
;;; padding: 0.2em 0;
;;; width: 100%;
;;; text-align: center;
;;; ">Clojure has great tools for working with data</div>
;;; 
;;; <div style="
;;; float: right;
;;; position: relative;
;;; top: 55%;
;;; width: 21rem;
;;; line-height: 1.2em;
;;; text-shadow: 2px 1px #161;
;;; transform: translateY(-50%);">
;;; 
;;; <ul>
;;; <li>Persistent Collections</li>
;;; <li>List Comprehension</li>
;;; <li>Concise Representation</li>
;;; <li>Simple serialization and deserialization</li>
;;; </ul>
;;; </div>
;;; </div>
;; **

;; **
;;; <div style="
;;; width: 100%;
;;; height: 100vh;
;;; padding: 0;
;;; color: #000;
;;; text-valign: center;
;;; font-size: 1.5em;
;;; font-family: sans-serif;
;;; ">
;;; <div style="
;;; width: 100%;
;;; padding: 0.5em;">
;;; Working with data in Clojure is so convenient we want to express everything as native Clojure data structures.
;;; </div>
;;; 
;;; <div style="
;;; top: 1em;
;;; width: 100%;">
;;; <img src="/data-all-the-things.jpg"
;;; style="
;;; float: right;
;;; width: 300px;"/>
;;; 
;;; <ul style="display: block;">
;;; <li>HTML : Hiccup</li>
;;; <li>HTTP : Ring</li>
;;; <li>JSON : Cheshire</li>
;;; <li>SQL : HoneySQL</li>
;;; <li>XML : data.xml</li>
;;; </ul>
;;; 
;;; </div>
;;; </div></div>
;; **

;; **
;;; <div style="
;;; background-image:url('/Data-costume-collage.jpg');
;;; background-position: center center;
;;; background-repeat: no-repeat;
;;; background-size: contain;
;;; background-color: #222;
;;; width: 100%;
;;; height: 100vh;
;;; padding: 0;
;;; color: #FFF;
;;; text-shadow: #AAA 0px 1px 2px;
;;; text-valign: center;
;;; font-size: 1.5em;
;;; font-family: sans-serif;
;;; ">
;;; <div style="
;;; text-shadow: 2px 1px #161;
;;; position: relative;
;;; margin: 1em;
;;; line-height: 1.5em;
;;; top: 60%;
;;; transform: translateY(-50%);">
;;; Transforming data between these representations becomes a recurring theme.
;;; </div>
;;; </div>
;; **

;; @@
(ns biiwide.applicative.basic-presentation
  (:require [clojure.pprint :refer [pp]])
  (:import java.util.Date))


(defn pos-number?
  "A predicate for testing if a value is both numeric and positive."
  [x]
  (and (number? x)
       (pos? x)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/pos-number?</span>","value":"#'biiwide.applicative.basic/pos-number?"}
;; <=

;; **
;;; 
;;; -----
;;; ## Example
;;; 
;;; In these examples I'll be constructing a query for ElasticSearch.  ElasticSearch natively represents queries as JSON data.  To construct a query is to build up a data structure.
;;; 
;;; Ring provides us with a data representing an HTTP request, including query parameters.
;;; 
;;; Building a simple RESTful endpoint is then a task of transforming query string parameters into a search request, and transforming the result of the search into a response.
;;; 
;;; We will focus just on constructing the query.
;;; 
;;; For this example we will assume we are constructing a RESTful endpoint that takes the following query string parameters:
;;; 
;;;  * `first-name` & `last-name`: Strings representing a person's name.
;;;  * `before` & `after`: Instant values representing a temporal range.
;;;  * `from` & `size`: Numbers used for paging through results.
;; **

;; @@
(defn make-query-easy
  [{:keys [first-name last-name
           before after
           from size]}]

  {:query
   {:bool
    {:must [{:match {:first_name first-name}}
            {:match {:last_name last-name}}
            {:range {:timestamp {:gt after
                                 :lt before}}}]}}
   :from (if (pos-number? from)
           from
           0)
   :size (if (pos-number? size)
           size
           50)})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/make-query-easy</span>","value":"#'biiwide.applicative.basic/make-query-easy"}
;; <=

;; **
;;; This `make-query-easy` function looks easy enough.  It's reasonably concise.  It uses data literals to plug values from the query parameters into the fully populated query, so the code looks like the data it will return.   Unfortunately, it's also has bugs.  Any `nil` parameters will be populated into the data, producing an invalid query.
;; **

;; @@
(make-query-easy
  {:last-name "Random"
   :size "xyz"})

(pp)
;; @@
;; ->
;;; {:query
;;;  {:bool
;;;   {:must
;;;    [{:match {:first_name nil}}
;;;     {:match {:last_name &quot;Random&quot;}}
;;;     {:range {:timestamp {:gt nil, :lt nil}}}]}},
;;;  :from 0,
;;;  :size 50}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; To construct a usable query, the data must be conditionally assembled instead.
;; **

;; @@
(defn make-query-better
  [{:keys [first-name last-name before after from size]}]

  (let [from (if (pos-number? from)
               from
               0)
        size (if (pos-number? size)
               size
               50)]
    (cond-> {}

            (some? first-name)
            (update-in [:query :bool :must]
                       conj {:match {:first_name first-name}})

            (some? last-name)
            (update-in [:query :bool :must]
                       conj {:match {:last_name last-name}})

            (or (some? before) (some? after))
            (update-in [:query :bool :must]
                       conj {:range
                             {:timestamp
                              (cond-> {}
                                      (some? before)
                                      (assoc :lt before)
                                      (some? after)
                                      (assoc :gt after))}})

            (some? from)
            (assoc :from from)

            (some? size)
            (assoc :size size)
            )
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/make-query-better</span>","value":"#'biiwide.applicative.basic/make-query-better"}
;; <=

;; **
;;; Let's review what it's doing.  For each parameter it traverses into the data and performs an update.  The traversals can be reduced, like the handling of the `before` and `after` values, but at the cost of increased code complexity.  Most importantly the code looks nothing like the desired output.
;;; 
;;; Another alternative to all of this conditional construction would be to recursively walk the data structure, and remove nil or empty values.  This approach is also less than desirable because the data structure will need to be traversed multiple times, and the logic to prune the data may not always interact well with how the data is constructed.
;;; 
;;; Below are some samples of what these two query builders produce.
;; **

;; @@
(make-query-easy
  {:last-name "Random"
   :size "xyz"})

(pp)
(println)

(make-query-better
  {:last-name "Random"
   :size "xyz"})

(pp)
;; @@
;; ->
;;; {:query
;;;  {:bool
;;;   {:must
;;;    [{:match {:first_name nil}}
;;;     {:match {:last_name &quot;Random&quot;}}
;;;     {:range {:timestamp {:gt nil, :lt nil}}}]}},
;;;  :from 0,
;;;  :size 50}
;;; 
;;; {:size 50,
;;;  :from 0,
;;;  :query {:bool {:must ({:match {:last_name &quot;Random&quot;}})}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(make-query-easy
  {:size -10
   :from 25
   :after (Date. 115 0 3)
   :last-name "Random"})

(pp)
(println)

(make-query-better
  {:size -10
   :from 25
   :after (Date. 115 0 3)
   :last-name "Random"})

(pp)
;; @@
;; ->
;;; {:query
;;;  {:bool
;;;   {:must
;;;    [{:match {:first_name nil}}
;;;     {:match {:last_name &quot;Random&quot;}}
;;;     {:range
;;;      {:timestamp
;;;       {:gt #inst &quot;2015-01-03T06:00:00.000-00:00&quot;, :lt nil}}}]}},
;;;  :from 25,
;;;  :size 50}
;;; 
;;; {:size 50,
;;;  :from 25,
;;;  :query
;;;  {:bool
;;;   {:must
;;;    ({:range {:timestamp {:gt #inst &quot;2015-01-03T06:00:00.000-00:00&quot;}}}
;;;     {:match {:last_name &quot;Random&quot;}})}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; 
;;; Let's engage in some speculative coding.
;;; 
;;; 
;;; <img src="/Philosoraptor-data2.jpg" style="
;;; display: block;
;;; margin: auto;
;;; width: 19rem;"/>
;;; 
;;; What I would like to write:
;;; 
;;; * The code should resemble the data structure it returns.
;;; * Conditional logic should be kept to a minimum.
;;; * For this use case, `nil` values and empty collections should be pruned from the output.
;; **

;; @@
(comment
  (def build-query
    (->transform
      {:query
       {:bool
        {:must [{:match
                 {:first_name (getter :first-name)}}
                {:match
                 {:last_name  (getter :last-name)}}
                {:range
                 {:lt (getter :before)
                  :gt (getter :after)}}]}}
       :from (default  0 (pos-number? :from))
       :size (default 30 (pos-number? :size))}
      ))
  
  (build-query {:last-name "Joe" :from 30})
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; To me, this looks better, and it's my code, so clearly it must be better.
;;; 
;;; What would this magical `->transform` function look like?
;;; * It must return a function.
;;; * The function returned should be of one argument, probably a map.
;;; * It looks like it must recursively transform the data structures into functions that rebuild the data structures from the provided argument.
;;; * Functions in the data will be evaluated against the argument passed to the created function.
;;; * Some supporting higher-order-functions will be needed for handling conditional logic like default values.
;;; 
;;; In short, it's functions all the way down.  Functions that all act on and propagate the same argument.
;;; 
;;; Let's take a stab at writing this function.
;;; 
;; **

;; @@
(defn ->transform-v1
  [x]
  (cond
    ;; If x is a function, return it.
    (fn? x)      x

    ;; If x is a keyword, return it,
    ;; because keywords are functions too.
    (keyword? x) x

    ;; Otherwise, return a constant function of x
    :else        (constantly x)
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/-&gt;transform-v1</span>","value":"#'biiwide.applicative.basic/->transform-v1"}
;; <=

;; **
;;; Now let's see how it behaves.
;; **

;; @@
((->transform-v1 "foo")
 nil)

((->transform-v1 1)
 "junk")

((->transform-v1 str)
 123)

((->transform-v1 :one)
 {:one "eins" :two "zwei"})

;; Not handled yet...
((->transform-v1 [:one :two])
 {:one "eins" :two "zwei"})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:one</span>","value":":one"},{"type":"html","content":"<span class='clj-keyword'>:two</span>","value":":two"}],"value":"[:one :two]"}
;; <=

;; **
;;; So far so good.  Data goes in, a function comes out.  Now let's tackle the vector and list cases.
;;; 
;;; For both cases:
;;; * Recursively apply the same transformation to each element.
;;; * Wrap the transformations in a function to distribute it's argument to each element function.
;;; * Conj the new value onto the result when the value isn't `nil` or empty.
;;; 
;;; For Vectors:
;;; * Nothing special beyond this.
;;; 
;;; For Lists:
;;; * Reverse the incoming list because `conj` will add to the head of the list instead of the tail.
;; **

;; @@
(defn fconj-some
  [f]
  (fn [result arg]
    (if-some [v (f arg)]
      (conj result v)
      result)))


(defn accumulate
  [seed f's]
  (fn [arg]
    (not-empty
      (reduce (fn [result f] (f result arg))
              seed f's))))


(defn ->transform-v2
  [x]
  (cond
    ;; If x is a vector, recursively transform all elements
    (vector? x)  (accumulate []
                   (map (comp fconj-some ->transform-v2)
                        x))

    ;; If x is any other seq, reverse it and recursively transform all elements
    (seq? x)     (accumulate '()
                   (map (comp fconj-some ->transform-v2)
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/-&gt;transform-v2</span>","value":"#'biiwide.applicative.basic/->transform-v2"}
;; <=

;; **
;;; There are two helper functions now, `fconj-some` and `accumulate`.  Let's examine their behavior.
;; **

;; @@
((fconj-some :two)
 ["abc"]
 {:one "eins" :two "zwei"})

((fconj-some :two)
 ["abc"]
 nil)

((fconj-some inc)
 ["a"] 0)
;; @@

;; @@
((accumulate [] (map fconj-some [:one :two :three]))
 {:two "zwei" :three "drei"})

((accumulate [] (map fconj-some [:a :b :c]))
 nil)

((accumulate '() (map fconj-some [:a :b :c]))
 {:a "Apple" :c "Cactus"})

((accumulate '() (map fconj-some (reverse [:a :b :c])))
 {:a "Apple" :c "Cactus"})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Apple&quot;</span>","value":"\"Apple\""},{"type":"html","content":"<span class='clj-string'>&quot;Cactus&quot;</span>","value":"\"Cactus\""}],"value":"(\"Apple\" \"Cactus\")"}
;; <=

;; **
;;; Here's a slightly more visual comparison of `reduce` and `accumulate`:
;;; 
;;; <code>
;;; => (reduce f seed [v<sub>1</sub> v<sub>2</sub> v<sub>3</sub>])
;;; 
;;; <span style="border: 1px solid red; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f
;;; <span style="border: 1px solid green; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f
;;; <span style="border: 1px solid blue; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f seed v<sub>1</sub>)
;;; </span>
;;; v<sub>2</sub>)
;;; </span>
;;; v<sub>3</sub>)
;;; </span>
;;; </code>
;;; 
;;; <code>
;;; => (accumulate seed [f<sub>1</sub> f<sub>2</sub> f<sub>3</sub>])
;;; 
;;; <span style="border: 1px solid black; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (fn [arg]
;;; <span style="border: 1px solid red; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f<sub>3</sub>
;;; <span style="border: 1px solid green; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f<sub>2</sub>
;;; <span style="border: 1px solid blue; background-color: rgba(190,255,190,0.25); padding: 3px; display: inline-block;">
;;; (f<sub>1</sub> seed arg)
;;; </span>
;;; arg)
;;; </span>
;;; arg)
;;; </span>
;;; )
;;; </span>
;;; </code>
;;; 
;;; Now let's try out the full `->transform-v2` functionality.
;; **

;; @@
((->transform-v2 [:one :two :three])
 {:one "eins" :two "zwei"})

((->transform-v2 [inc dec pos? neg?])
 1)

((->transform-v2 [inc dec pos? neg?])
 -2)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>-1</span>","value":"-1"},{"type":"html","content":"<span class='clj-long'>-3</span>","value":"-3"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"[-1 -3 false true]"}
;; <=

;; @@
((->transform-v2 [ ["FOO" :foo]
                   ["BAR" :bar]
                   count ])
 {:foo "hello"
  :bar "world"
  :other "entry"})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;FOO&quot;</span>","value":"\"FOO\""},{"type":"html","content":"<span class='clj-string'>&quot;hello&quot;</span>","value":"\"hello\""}],"value":"[\"FOO\" \"hello\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;BAR&quot;</span>","value":"\"BAR\""},{"type":"html","content":"<span class='clj-string'>&quot;world&quot;</span>","value":"\"world\""}],"value":"[\"BAR\" \"world\"]"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[[\"FOO\" \"hello\"] [\"BAR\" \"world\"] 3]"}
;; <=

;; @@
((->transform-v2 (list
                   ["FOO" "BAR"]
                   [:foo :bar]
                   count ))
 nil)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;FOO&quot;</span>","value":"\"FOO\""},{"type":"html","content":"<span class='clj-string'>&quot;BAR&quot;</span>","value":"\"BAR\""}],"value":"[\"FOO\" \"BAR\"]"},{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}],"value":"([\"FOO\" \"BAR\"] 0)"}
;; <=

;; **
;;; The next thing to handle is maps.  Maps add a little extra complexity because we will need to assoc a key with a value.  This adds an extra argument into the process.
;; **

;; @@
(defn fassoc-some
  [k f]
  (fn [result arg]
    (if-some [v (f arg)]
      (assoc result k v)
      result)))


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
((->transform { "foo" [:foo count]
                :bar  [:bar :bar] })
 nil)

((->transform { "foo" [:foo count]
                :bar  [:bar :bar] })
 {:foo "abc"
  :bar "1234"})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:bar</span>","value":":bar"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;1234&quot;</span>","value":"\"1234\""},{"type":"html","content":"<span class='clj-string'>&quot;1234&quot;</span>","value":"\"1234\""}],"value":"[\"1234\" \"1234\"]"}],"value":"[:bar [\"1234\" \"1234\"]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;foo&quot;</span>","value":"\"foo\""},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;abc&quot;</span>","value":"\"abc\""},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"[\"abc\" 2]"}],"value":"[\"foo\" [\"abc\" 2]]"}],"value":"{:bar [\"1234\" \"1234\"], \"foo\" [\"abc\" 2]}"}
;; <=

;; **
;;; This is starting to look useful.  Let's see if it works for the original example.
;; **

;; @@
(def build-query
  (->transform
    {:query
     {:bool
      {:must [{:match {:first_name :first-name}}
              {:match {:last_name  :last-name}}
              {:range {:lt :before
                       :gt :after}}]}}
     :from :from
     :size :size}
    ))

(build-query {:first-name "Joe" :from 30})
(pp)
;; @@
;; ->
;;; {:query {:bool {:must [{:match {:first_name &quot;Joe&quot;}}]}}, :from 30}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Getting close, but not quite there yet.  Now we need some way of handling default values and predicates.
;;; More generally, it would be convenient if we could take existing functions, of potentially many arguments, and _lift_ them into this universe of composable functions of one argument.
;;; Since there's a few different common conventions for functions, there's a few different ways of transforming existing functions.
;;; 
;;; One simple type of transformation is to produce a function of functional arugments:
;;; ```
;;; => ((lift * :alice :bob)
;;;     {:alice 4 :bob 3})
;;; 12
;;; ```
;;; 
;;; Another, more specific, type would be to use a predicate to test values before propagating them:
;;; ```
;;; => ((guard string? :name)
;;;     {:name 4})
;;; nil
;;; 
;;; => ((guard string? :name)
;;;     {:name "Bill T. Cat"})
;;; "Bill T. Cat"
;;; ```
;;; 
;;; These two functional transformations should be fairly easy to write.
;; **

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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/guard</span>","value":"#'biiwide.applicative.basic/guard"}
;; <=

;; @@
((lift * :alice :bob)
 {:alice 4 :bob 3})

((lift assoc identity (constantly :count) count)
 {:alice 4 :bob 3})
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:count</span>","value":":count"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"[:count 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:bob</span>","value":":bob"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:bob 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:alice</span>","value":":alice"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[:alice 4]"}],"value":"{:count 2, :bob 3, :alice 4}"}
;; <=

;; @@
((guard string? :name)
 {:name 100})

((guard string? :name)
 {:name "Bill T. Cat"})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;Bill T. Cat&quot;</span>","value":"\"Bill T. Cat\""}
;; <=

;; **
;;; Also nice to have are actual logic functions, like `and`, `or`, and `not`, instead of just the logic macros provided by `clojure.core`.  Here are some functional definitions of `and`, `or`, & `not` that use `nil` so they can play well with what we've already written.
;;; 
;;; * `and`: If all values are truthy, return the last value, otherwise `nil`.
;;; * `or`: Return the first truthy value or `nil`.
;;; * `not`: If the value is truthy then return `nil` otherwise `true`.
;; **

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/not-f</span>","value":"#'biiwide.applicative.basic/not-f"}
;; <=

;; **
;;; Finally, to round out the functionality we can construct some simple helper functions for handling default values and testing numeric values.
;; **

;; @@
(defn default
  [v f]
  (lift or-f f v))

(def pos-number?
  (guard (every-pred number? pos?)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;biiwide.applicative.basic/pos-number?</span>","value":"#'biiwide.applicative.basic/pos-number?"}
;; <=

;; **
;;; Now we can complete the original goal.
;; **

;; @@
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

(pp)
;; @@
;; ->
;;; {:size 30,
;;;  :from 30,
;;;  :query
;;;  {:bool
;;;   {:must [{:match {:first_name &quot;Joe&quot;}} {:range {:gt &quot;2014-01-01&quot;}}]}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Fin
;; **
