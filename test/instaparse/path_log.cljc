(ns instaparse.path-log
  (:require
    #?(:clj  [clojure.test :refer [deftest is]]
       :cljs [cljs.test :as t])
    #?(:clj  [instaparse.core :as insta]
       :cljs [instaparse.core :as insta]))
  #?(:cljs (:require-macros
             [cljs.test :refer [is deftest]])))

(def simple-parser
  (insta/parser
    "TOP = R1+ \"\\n\"
    R1 = ( 'foo' | 'bar' | R3 )+
       | 'baz'
       | R2
    R2 = 'qux'
       | 'quux'
       | 'quuux'
    R3 = 'z'"))

(def text1
  "foo\n")

(def text2
  "barquuxz\n")

(def text3
  "quxquxquuxquuxquuuxquuux\n")

(def ord-parser
  (insta/parser
    "r1 = 'a' / 'b' / 'c'"))

(def text4
  "a")

(def text5
  "c")

(def opt-reps-parser
  (insta/parser
    "r = 'a' 'b'+ 'c'* 'd'?"))

(def text6
  "ab")

(def text7
  "abc")

(def text8
  "abccd")

(deftest simple-path-log-tests
  (let [res1 (simple-parser text1)
        path-freqs1 (->> res1 meta :path-log frequencies)
        res2 (simple-parser text2)
        path-freqs2 (->> res2 meta :path-log frequencies)
        res3 (simple-parser text3)
        path-freqs3 (->> res3 meta :path-log frequencies)

        res4 (ord-parser text4)
        path-freqs4 (->> res4 meta :path-log frequencies)
        res5 (ord-parser text5)
        path-freqs5 (->> res5 meta :path-log frequencies)

        res6 (opt-reps-parser text6)
        path-freqs6 (->> res6 meta :path-log frequencies)
        res7 (opt-reps-parser text7)
        path-freqs7 (->> res7 meta :path-log frequencies)
        res8 (opt-reps-parser text8)
        path-freqs8 (->> res8 meta :path-log frequencies)]

    (is (= res1
           [:TOP
            [:R1 "foo"]
            "\n"]))
    (is (= path-freqs1
           {[:TOP] 1,
            [:TOP :cat 0] 1,
            [:TOP :cat 0 :plus 0] 1,
            [:TOP :cat 1] 1,
            [:R1] 1,
            [:R1 :alt 0] 1,
            [:R1 :alt 0 :plus 0] 1,
            [:R1 :alt 0 :plus 0 :alt 0] 1}))

    (is (= res2
           [:TOP
            [:R1 "bar"]
            [:R1 [:R2 "quux"]]
            [:R1 [:R3 "z"]]
            "\n"]))
    (is (= path-freqs2
           {[:TOP] 1,
            [:TOP :cat 0] 3,
            [:TOP :cat 0 :plus 0] 3,
            [:TOP :cat 1] 1,
            [:R1] 3,
            [:R1 :alt 0] 2,
            [:R1 :alt 0 :plus 0] 2,
            [:R1 :alt 0 :plus 0 :alt 1] 1,
            [:R1 :alt 0 :plus 0 :alt 2] 1,
            [:R1 :alt 2] 1,
            [:R2] 1,
            [:R2 :alt 1] 1,
            [:R3] 1}))

    (is (= res3
           [:TOP
            [:R1 [:R2 "qux"]]
            [:R1 [:R2 "qux"]]
            [:R1 [:R2 "quux"]]
            [:R1 [:R2 "quux"]]
            [:R1 [:R2 "quuux"]]
            [:R1 [:R2 "quuux"]]
            "\n"]))
    (is (= path-freqs3
           {[:TOP] 1,
            [:TOP :cat 0] 6,
            [:TOP :cat 0 :plus 0] 6,
            [:TOP :cat 1] 1,
            [:R1] 6,
            [:R1 :alt 2] 6,
            [:R2] 6,
            [:R2 :alt 0] 2,
            [:R2 :alt 1] 2,
            [:R2 :alt 2] 2}))

    (is (= res4
           [:r1 "a"]))
    (is (= path-freqs4
           {[:r1] 1,
            [:r1 :ord 0] 1}))

    (is (= res5
           [:r1 "c"]))
    (is (= path-freqs5
           {[:r1] 1,
            [:r1 :ord 1] 1,
            [:r1 :ord 1 :ord 1] 1}))

    (is (= res6
           [:r "a" "b"]))
    (is (= path-freqs6
           {[:r] 1,
            [:r :cat 0] 1,
            [:r :cat 1] 1,
            [:r :cat 1 :plus 0] 1,
            [:r :cat 2] 1,
            [:r :cat 3] 1}))

    (is (= res7
           [:r "a" "b" "c"]))
    (is (= path-freqs7
           {[:r] 1,
            [:r :cat 0] 1,
            [:r :cat 1] 1,
            [:r :cat 1 :plus 0] 1,
            [:r :cat 2] 2,
            [:r :cat 2 :star 0] 1,
            [:r :cat 3] 1}))

    (is (= res8
           [:r "a" "b" "c" "c" "d"]))
    (is (= path-freqs8
           {[:r] 1,
            [:r :cat 0] 1,
            [:r :cat 1] 1,
            [:r :cat 1 :plus 0] 1,
            [:r :cat 2] 3,
            [:r :cat 2 :star 0] 2,
            [:r :cat 3] 1,
            [:r :cat 3 :opt 0] 1}))))

