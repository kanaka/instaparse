(ns instaparse.path-log
  (:require
    #?(:clj  [clojure.test :refer [deftest is testing]]
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

(def repeated-first-elem-parser
  (insta/parser
    "foo = '0' | '0' ' '; "))



(deftest path-log-tests
  (testing "simple-parser"
    (let [res1 (simple-parser text1)
          path1 (->> res1 meta :path-log)
          res2 (simple-parser text2)
          path2 (->> res2 meta :path-log)
          res3 (simple-parser text3)
          path-freqs3 (->> res3 meta :path-log frequencies)]

      (testing "ordering"
        (is (= [:TOP
                [:R1 "foo"]
                "\n"]
               res1))
        (is (= [[:TOP]
                [:TOP :cat 0]
                [:TOP :cat 0 :plus 0]
                [:R1]
                [:R1 :alt 0]
                [:R1 :alt 0 :plus 0]
                [:R1 :alt 0 :plus 0 :alt 0]
                [:TOP :cat 1]]
               path1))

        (is (= [:TOP
                [:R1 "bar"]
                [:R1 [:R2 "quux"]]
                [:R1 [:R3 "z"]]
                "\n"]
               res2))
        (is (= [[:TOP]
                [:TOP :cat 0]
                [:TOP :cat 0 :plus 0]
                [:R1]
                [:R1 :alt 0]
                [:R1 :alt 0 :plus 0]
                [:R1 :alt 0 :plus 0 :alt 1]
                [:TOP :cat 0 :plus 0]
                [:R1]
                [:R1 :alt 2]
                [:R2]
                [:R2 :alt 1]
                [:TOP :cat 0 :plus 0]
                [:R1]
                [:R1 :alt 0]
                [:R1 :alt 0 :plus 0]
                [:R1 :alt 0 :plus 0 :alt 2]
                [:R3]
                [:TOP :cat 1]]
               path2)))

      (testing "frequencies"
        (is (= [:TOP
                [:R1 [:R2 "qux"]]
                [:R1 [:R2 "qux"]]
                [:R1 [:R2 "quux"]]
                [:R1 [:R2 "quux"]]
                [:R1 [:R2 "quuux"]]
                [:R1 [:R2 "quuux"]]
                "\n"]
               res3))
        (is (= {[:TOP] 1,
                [:TOP :cat 0] 1,
                [:TOP :cat 0 :plus 0] 6,
                [:TOP :cat 1] 1,
                [:R1] 6,
                [:R1 :alt 2] 6,
                [:R2] 6,
                [:R2 :alt 0] 2,
                [:R2 :alt 1] 2,
                [:R2 :alt 2] 2}
               path-freqs3)))))

  (testing "ord-parser"
    (let [res4 (ord-parser text4)
          path4 (->> res4 meta :path-log)
          res5 (ord-parser text5)
          path5 (->> res5 meta :path-log)]
      (is (= [:r1 "a"]
             res4))
      (is (= [[:r1]
              [:r1 :ord 0]]
             path4))

      (is (= [:r1 "c"]
             res5))
      (is (= [[:r1]
              [:r1 :ord 1]
              [:r1 :ord 1 :ord 1]]
             path5))))

  (testing "opt-reps-parser"
    (let [res6 (opt-reps-parser text6)
          path6 (->> res6 meta :path-log)
          res7 (opt-reps-parser text7)
          path7 (->> res7 meta :path-log)
          res8 (opt-reps-parser text8)
          path8 (->> res8 meta :path-log)]
      (is (= [:r "a" "b"]
             res6))
      (is (= [[:r]
              [:r :cat 0]
              [:r :cat 1]
              [:r :cat 1 :plus 0]
              [:r :cat 2]
              [:r :cat 3]]
             path6))

      (is (= [:r "a" "b" "c"]
             res7))
      (is (= [[:r]
              [:r :cat 0]
              [:r :cat 1]
              [:r :cat 1 :plus 0]
              [:r :cat 2]
              [:r :cat 2 :star 0]
              [:r :cat 3]]
             path7))

      (is (= [:r "a" "b" "c" "c" "d"]
             res8))
      (is (= [[:r]
              [:r :cat 0]
              [:r :cat 1]
              [:r :cat 1 :plus 0]
              [:r :cat 2]
              [:r :cat 2 :star 0]
              [:r :cat 2 :star 0]
              [:r :cat 3]
              [:r :cat 3 :opt 0]]
             path8))))

  (testing "repeated-first-elem-parser"
    (let [res1 (repeated-first-elem-parser "0")
          path1 (->> res1 meta :path-log)]
      (is (= [[:foo]
              [:foo :alt 0]]
             path1)))))
