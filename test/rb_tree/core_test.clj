(ns rb-tree.core-test
  (:require [clojure.test :refer :all]
            [rb-tree.core :refer :all]
            [clojure.zip :as z]))

(defn assert-balanced [balanced]
  (are [x y] (= x (first y))
    "y"    (:value balanced)
    :red   (:color balanced)

    "x"    (-> balanced :left :value)
    :black (-> balanced :left :color)

    "a"    (-> balanced :left :left :value)
    "b"    (-> balanced :left :right :value)

    "z"    (-> balanced :right :value)
    :black (-> balanced :right :color)

    "c"    (-> balanced :right :left :value)
    "d"    (-> balanced :right :right :value))) 

(deftest red-black-trees
  (testing "Vector-based red-black trees"
    (letfn [(assert-balanced [balanced]
              (let [balanced-zp (z/zipper vector? seq (fn [_ c] c) balanced)
                    left-child  (comp z/right z/down)
                    right-child (comp z/right z/right z/right z/down)
                    value       (comp z/right z/right z/down)
                    color       z/down]
                ;; root
                (are [x y] (= x (first y))
                  :red   (-> balanced-zp color)
                  "y"    (-> balanced-zp value)

                  :black (-> balanced-zp left-child color)
                  "x"    (-> balanced-zp left-child  value)

                  "a"    (-> balanced-zp left-child left-child value)
                  "b"    (-> balanced-zp left-child right-child value)

                  :black (-> balanced-zp right-child color)
                  "z"    (-> balanced-zp right-child value)

                  "c"    (-> balanced-zp right-child left-child value)
                  "d"    (-> balanced-zp right-child right-child value))))]

      (testing "balancing, case 1"
        (let [tree [:black
                    [:red
                     [:black nil "a" nil]
                     "x"
                     [:red
                      [:black nil "b" nil]
                      "y"
                      [:black nil "c" nil]]]
                    "z"
                    [:black nil "d" nil]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 2"
        (let [tree [:black
                    [:red
                     [:red
                      [:black nil "a" nil]
                      "x"
                      [:black nil "b" nil]]
                     "y"
                     [:black nil "c" nil]]
                    "z"
                    [:black nil "d" nil]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 3"
        (let [tree [:black
                    [:black nil "a" nil]
                    "x"
                    [:red
                     [:red
                      [:black nil "b" nil]
                      "y"
                      [:black nil "c" nil]]
                     "z"
                     [:black nil "d" nil]]]]

          (assert-balanced (balance tree))))

      (testing "balancing, case 4"
        (let [tree [:black
                    [:black nil "a" nil]
                    "x"
                    [:red
                     [:black nil "b" nil]
                     "y"
                     [:red
                      [:black nil "c" nil]
                      "z"
                      [:black nil "d" nil]]]]]
          (assert-balanced (balance tree))))))

  (testing "Vector-based red-black trees: insert"
    (let [tree (-> (insert nil 10)
                   (insert 5)
                   (insert 7)
                   (insert 1)
                   (insert 14))]
      (is (= tree
             [:black
              [:black [:red nil 1 nil] 5 nil]
              7
              [:black nil 10 [:red nil 14 nil]]]))))

  (testing "Vector-based red-black trees: membership"
    (let [tree (-> (insert nil 10)
                   (insert 5)
                   (insert 7)
                   (insert 1)
                   (insert 14))]
      (are [x y] (= x y)
        true  (is-member? tree 10)
        true  (is-member? tree 7)
        true  (is-member? tree 14)
        false (is-member? tree 20)
        false (is-member? tree 2)))))