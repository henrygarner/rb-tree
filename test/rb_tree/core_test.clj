(ns rb-tree.core-test
  (:refer-clojure :exclude [assoc contains?])
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

  (testing "Vector-based red-black trees: assoc"
    (let [tree (-> (assoc nil 10 nil)
                   (assoc 5 nil)
                   (assoc 7 nil)
                   (assoc 1 nil)
                   (assoc 14 nil))]
      (is (= tree
             [:black
              [:black [:red nil [1 nil] nil] [5 nil] nil]
              [7 nil]
              [:black nil [10 nil] [:red nil [14 nil] nil]]]))))

  (testing "Vector-based red-black trees: contains?"
    (let [tree (-> (assoc nil 10 nil)
                   (assoc 5 nil)
                   (assoc 7 nil)
                   (assoc 1 nil)
                   (assoc 14 nil))]
      (are [x y] (= x y)
        true  (contains? tree 10)
        true  (contains? tree 7)
        true  (contains? tree 14)
        false (contains? tree 20)
        false (contains? tree 2))))

  (testing "keys"
    (let [tree (-> (assoc nil 10 nil)
                   (assoc 5 nil)
                   (assoc 7 nil)
                   (assoc 1 nil)
                   (assoc 14 nil))]
      (is (= (keys tree)
             '(1 5 7 10 14)))))

  (testing "vals"
    (let [tree (-> (assoc nil 10 14)
                   (assoc 5 10)
                   (assoc 7 5)
                   (assoc 1 7)
                   (assoc 14 1))]
      (is (= (vals tree)
             '(7 10 5 14 1))))))
