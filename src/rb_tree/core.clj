(ns rb-tree.core
  (:refer-clojure :exclude [assoc contains?])
  (:require [clojure.core.match :refer [match]]))

(defn balance [tree]
  (match [tree]
    [(:or [:black [:red [:red a x b] y c] z d]
          [:black [:red a x [:red b y c]] z d]
          [:black a x [:red [:red b y c] z d]]
          [:black a x [:red b y [:red c z d]]])] [:red [:black a x b]
                                                  y
                                                  [:black c z d]]
    :else tree))

(defn assoc [tree k v]
  (let [ins (fn ins [tree]
              (match tree
                nil [:red nil k nil]
                [color a y b] (cond
                                (< k y) (balance [color (ins a) y b])
                                (> k y) (balance [color a y (ins b)])
                                :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn contains? [tree k]
  (match tree
    nil false
    [_ a y b] (cond
                (< k y) (recur a k)
                (> k y) (recur b k)     
                :else true)))


;; (defn value-slice [tree min max])
;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])
;; (defn min-key [tree])
;; (defn max-key [tree])
;; (defn dissoc [tree x])
;; (defn values [tree])
;; (defn keys [tree])
