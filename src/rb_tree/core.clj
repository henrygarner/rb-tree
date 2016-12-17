(ns rb-tree.core
  (:refer-clojure :exclude [assoc contains? keys vals])
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
                nil [:red nil (clojure.lang.MapEntry. k v) nil]
                [color a kv b] (cond
                                 (< k (key kv)) (balance [color (ins a) kv b])
                                 (> k (key kv)) (balance [color a kv (ins b)])
                                 :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn contains? [tree k]
  (match tree
    nil false
    [_ a kv b] (cond
                 (< k (key kv)) (recur a k)
                 (> k (key kv)) (recur b k)
                 :else true)))

(defn vals [tree]
  (match tree
    nil '()
    [_ a y b] (concat (vals a) (cons (val y) (vals b)))))

(defn keys [tree]
  (match tree
    nil '()
    [_ a y b] (concat (keys a) (cons (key y) (keys b)))))

;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
