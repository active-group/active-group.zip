(ns active-group.zip.core-test
  (:require [clojure.test :refer :all]
            [active-group.zip.core :as zip]))

(def data '[[a * b] + [c * d]])
(def dz (zip/zipper data))

(deftest movement
  (is (= '*
         (zip/node (zip/right (zip/down (zip/right (zip/right (zip/down dz))))))))
  (is (= '(c)
         (zip/lefts (zip/right (zip/down (zip/right (zip/right (zip/down dz))))))))
  (is (= '(d)
         (zip/rights (zip/right (zip/down (zip/right (zip/right (zip/down dz))))))))
  (is (= '[[a * b] + [c * d]]
         (zip/node (zip/up (zip/up (zip/right (zip/down (zip/right (zip/right (zip/down dz))))))))))
  (is (= '[[[a * b] + [c * d]] [c * d]]
         (zip/path (zip/right (zip/down (zip/right (zip/right (zip/down dz)))))))))

(deftest basic
  (is (= '*
         (-> dz zip/down zip/right zip/right zip/down zip/right zip/node)))
  (is (= '[[a * b] + [c / d]]
         (-> dz zip/down zip/right zip/right zip/down zip/right (zip/replace '/) zip/root)))
  (is (= '[["a" * b] / [c * d]]
         (-> dz zip/next zip/next (zip/edit str) zip/next zip/next zip/next (zip/replace '/) zip/root)))
  (is (= '[[a * b] + [c *]]
         (-> dz zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/remove zip/root)))
  (is (= '[[a * b] + [c * e]]
         (-> dz zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/remove (zip/insert-right 'e) zip/root)))
  (is (= '[[a * b] + [c * e]]
         (-> dz zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/remove zip/up (zip/append-child 'e) zip/root))))

(deftest tnext
  (is (= true
         (zip/end? (-> dz zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/next zip/remove zip/next))))
  (is (= '[[c * d]]
         (-> dz zip/next zip/remove zip/next zip/remove zip/root)))
  (is (= '[[a / b] + [c / d]]
         (loop [loc dz]
           (if (zip/end? loc)
             (zip/root loc)
             (recur (zip/next (if (= '* (zip/node loc)) 
                                (zip/replace loc '/)
                                loc)))))))
  (is (= '[[a b] + [c d]]
         (loop [loc dz]
           (if (zip/end? loc)
             (zip/root loc)
             (recur (zip/next (if (= '* (zip/node loc)) 
                                (zip/remove loc)
                                loc))))))))


; Alex Miller's example from
; http://tech.puredanger.com/2010/10/22/zippers-with-records-in-clojure/

(defrecord ScalarFunction [f exprs]
  zip/IZipNode
  (-branch? [node] true)
  (-node-children [node]
    (seq (:exprs node)))
  (-make-node [node children]
    (ScalarFunction. (:f node) children)))

(defrecord Comparison [op left right]
  zip/IZipNode
  (-branch? [node] true)
  (-node-children [node]
    (seq [(:left node) (:right node)]))
  (-make-node [node children]
    (Comparison. (:op node) (first children) (second children))))

(deftest records
  (let [f1 (ScalarFunction. :+ [2 3])
        f2 (ScalarFunction. :- [6 1])
        c1 (Comparison. := f1 f2)
        tree-z (zip/zipper c1)]
    (is (= 3
           (-> tree-z zip/down zip/down zip/right zip/node)))))
