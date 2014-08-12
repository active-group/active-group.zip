;   Copyright (c) Rich Hickey, Active Group GmbH. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; This is a modified version of clojure.zip that supports datatypes
; other than seqs, vectors, and XML:
;
; Instead, any type that implements the IZipNode protocol can be the
; in a zipper.

(ns ^{:doc "Functional hierarchical compositional zippers, with navigation, editing,
  and enumeration.  See Huet."
       :author "Rich Hickey, Mike Sperber"}
  active-group.zip.core
  (:refer-clojure :exclude (replace remove next)))

(defprotocol IZipNode
  (-branch? [node] "Is it possible for node to have children?")
  (-node-children [node] "Return children of this node.")
  (-make-node [node children] "Makes new node from existing node and new children."))

(extend-type #+clj Object #+cljs default IZipNode
             (-branch? [node] false)
             (-make-node [node children] node))

(defrecord ZipLoc [node path])

(defrecord Path [l pnodes ppath r changed?])

(defn- excn
  [^String msg]
  (#+clj Exception. #+cljs js/Error msg))

(defn zipper
  [root]
  (ZipLoc. root (Path. nil [] nil nil false)))

(defn node
  "Returns the node at loc"
  [loc] (:node loc))

(defn- node-branch?
  "Is node a branch?"
  [node]
  (or (seq? node)
      (vector? node)
      (-branch? node)))
  
(defn branch?
  "Is node at loc a branch?"
  [loc]
  (node-branch? (:node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a branch"
  [loc]
  (let [node (:node loc)]
    (cond
     (not (node-branch? node))
     (throw (excn "called children on a leaf node"))
     
     (seq? node) node
     (vector? node) (seq node)
     
     :else
     (-node-children node))))

(defn make-node
  "Returns a new branch node, given an existing node and new
  children."
  [node children]
  (cond
   (seq? node) children
   (vector? node) (vec children)
   :else
   (-make-node node children)))

(defn path
  "Returns a seq of nodes leading to this loc."
  [loc]
  (:pnodes (:path loc)))

(defn lefts
  "Returns a seq of the left siblings of this loc."
  [loc]
  (seq (:l (:path loc))))

(defn rights
  "Returns a seq of the right siblings of this loc"
  {:added "1.0"}
  [loc]
  (:r (:path loc)))

(defn down
  "Returns the loc of the leftmost child of the node at this loc, or
  nil if no children"
  [loc]
  (when (branch? loc)
    (let [node (:node loc)
          path (:path loc)
          [c & cnext :as cs] (children loc)]
      (when cs
        (ZipLoc. c
                 (Path. [] 
                        (if path (conj (:pnodes path) node) [node]) 
                        path 
                        cnext
                        false))))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at
  the top"
  [loc]
  (let [path (:path loc)
        pnodes (:pnodes path)
        ppath (:ppath path)]
    (when (seq pnodes)
      (let [pnode (peek pnodes)]
        (if (:changed? path)
          (ZipLoc. (make-node pnode (concat (:l path) (cons (:node loc) (:r path)))) 
                   (and ppath (assoc ppath :changed? true)))
          (ZipLoc. pnode ppath))))))

(defn top
  "Zips all the way up and returns the top loc."
  [loc]
  (if (= :end (:path loc))
    loc
    (let [p (up loc)]
      (if p
        (recur p)
        loc))))

(defn root
  "zips all the way up and returns the root node, reflecting any
 changes."
  [loc]
  (node (top loc)))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  [loc]
  (let [path (:path loc)
        [r & rnext :as rs] (:r path)]
    (when (and path rs)
      (ZipLoc. r (assoc path :l (conj (:l path) (:node loc)) :r rnext)))))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [loc]
  (let [path (:path loc)
        r (:r path)]
    (if (and path r)
      (ZipLoc. (last r) (assoc path :l (apply conj (:l path) (:node loc) (butlast r)) :r nil))
      loc)))

(defn left
  "Returns the loc of the left sibling of the node at this loc, or nil"
  [loc]
  (let [path (:path loc)
        l (:l path)]
    (ZipLoc. (peek l) (assoc path :l (pop l) :r (cons (:node loc) (:r path))))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [loc]
  (let [path (:path loc)
        l (:l path)]
    (if (and path (seq l))
      (ZipLoc. (first l) (assoc path :l [] :r (concat (rest l) [(:node loc)] (:r path))))
      loc)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc,
 without moving"
  [loc item]
  (let [path (:path loc)]
    (if (nil? (:ppath path))
      (throw (excn "Insert at top"))
      (ZipLoc. (:node loc) (assoc path :l (conj (:l path) item) :changed? true)))))
  
(defn insert-right
  "Inserts the item as the right sibling of the node at this loc,
  without moving"
  [loc item]
  (let [path (:path loc)]
    (if (nil? (:ppath path))
      (throw (excn "Insert at top"))
      (ZipLoc. (:node loc) (assoc path :r (cons item (:r path)) :changed? true)))))

(defn replace
  "Replaces the node at this loc, without moving"
  [loc node]
  (ZipLoc. node (assoc (:path loc) :changed? true)))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [loc f & args]
  (replace loc (apply f (node loc) args)))

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc,
  without moving"
  [loc item]
  (replace loc (make-node (node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc,
  without moving"
  [loc item]
  (replace loc (make-node (node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching
  the end, returns a distinguished loc detectable via end?. If already
  at the end, stays there."
  [loc]
  (if (= :end (:path loc))
    loc
    (or 
     (and (branch? loc) (down loc))
     (right loc)
     (loop [p loc]
       (if (up p)
         (or (right (up p)) (recur (up p)))
         (ZipLoc. (:node p) :end))))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. If already
  at the root, returns nil."
  [loc]
  (if-let [lloc (left loc)]
    (loop [loc lloc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    (up loc)))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [loc]
  (= :end (:path loc)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded
  it in a depth-first walk."
  [loc]
  (let [path (:path loc)
        l (:l path)
        ppath (:ppath path)]
    (if (nil? ppath)
      (throw (excn "Remove at top"))
      (if (pos? (count l))
        (loop [loc (ZipLoc. (peek l) (assoc path :l (pop l) :changed? true))]
          (if-let [child (and (branch? loc) (down loc))]
            (recur (rightmost child))
            loc))
        (ZipLoc. (make-node (peek (:pnodes path)) (:r path))
                 (and ppath (assoc ppath :changed? true)))))))
  
