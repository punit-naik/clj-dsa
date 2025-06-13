(ns clj-dsa.data-structures.tree.binary-tree.binary-search-tree
  "Binary Search Tree"
  (:require
   [clj-dsa.data-structures.tree.binary-tree :as bt]))

(definterface INode
  (get_data [])
  (set_data [d])
  (get_left [])
  (set_left [l])
  (get_right [])
  (set_right [r]))

(deftype Node [^:volatile-mutable data ^:volatile-mutable left ^:volatile-mutable right]
  INode
  (get_data
    [_]
    data)
  (set_data
    [_ d]
    (set! data d))
  (get_left
    [_]
    left)
  (set_left
    [_ l]
    (set! left l))
  (get_right
    [_]
    right)
  (set_right
    [_ r]
    (set! right r)))

(defn- insert
  [root v]
  (if-not root
    (->Node v nil nil)
    (do
      (if (< v (.get_data root))
        (.set_left root (insert (.get_left root) v))
        (.set_right root (insert (.get_right root) v)))
      root)))

(defn build-tree
  [nodes-arr]
  (reduce insert nil nodes-arr))

(defn in-order-traverse
  [tree]
  (bt/in-order-traverse tree))

(defn node-exists?
  [tree k]
  (cond
    (nil? tree) false
    (= k (.get_data tree)) true
    (< k (.get_data tree)) (node-exists? (.get_left tree) k)
    (> k (.get_data tree)) (node-exists? (.get_right tree) k)
    :else nil))

(defn- in-order-successor
  "Leftmost child of the right sub-tree"
  [root]
  (loop [root-right (.get_right root)]
    (if-not (and root-right
                 (.get_left root-right))
      root-right
      (recur (.get_left root-right)))))

(declare delete-node)

(defn- splice-in-successor!
  "In the case of both left and right children being present for the `root`"
  [root]
  (let [is (in-order-successor root)
        is-data (.get_data is)]
    (.set_data root is-data)
    (.set_right root (delete-node (.get_right root) is-data))
    root))

(defn- delete-root!
  "Sets the data and right child of the `root` tree (in case of both children existing)
   Also returns the `root` tree"
  [root]
  (if-not (.get_left root)
    (.get_right root)
    (if-not (.get_right root)
      (.get_left root)
      ;; Both left and right children are present
      (splice-in-successor! root))))

(defn delete-node
  [tree k]
  (when tree
    (let [root-data (.get_data tree)]
      (if (= root-data k)
        (delete-root! tree)
        (do
          (if (< k root-data)
            (.set_left tree (delete-node (.get_left tree) k))
            (.set_right tree (delete-node (.get_right tree) k)))
          tree)))))