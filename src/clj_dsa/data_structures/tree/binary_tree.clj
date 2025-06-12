(ns clj-dsa.data-structures.tree.binary-tree
  "Binary Tree"
  (:require
   [clj-dsa.data-structures.queue.core :as queue]))

(definterface INode
  (get_data [])
  (get_left [])
  (get_right []))

(deftype Node [^:volatile-mutable data ^:volatile-mutable left ^:volatile-mutable right]
  INode
  (get_data
    [_]
    data)
  (get_left
   [_]
   left)
  (get_right
   [_]
   right))

(defn build-tree
  "Builds a binary tree from preorder traversal with -1 representing nil nodes.
   The recursive `build` fn returns [`Node` remaining-preorder].
   Returns a `Node`."
  [pre-order-seq]
  (letfn [(build [nodes]
            (when-not (empty? nodes)
              (let [root (first nodes)
                    rest-nodes (rest nodes)]
                (if (= root -1)
                  [nil rest-nodes]
                  (let [[left rest-left] (build rest-nodes)
                        [right rest-right] (build rest-left)]
                    [(->Node root left right) rest-right])))))]
    (first (build pre-order-seq))))

(defn pre-order-traverse
  [tree]
  (letfn [(traverse [t]
            (when-let [root-data (and t (.get_data t))]
              (concat
               [root-data]
               (traverse (.get_left t))
               (traverse (.get_right t)))))]
    (traverse tree)))

(defn in-order-traverse
  [tree]
  (letfn [(traverse [t]
            (when-let [root-data (and t (.get_data t))]
              (concat
               (traverse (.get_left t))
               [root-data]
               (traverse (.get_right t)))))]
    (traverse tree)))

(defn post-order-traverse
  [tree]
  (letfn [(traverse [t]
            (when-let [root-data (and t (.get_data t))]
              (concat
               (traverse (.get_left t))
               (traverse (.get_right t))
               [root-data])))]
    (traverse tree)))

(defn- generate-levels
  [v]
  (->> v
       (partition-by nil?)
       (remove #(nil? (first %)))))

(defn level-order-traverse
  [tree]
  (let [q (queue/make-queue [tree nil])]
    (loop [acc []]
      (if (zero? (.size q))
        (generate-levels acc)
        (let [node (.front q)]
          (.dequeue q)
          (when node
            (let [l (.get_left node)
                  r (.get_right node)]
              (when l
                (.enqueue q l))
              (when r
                (.enqueue q r))
              (when (or l r)
                (.enqueue q nil))))
          (recur
           (conj acc (when node (.get_data node)))))))))

