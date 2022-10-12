(ns clj-dsa.data-structures.binary-search-tree.binary-search-tree)

(defrecord Node [el left right])

(defn insert-into-tree
  [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) (Node. value nil nil)
    (< value el) (Node. el (insert-into-tree left value) right)
    (> value el) (Node. el left (insert-into-tree right value))
    :else tree))

(defn remove-from-tree
  [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) nil
    (< value el) (Node. el (remove-from-tree left value) right)
    (> value el) (Node. el left (remove-from-tree right value))
    (nil? left) right
    (nil? right) left
    :else (let [min-value (min right)]
            (Node. min-value left (remove-from-tree right min-value)))))

(defn min-node-value
  [{:keys [el left]}]
  (if left
    (recur left)
    el))

(defn max-node-value
  [{:keys [el right]}]
  (if right
    (recur right)
    el))

(defn contains-node-value?
  [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) false
    (< value el) (recur left value)
    (> value el) (recur right value)
    :else true))

(defn count-nodes
  [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (count left) (count right))
    0))

(defn height
  ([tree] (height tree 0))
  ([tree count]
   (if tree
     (max (height (:left tree) (inc count))
          (height (:right tree) (inc count)))
     count)))

(defn bst?
  ([tree] (bst? tree Integer/MAX_VALUE :left))
  ([{:keys [el left right] :as tree} parent-node-value branch]
   (cond
     (nil? tree) true
     (not ((if (= :left branch) < >) el parent-node-value)) false
     :else (and (bst? left el :left)
                (bst? right el :right)))))

(def to-tree #(reduce insert-into-tree nil %))

(defn to-list [{:keys [el left right] :as tree}]
  (when tree
    `(~@(to-list left) ~el ~@(to-list right))))