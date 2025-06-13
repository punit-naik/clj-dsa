(ns clj-dsa.data-structures.tree.binary-tree.binary-search-tree-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clj-dsa.data-structures.tree.binary-tree.binary-search-tree :as bst]))

(deftest test-build-tree
  (testing "in-order traversal after build-tree should be sorted"
    (let [tree (bst/build-tree [5 3 7 2 4 6 8])]
      (is (= (#'bst/in-order-traverse tree) [2 3 4 5 6 7 8]))))

  (testing "build-tree with empty list"
    (is (nil? (bst/build-tree [])))))

(deftest test-node-exists
  (let [tree (bst/build-tree [10 5 15 3 7 12 17])]
    (testing "existing nodes"
      (doseq [v [3 5 7 10 12 15 17]]
        (is (true? (bst/node-exists? tree v)))))
    (testing "non-existing nodes"
      (doseq [v [1 6 9 18]]
        (is (false? (bst/node-exists? tree v)))))))

(deftest test-delete-node
  (testing "delete leaf node"
    (let [tree (bst/build-tree [10 5 15])
          _ (bst/delete-node tree 5)]
      (is (= (#'bst/in-order-traverse tree) [10 15]))))

  (testing "delete node with one child"
    (let [tree (bst/build-tree [10 5 2])
          _ (bst/delete-node tree 5)]
      (is (= (#'bst/in-order-traverse tree) [2 10]))))

  (testing "delete node with two children"
    (let [tree (bst/build-tree [10 5 15 2 7])
          _ (bst/delete-node tree 5)]
      ;; 7 replaces 5
      (is (= (#'bst/in-order-traverse tree) [2 7 10 15]))))

  (testing "delete root node with two children"
    (let [tree (bst/build-tree [10 5 15 2 7])
          _ (bst/delete-node tree 10)]
      ;; 15 should remain, 10 deleted
      (is (= (#'bst/in-order-traverse tree) [2 5 7 15]))))

  (testing "delete from empty tree returns nil"
    (is (nil? (bst/delete-node nil 5)))))

(deftest test-in-order-successor
  (testing "find in-order successor (leftmost node in right subtree)"
    (let [right-subtree (bst/build-tree [20 18 25 17])
          root (bst/->Node 15 nil right-subtree)
          successor (#'bst/in-order-successor root)]
      ;; in-order successor should be 17
      (is (= (.get_data successor) 17)))))

(deftest test-delete-root!
  (testing "node with only left child"
    (let [child (bst/build-tree [3])
          node (bst/->Node 5 child nil)
          result (#'bst/delete-root! node)]
      (is (= (.get_data result) 3))))

  (testing "node with only right child"
    (let [child (bst/build-tree [7])
          node (bst/->Node 5 nil child)
          result (#'bst/delete-root! node)]
      (is (= (.get_data result) 7))))

  (testing "node with two children"
    (let [tree (bst/build-tree [5 3 8 7])
          ;; node 5 has two children (3 and 8), and 7 is in right subtree
          result (#'bst/delete-root! tree)]
      ;; the in-order successor of 5 is 7
      (is (= (.get_data result) 7))
      (is (= (#'bst/in-order-traverse result) [3 7 8])))))

(deftest test-insert-duplicates
  (let [tree (bst/build-tree [5 3 7 3 7])]
    ;; Your logic inserts duplicates to the right subtree
    (is (= (#'bst/in-order-traverse tree) [3 3 5 7 7]))))

(deftest test-delete-missing-node
  (let [tree (bst/build-tree [5 3 7])
        result (bst/delete-node tree 10)]
    ;; Tree should remain unchanged
    (is (= (#'bst/in-order-traverse result) [3 5 7]))))