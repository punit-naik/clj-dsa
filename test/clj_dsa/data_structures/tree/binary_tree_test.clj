(ns clj-dsa.data-structures.tree.binary-tree-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clj-dsa.data-structures.tree.binary-tree :as bt]))

(deftest build-tree-tests
  (testing "build-tree with a complete binary tree"
    (let [input [1 2 -1 -1 3 -1 -1]
          tree (bt/build-tree input)]
      (is (= 1 (.get_data tree)))
      (is (= 2 (.get_data (.get_left tree))))
      (is (= 3 (.get_data (.get_right tree))))))

  (testing "build-tree with only root"
    (let [input [10 -1 -1]
          tree (bt/build-tree input)]
      (is (= 10 (.get_data tree)))
      (is (nil? (.get_left tree)))
      (is (nil? (.get_right tree)))))

  (testing "build-tree with empty input"
    (let [input []
          tree (bt/build-tree input)]
      (is (nil? tree))))

  (testing "build-tree with all -1s (null root)"
    (let [input [-1]
          tree (bt/build-tree input)]
      (is (nil? tree)))))

(deftest pre-order-traverse-tests
  (testing "pre-order traversal"
    (let [tree (bt/build-tree [1 2 -1 -1 3 -1 -1])]
      (is (= [1 2 3] (bt/pre-order-traverse tree)))))

  (testing "pre-order of single-node tree"
    (let [tree (bt/build-tree [42 -1 -1])]
      (is (= [42] (bt/pre-order-traverse tree)))))

  (testing "pre-order of empty tree"
    (is (nil? (bt/pre-order-traverse nil)))))

(deftest in-order-traverse-tests
  (testing "in-order traversal"
    (let [tree (bt/build-tree [1 2 -1 -1 3 -1 -1])]
      (is (= [2 1 3] (bt/in-order-traverse tree)))))

  (testing "in-order of single-node tree"
    (let [tree (bt/build-tree [99 -1 -1])]
      (is (= [99] (bt/in-order-traverse tree)))))

  (testing "in-order of empty tree"
    (is (nil? (bt/in-order-traverse nil)))))

(deftest post-order-traverse-tests
  (testing "post-order traversal"
    (let [tree (bt/build-tree [1 2 -1 -1 3 -1 -1])]
      (is (= [2 3 1] (bt/post-order-traverse tree)))))

  (testing "post-order of single-node tree"
    (let [tree (bt/build-tree [7 -1 -1])]
      (is (= [7] (bt/post-order-traverse tree)))))

  (testing "post-order of empty tree"
    (is (nil? (bt/post-order-traverse nil)))))

(deftest level-order-traverse-tests
  (testing "level-order traversal with levels"
    (let [tree (bt/build-tree [1 2 -1 -1 3 -1 -1])]
      (is (= [[1] [2 3]] (bt/level-order-traverse tree)))))

  (testing "level-order with single node"
    (let [tree (bt/build-tree [10 -1 -1])]
      (is (= [[10]] (bt/level-order-traverse tree)))))

  (testing "level-order of empty tree"
    (is (= [] (bt/level-order-traverse nil)))))