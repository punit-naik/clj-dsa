(ns clj-dsa.data-structures.linked-list.circular-linked-list-test
  (:require
   [clojure.test :refer [deftest is]]
   [clj-dsa.data-structures.linked-list.circular-linked-list :as cll]))

(deftest test-node-creation
  (let [node (cll/->Node "data" nil)]
    (is (= "data" (.get_data node)))
    (is (= nil (.get_addr node)))))

(deftest test-set-and-get-head
  (let [cll (cll/->CLL nil nil)
        head-node (cll/->Node "head" nil)]
    (.set_head cll head-node)
    (is (= head-node (.get_head cll)))))

(deftest test-set-and-get-tail
  (let [cll (cll/->CLL nil nil)
        tail-node (cll/->Node "tail" nil)]
    (.set_tail cll tail-node)
    (is (= tail-node (.get_tail cll)))))

(deftest test-push-front
  (let [cll (cll/->CLL nil nil)]
    (.push_front cll "data1")
    (.push_front cll "data2")
    (is (= "data2" (.get_data (.get_head cll))))
    (is (= "data1" (.get_data (.get_addr (.get_head cll)))))))

(deftest test-push-back
  (let [cll (cll/->CLL nil nil)]
    (.push_back cll "data1")
    (.push_back cll "data2")
    (is (= "data2" (.get_data (.get_tail cll))))
    (is (= "data1" (.get_data (.get_head cll))))))

(deftest test-pop-front
  (let [cll (cll/->CLL nil nil)]
    (.push_back cll "head")
    (.push_back cll "tail")
    (.pop_front cll)
    (is (= "tail" (.get_data (.get_head cll))))))

(deftest test-pop-back
  (let [cll (cll/->CLL nil nil)]
    (.push_back cll "head")
    (.push_back cll "tail")
    (.pop_back cll)
    (is (= "head" (.get_data (.get_head cll))))))

(deftest test-insert
  (let [cll (cll/->CLL nil nil)]
    (.push_back cll "data1")
    (.push_back cll "data2")
    (.insert cll 1 "middle")
    (is (= "middle" (.get_data (.get_addr (.get_head cll)))))))

(deftest test-search
  (let [cll (cll/->CLL nil nil)]
    (.push_back cll "data1")
    (.push_back cll "data2")
    (is (= 0 (.search cll "data1")))
    (is (= 1 (.search cll "data2")))
    (is (= -1 (.search cll "data3")))))