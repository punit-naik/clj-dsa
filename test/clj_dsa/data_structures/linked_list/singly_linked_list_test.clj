(ns clj-dsa.data-structures.linked-list.singly-linked-list-test
  (:require
   [clojure.test :refer [deftest is]]
   [clj-dsa.data-structures.linked-list.singly-linked-list :as sll]))

(deftest test-node-creation
  (let [node (sll/->Node "data" nil)]
    (is (= "data" (.get_data node)))
    (is (= nil (.get_addr node)))))

(deftest test-set-and-get-head
  (let [sll (sll/->SLL nil nil)
        head-node (sll/->Node "head" nil)]
    (.set_head sll head-node)
    (is (= head-node (.get_head sll)))))

(deftest test-set-and-get-tail
  (let [sll (sll/->SLL nil nil)
        tail-node (sll/->Node "tail" nil)]
    (.set_tail sll tail-node)
    (is (= tail-node (.get_tail sll)))))

(deftest test-push-front
  (let [sll (sll/->SLL nil nil)]
    (.push_front sll "data1")
    (.push_front sll "data2")
    (is (= "data2" (.get_data (.get_head sll))))
    (is (= "data1" (.get_data (.get_addr (.get_head sll)))))))

(deftest test-push-back
  (let [sll (sll/->SLL nil nil)]
    (.push_back sll "data1")
    (.push_back sll "data2")
    (is (= "data2" (.get_data (.get_tail sll))))
    (is (= "data1" (.get_data (.get_head sll))))))

(deftest test-pop-front
  (let [sll (sll/->SLL nil nil)]
    (.push_back sll "head")
    (.push_back sll "tail")
    (.pop_front sll)
    (is (= "tail" (.get_data (.get_head sll))))))

(deftest test-pop-back
  (let [sll (sll/->SLL nil nil)]
    (.push_back sll "head")
    (.push_back sll "tail")
    (.pop_back sll)
    (is (= "head" (.get_data (.get_head sll))))))

(deftest test-insert
  (let [sll (sll/->SLL nil nil)]
    (.push_back sll "data1")
    (.push_back sll "data2")
    (.insert sll 1 "middle")
    (is (= "middle" (.get_data (.get_addr (.get_head sll)))))))

(deftest test-search
  (let [sll (sll/->SLL nil nil)]
    (.push_back sll "data1")
    (.push_back sll "data2")
    (is (= 0 (.search sll "data1")))
    (is (= 1 (.search sll "data2")))
    (is (= -1 (.search sll "data3")))))