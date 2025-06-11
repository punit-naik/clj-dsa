(ns clj-dsa.data-structures.linked-list.doubly-linked-list-test
  (:require
   [clojure.test :refer [deftest is]]
   [clj-dsa.data-structures.linked-list.doubly-linked-list :as dll]))

(deftest test-node-creation
  (let [node (dll/->Node "data" nil nil)]
    (is (= "data" (.get_data node)))
    (is (= nil (.get_next_addr node)))
    (is (= nil (.get_prev_addr node)))))

(deftest test-set-and-get-head
  (let [dll (dll/->DLL nil nil)
        head-node (dll/->Node "head" nil nil)]
    (.set_head dll head-node)
    (is (= head-node (.get_head dll)))))

(deftest test-set-and-get-tail
  (let [dll (dll/->DLL nil nil)
        tail-node (dll/->Node "tail" nil nil)]
    (.set_tail dll tail-node)
    (is (= tail-node (.get_tail dll)))))

(deftest test-push-front
  (let [dll (dll/->DLL nil nil)]
    (.push_front dll "data1")
    (.push_front dll "data2")
    (is (= "data2" (.get_data (.get_head dll))))
    (is (= "data1" (.get_data (.get_next_addr (.get_head dll)))))
    (is (= nil (.get_prev_addr (.get_head dll)))))
  (let [dll (dll/->DLL nil nil)
        head-node (dll/->Node "head" nil nil)]
    (.set_head dll head-node)
    (.push_front dll "data1")
    (is (= "data1" (.get_data (.get_head dll))))
    (is (= "head" (.get_data (.get_next_addr (.get_head dll)))))
    (is (= nil (.get_prev_addr (.get_head dll))))))

(deftest test-push-back
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "data1")
    (.push_back dll "data2")
    (is (= "data2" (.get_data (.get_tail dll))))
    (is (= "data1" (.get_data (.get_prev_addr (.get_tail dll)))))
    (is (= nil (.get_next_addr (.get_tail dll)))))
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "tail")
    (.push_back dll "data2")
    (is (= "data2" (.get_data (.get_tail dll))))
    (is (= "tail" (.get_data (.get_prev_addr (.get_tail dll)))))
    (is (= nil (.get_next_addr (.get_tail dll))))))

(deftest test-pop-front
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "head")
    (.push_back dll "tail")
    (.pop_front dll)
    (is (= "tail" (.get_data (.get_head dll))))
    (is (= nil (.get_prev_addr (.get_head dll)))))
  (let [dll (dll/->DLL nil nil)
        head-node (dll/->Node "head" nil nil)]
    (.set_head dll head-node)
    (.pop_front dll)
    (is (= nil (.get_head dll)))))

(deftest test-pop-back
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "head")
    (.push_back dll "tail")
    (.pop_back dll)
    (is (= "head" (.get_data (.get_tail dll))))
    (is (= nil (.get_next_addr (.get_tail dll)))))
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "tail")
    (.pop_back dll)
    (is (= nil (.get_tail dll)))))

(deftest test-insert
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "data1")
    (.push_back dll "data2")
    (.insert dll 1 "middle")
    (is (= "middle" (.get_data (.get_next_addr (.get_head dll)))))
    (is (= "data1" (.get_data (.get_prev_addr (.get_next_addr (.get_head dll))))))))

(deftest test-search
  (let [dll (dll/->DLL nil nil)]
    (.push_back dll "data1")
    (.push_back dll "data2")
    (is (= 0 (.search dll "data1")))
    (is (= 1 (.search dll "data2")))
    (is (= -1 (.search dll "data3")))))