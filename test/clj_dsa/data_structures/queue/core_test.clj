(ns clj-dsa.data-structures.queue.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clj-dsa.data-structures.queue.core :as queue])
  (:import
   [clj_dsa.data_structures.queue.core IQueue]
   [clojure.lang PersistentVector]))

(deftest make-queue-tests
  (testing "make-queue with PersistentVector input returns queue with same data"
    (let [vec [1 2 3]
          q (queue/make-queue vec)]
      (is (instance? IQueue q))
      (is (= vec (.get_data q)))
      (is (instance? PersistentVector (.get_data q)))))

  (testing "make-queue with list input converts to PersistentVector"
    (let [lst '(1 2 3)
          q (queue/make-queue lst)]
      (is (instance? IQueue q))
      (is (instance? PersistentVector (.get_data q)))
      (is (= [1 2 3] (.get_data q)))))

  (testing "make-queue with empty input returns empty queue"
    (let [q (queue/make-queue [])]
      (is (instance? IQueue q))
      (is (empty? (.get_data q))))))

(deftest enqueue-tests
  (testing "enqueue adds element to the end of the queue"
    (let [q (queue/make-queue [1 2 3])
          _ (.enqueue q 4)]
      ;; returned queue is same instance (mutable)
      (is (= [1 2 3 4] (.get_data q))))))

(deftest dequeue-tests
  (testing "dequeue removes element from the front of the queue"
    (let [q (queue/make-queue [1 2 3])
          _ (.dequeue q)]
      ;; returned queue is same instance (mutable)
      (is (= [2 3] (.get_data q)))))

  (testing "dequeue on empty queue results in empty queue"
    (let [q (queue/make-queue [])
          _ (.dequeue q)]
      (is (empty? (.get_data q))))))

(deftest size-tests
  (testing "enqueue adds element to the end of the queue and finds it's size"
    (let [q (queue/make-queue [1 2 3])
          _ (.enqueue q 4)]
      ;; returned queue is same instance (mutable)
      (is (= [1 2 3 4] (.get_data q)))
      (is (= 4 (.size q))))))

(deftest front-tests
  (testing "enqueue adds element to the end of the queue and finds it's head value"
    (let [q (queue/make-queue [1 2 3])
          _ (.enqueue q 4)]
      ;; returned queue is same instance (mutable)
      (is (= [1 2 3 4] (.get_data q)))
      (is (= 1 (.front q))))))

(deftest multiple-ops-test
  (testing "multiple enqueue and dequeue calls mutate the same queue correctly"
    (let [q (queue/make-queue [])]
      (.enqueue q 1)
      (is (= [1] (.get_data q)))

      (is (= 1 (.size q)))

      (.enqueue q 2)
      (is (= [1 2] (.get_data q)))

      (is (= 1 (.front q)))

      (.dequeue q)
      (is (= [2] (.get_data q)))

      ;; dequeue again on empty queue should stay empty
      (.dequeue q)
      (is (empty? (.get_data q))))))
