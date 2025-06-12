(ns clj-dsa.data-structures.stack.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clj-dsa.data-structures.stack.core :as stack])
  (:import
   [clj_dsa.data_structures.stack.core IStack]
   [clojure.lang PersistentList]))

(deftest make-stack-tests
  (testing "stack/make-stack with PersistentList input returns stack with same data"
    (let [lst (list 1 2 3)
          stack (stack/make-stack lst)]
      (is (instance? IStack stack))
      (is (= lst (.get_data stack)))
      (is (instance? PersistentList (.get_data stack)))))

  (testing "stack/make-stack with vector input converts to PersistentList"
    (let [v [1 2 3]
          stack (stack/make-stack v)]
      (is (instance? IStack stack))
      (is (instance? PersistentList (.get_data stack)))
      (is (= '(1 2 3) (.get_data stack)))))

  (testing "stack/make-stack with no input returns empty stack"
    (let [stack (stack/make-stack [])]
      (is (instance? IStack stack))
      (is (empty? (.get_data stack))))))

(deftest push-tests
  (testing "push mutates stack by adding element to the front"
    (let [stack (stack/make-stack [1 2 3])
          returned-stack (.push stack 4)]
      ;; returned stack is same instance (mutable)
      (is (identical? stack returned-stack))
      (is (= '(4 1 2 3) (.get_data stack))))))

(deftest pop-tests
  (testing "pop mutates stack by removing first element"
    (let [stack (stack/make-stack [1 2 3])
          returned-stack (.pop stack)]
      ;; returned stack is same instance (mutable)
      (is (identical? stack returned-stack))
      (is (= '(2 3) (.get_data stack)))))

  (testing "pop on empty stack results in empty stack"
    (let [stack (stack/make-stack [])
          returned-stack (.pop stack)]
      (is (identical? stack returned-stack))
      (is (empty? (.get_data stack))))))

(deftest multiple-ops-test
  (testing "multiple push and pop calls mutate the same stack correctly"
    (let [stack (stack/make-stack [])]
      (.push stack 1)
      (is (= '(1) (.get_data stack)))

      (.push stack 2)
      (is (= '(2 1) (.get_data stack)))

      (.pop stack)
      (is (= '(1) (.get_data stack)))

      (.pop stack)
      (is (empty? (.get_data stack)))

      ;; pop again on empty should stay empty
      (.pop stack)
      (is (empty? (.get_data stack))))))

