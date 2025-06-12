(ns clj-dsa.data-structures.stack.core
  "Stack (LIFO)"
  (:import
   [clojure.lang PersistentList]))

(definterface IStack
  (push [v])
  (pop [])
  (get_data []))

(deftype Stack [^:volatile-mutable data]
  IStack
  (push
    [this v]
    (set! data (conj data v))
    this)
  (pop
    [this]
    (set! data (rest data))
    this)
  (get_data
    [_]
    data))

(defn make-stack
  [data]
  (->Stack
   (if (instance? PersistentList data)
     data
     (apply list data))))