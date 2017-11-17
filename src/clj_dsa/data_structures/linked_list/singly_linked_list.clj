(ns clj-dsa.data-structures.linked-list.singly-linked-list
  (:require [clojure.string :refer [join]]))

(definterface INode
  (get_data [])
  (set_data [x])
  (get_addr [])
  (set_addr [x])
  (append [d])
  (prepend [d])
  (insert_after [n d])
  (delete [n])
  (reverse [])
  (vectorise [])
  (printlist [])
  (size [])
  (get [n]))

(deftype Node [^:volatile-mutable data ^:volatile-mutable addr]
  INode
  (get_data [_] data)
  (get_addr [_] addr)
  (set_data [this x] (set! data x))
  (set_addr [this x] (set! addr x) this)

  (reverse
    [this]
    (loop [cur-list this
           new-list nil]
      (if-not cur-list
        (or new-list this)
        (let [cur-list->addr (.get_addr cur-list)]
          (recur cur-list->addr (.set_addr cur-list new-list))))))

  (vectorise
    [this]
    (loop [cur-list this
           v []]
      (if-not cur-list
        v
        (recur (.get_addr cur-list) (conj v (.get_data cur-list))))))

  (printlist
    [this]
    (println (join "->" (.vectorise this))))

  (append
    [this data]
    (.reverse (Node. data (.reverse this))))

  (prepend
    [this data]
    (Node. data this))

  (insert_after
    [this node data]
    (let [check (atom false)
          l (.reverse
              (loop [cur-list this
                     new-list nil]
                (if-not cur-list
                  new-list
                  (let [cur-list->addr (.get_addr cur-list)
                        cur-list->data (.get_data cur-list)]
                    (if (= cur-list->data node)
                      (do
                        (reset! check true)
                        (recur cur-list->addr (Node. data (.set_addr cur-list new-list))))
                      (recur cur-list->addr (.set_addr cur-list new-list)))))))]
      (if @check
        (do (println "Successfully inserted!") l)
        (do (println "Did not find the element after which the new element should be inserted!") l))))

  (delete
    [this node]
   (let [check (atom false)
         l (.reverse
             (loop [cur-list this
                    new-list nil]
               (if-not cur-list
                 new-list
                 (let [cur-list->addr (.get_addr cur-list)
                       cur-list->data (.get_data cur-list)]
                   (if (= cur-list->data node)
                     (do
                       (reset! check true)
                       (recur cur-list->addr new-list))
                     (recur cur-list->addr (.set_addr cur-list new-list)))))))]
     (if @check
       (do (println "Successfully deleted!") l)
       (do (println "Did not find the element to be deleted") l))))

  (size
    [this]
    (loop [cur-list this
           c 0]
      (if-not cur-list
        c
        (recur (.get_addr cur-list) (inc c)))))

  (get
    [this n]
    (let [c (.size this)]
      (if (> n (dec c))
        (throw (Exception. "Index out of bounds"))
        (loop [cur-list this
               m 0]
          (if (= n m)
            (.get_data cur-list)
            (recur (.get_addr cur-list) (inc m))))))))
