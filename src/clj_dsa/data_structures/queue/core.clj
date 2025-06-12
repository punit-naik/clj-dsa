(ns clj-dsa.data-structures.queue.core
  "Queue (FIFO)"
  (:import
   [clojure.lang PersistentVector]))

(definterface IQueue
  (enqueue [v])
  (dequeue [])
  (get_data [])
  (size [])
  (front []))

(deftype Queue [^:volatile-mutable data]
  IQueue
  (enqueue
    [_ v]
    ;; Add to the end of the queue
    (set! data (conj data v)))
  (dequeue
    [this]
    ;; Remove from the front of the queue
    (when (> (.size this) 0)
      (set! data (subvec data 1))))
  (get_data
    [_]
    data)
  (size
    [_]
    (loop [d data
           s 0]
      (if (empty? d)
        s
        (recur
         (rest d)
         (inc s)))))
  (front
    [_]
    (first data)))

(defn make-queue
  [data]
  (->Queue
   (if (instance? PersistentVector data)
     data
     (apply vector data))))