(ns clj-dsa.data-structures.linked-list.doubly-linked-list
  "Doubly Linked List")

(definterface INode
  (set_data [d])
  (get_data [])
  (set_next_addr [a])
  (get_next_addr [])
  (set_prev_addr [a])
  (get_prev_addr []))

(deftype Node [^:volatile-mutable data ^:volatile-mutable next-addr ^:volatile-mutable prev-addr]
  INode
  (set_data
    [_ d]
    (set! data d))
  (get_data
    [_]
    data)
  (set_next_addr
    [_ a]
    (set! next-addr a))
  (get_next_addr
    [_]
    next-addr)
  (set_prev_addr
   [_ a]
   (set! prev-addr a))
  (get_prev_addr
   [_]
   prev-addr))

(definterface IDLL
  (set_head [h])
  (get_head [])
  (set_tail [t])
  (get_tail [])
  (print_dll [])
  (push_front [d])
  (push_back [d])
  (pop_front [])
  (pop_back [])
  (insert [i v])
  (search [v]))

(deftype DLL [^:volatile-mutable head ^:volatile-mutable tail]
  IDLL
  (set_head
    [_ h]
    (set! head h))
  (get_head
    [_]
    head)
  (set_tail
    [_ t]
    (set! tail t))
  (get_tail
    [_]
    tail)
  (print_dll
    [_]
    (when head
      (loop [current-head head]
        (let [current-head-data (.get_data current-head)
              current-head-address (.get_next_addr current-head)]
          (print (str current-head-data (when current-head-address "->")))
          (when current-head-address
            (recur current-head-address))))))
  (push_front
    [this d]
    (let [new-node (->Node d nil nil)]
      (if-not head
        (do
          (.set_head this new-node)
          (.set_tail this new-node))
        (do
          (.set_next_addr new-node head)
          (.set_prev_addr head new-node)
          (.set_head this new-node)))))
  (push_back
    [this d]
    (let [new-node (->Node d nil nil)]
      (if-not head
        (do
          (.set_head this new-node)
          (.set_tail this new-node))
        (do
          (.set_prev_addr new-node tail)
          (.set_next_addr tail new-node)
          (.set_tail this new-node)))))
  (pop_front
    [this]
    (when head
      (let [next-node (.get_next_addr head)]
        (when next-node
          (.set_prev_addr next-node nil))
        (.set_head this next-node))))
  (pop_back
    [this]
    (when tail
      (let [new-tail (.get_prev_addr tail)]
        (when new-tail
          (.set_next_addr new-tail nil))
        (.set_tail this new-tail))))
  (insert
    [this i v]
    (when head
      (if (= 0 i)
        (.push_front this v)
        (loop [current-head head
               index 0]
          (if (= index (dec i))
            ;; i.e. Found the node before the new (to be inserted) node
            (let [new-node (->Node v (.get_next_addr current-head) current-head)]
              (.set_next_addr current-head new-node))
            (recur
             (.get_next_addr current-head)
             (inc index)))))))
  (search
    [_ v]
    (when head
      (loop [current-head head
             index 0]
        (if current-head
          (if (= (.get_data current-head) v)
            index
            (recur
             (.get_next_addr current-head)
             (inc index)))
          -1)))))