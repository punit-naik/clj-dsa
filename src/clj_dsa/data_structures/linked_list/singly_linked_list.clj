(ns clj-dsa.data-structures.linked-list.singly-linked-list
  "Singly Linked List")

(definterface INode
  (set_data [d])
  (get_data [])
  (set_addr [a])
  (get_addr []))

(deftype Node [^:volatile-mutable data ^:volatile-mutable addr]
  INode
  (set_data
    [_ d]
    (set! data d))
  (get_data
    [_]
    data)
  (set_addr
    [_ a]
    (set! addr a))
  (get_addr
    [_]
    addr))

(definterface ISLL
  (set_head [h])
  (get_head [])
  (set_tail [t])
  (get_tail [])
  (print_sll [])
  (push_front [d])
  (push_back [d])
  (pop_front [])
  (pop_back [])
  (insert [i v])
  (search [v]))

(deftype SLL [^:volatile-mutable head ^:volatile-mutable tail]
  ISLL
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
  (print_sll
    [_]
    (when head
      (loop [current-head head]
        (let [current-head-data (.get_data current-head)
              current-head-address (.get_addr current-head)]
          (print (str current-head-data (when current-head-address "->")))
          (when current-head-address
            (recur current-head-address))))))
  (push_front
    [this d]
    (let [new-node (->Node d nil)]
      (if-not head
        (do
          (.set_head this new-node)
          (.set_tail this new-node))
        (do
          (.set_addr new-node head)
          (.set_head this new-node)))))
  (push_back
    [this d]
    (let [new-node (->Node d nil)]
      (if-not head
        (do
          (.set_head this new-node)
          (.set_tail this new-node))
        (do
          (.set_addr tail new-node)
          (.set_tail this new-node)))))
  (pop_front
    [this]
    (when head
      (let [next-node (.get_addr head)]
        (.set_head this next-node))))
  (pop_back
    [this]
    (when head
      (loop [current-head head]
        (if-let [current-head-address (.get_addr current-head)]
          (if (= current-head-address tail)
            (do
              (.set_addr current-head nil)
              (.set_tail this current-head))
            (recur current-head-address))
          ;; List only has one element; remove it
          (do
            (.set_head this nil)
            (.set_tail this nil))))))
  (insert
    [this i v]
    (when head
      (if (= 0 i)
        (.push_front this v)
        (loop [current-head head
               index 0]
          (if (= index (dec i))
            ;; i.e. Found the node before the new (to be inserted) node
            (let [new-node (->Node v (.get_addr current-head))]
              (.set_addr current-head new-node))
            (recur
             (.get_addr current-head)
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
             (.get_addr current-head)
             (inc index)))
          -1)))))