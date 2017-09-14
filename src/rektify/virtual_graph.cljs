(ns rektify.virtual-graph
  "Virtual query and manipulation")

(def generator-key ::generator)
(def object-key ::object)

(defn props
  [virtual-node]
  (nth virtual-node 2 {}))


(defn update-props
  "Returns the virtual node with its properties changed to the `new-props `."
  [virtual-node new-props]
  (assoc virtual-node 2 new-props))


(defn update-children
  [virtual-node new-children]
  (assoc virtual-node 3 new-children))


(defn type-desc
  [virtual-node]
  (second virtual-node))


(defn children
  "Gets the sequence of children from a virtual graph node."
  [virtual-graph]
  (nth virtual-graph 3 nil))


(defn node=
  "Test to see if the type and properties of each virtual graph are the same"
  [v-graph-1 v-graph-2]
  (and (= (type-desc v-graph-1)
          (type-desc v-graph-2))
       (= (props v-graph-1)
          (props v-graph-2))))


(defn object?
  "Does this virtual node represent an object?"
  [virtual-node]
  (= object-key (first virtual-node)))


(defn generator?
  "Does this virtual node represent a generator?"
  [virtual-node]
  (= generator-key (first virtual-node)))


(defn pp
  "Return a human readable string representation of the provided virtual graph"
  [v-graph]
  (let [const (:constructor (type-desc v-graph))]
    [(first v-graph)
     (when const (.-name const))
     (props v-graph)
     (mapv pp (children v-graph))]))


(defn valid?
  "Is the provided `virtual-graph` a valid virtual graph?"
  [virtual-graph]
  (and (vector? virtual-graph)
       (let [node-type (first virtual-graph)
             type-desc (second virtual-graph)
             props (nth virtual-graph 2 nil)
             children (nth virtual-graph 3 nil)]
         (and (keyword? node-type)
              (map? type-desc)
              (map? props)
              (or (nil? children) (seq? children))))))

