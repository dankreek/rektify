(ns rektify.virtual-graph
  "
  # Virtual graph query, creation and manipulation for Rektify.

  In Rektify a virtual graph is much like the virtual DOM in React. It is itself
  a graph which represents the desired state of an object tree. Under the hood,
  a virtual graph node is a vector with 4 elements:

  ```clojure
  [type-key description properties children]
  ```

  * `type-key` A keyword that's either `::generator` or `::object` signifying
    that this virtual node represents either a generator or an object.

  * `description` A map which describes the node. If this is an object node then
    an `obj-desc` will be here, otherwise it will be a `gen-desc`. See the
    `rektfy.object` and `rektify.generator` namespaces for a further
    explanation on these.

  * `properties` A map of properties unique to the node instance.

  * `children` A sequence of this node's children. If there aren't any children
    will be either an empty sequence or `nil`. Each child is also a valid
    virtual node.

  Node's may cary state. If so, the state will be stored in the enclosing
  vector's metadata.
  "
  (:refer-clojure :exclude [object?]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def gen-key ::generator)
(def obj-key ::object)


(def type-index 0)
(def desc-index 1)
(def props-index 2)
(def children-index 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public


(defn object
  "Return an object virtual node"
  ([obj-desc]
    (object obj-desc nil))
  ([obj-desc props]
    (object obj-desc props nil))
  ([obj-desc props children]
   (assert (map? obj-desc)
           "obj-desc must be a map")
   (assert (or (nil? props) (map? props))
           "props must be a map or nil")
   (assert (or (nil? children) (sequential? children))
           "children must be sequential or nil")
   [obj-key obj-desc props children]))


(defn generator
  "Return a generator virtual node"
  ([gen-desc]
    (generator gen-desc nil))
  ([gen-desc props]
    (generator gen-desc props nil))
  ([gen-desc props children]
   (assert (map? gen-desc)
           "gen-desc must be a map")
   (assert (or (nil? props) (map? props))
           "props must be a map or nil")
   (assert (or (nil? children) (sequential? children))
           "children must be sequential or nil")
   [gen-key gen-desc props children]))


(defn node?
  "Is `virtual-node` a valid virtual node?"
  [virtual-node]
  (if (vector? virtual-node)
    (let [type (get virtual-node type-index)
          desc (get virtual-node desc-index)
          props (get virtual-node props-index)
          children (get virtual-node children-index)]
      (and (or (= obj-key type) (= gen-key type))
           (map? desc)
           (or (nil? props) (map? props))
           (or (nil? children) (sequential? children))))
    false))


(defn object?
  "Does this virtual node represent an object?"
  [virtual-node]
  (= obj-key (get virtual-node type-index)))


(defn generator?
  "Does this virtual node represent a generator?"
  [virtual-node]
  (= gen-key (get virtual-node type-index)))


(defn props
  "Return the virtual node's properties"
  [virtual-node]
  (get virtual-node props-index {}))


(defn type-desc
  "Return the virtual node's type description"
  [virtual-node]
  (get virtual-node desc-index))


(defn children
  "Gets the sequence of children from a virtual graph node."
  [virtual-graph]
  (get virtual-graph children-index))


(defn update-props
  "Returns the virtual node with its properties changed to the `new-props `."
  [virtual-node new-props]
  (assert (node? virtual-node))
  (assert (or (nil? new-props)
              (map? new-props)))
  (assoc virtual-node props-index new-props))


(defn update-children
  "Return a new virtual node with the same type and properties but with a new
  list of children."
  [virtual-node new-children]
  (assert (node? virtual-node))
  (assert (or (nil? new-children)
              (sequential? new-children)))
  (assoc virtual-node children-index new-children))


(defn node=
  "Test to see if the type and properties of each virtual graph node are the
  same. Does not verify the the child list is the same."
  [v-graph-1 v-graph-2]
  (and (= (get v-graph-1 type-index)
          (get v-graph-2 type-index))
       (= (type-desc v-graph-1)
          (type-desc v-graph-2))
       (= (props v-graph-1)
          (props v-graph-2))))


(defn with-state
  "Return a new virtual node with the provided state map."
  [v-node state]
  (assert (or (map? state)
              (nil? state))
          "A virtual node's state must be either a map or nil")
  (with-meta v-node state))


(defn state
  [v-node]
  "Return the virtual node's state map."
  (meta v-node))


