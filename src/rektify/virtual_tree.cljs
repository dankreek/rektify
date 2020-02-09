(ns rektify.virtual-tree
  ;; XXX: Docs need updating pretty bad
  "
  # Virtual tree query, creation and manipulation for Rektify.

  In Rektify a virtual tree is much like the virtual DOM in React. It is itself
  a tree which represents the desired state of an object tree. Under the hood,
  a virtual tree node is a vector with 4 elements:

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

(def ^:private obj-key ::object)

(def ^:private type-index 0)
(def ^:private desc-index 1)
(def ^:private props-index 2)
(def ^:private children-index 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public


(defn object
  "Return an object virtual node, given an object descriptor and optionally
  its properties and a list of children."
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


(defn node?
  "Is `virtual-node` a valid virtual node?"
  [v-node]
  (if (vector? v-node)
    (let [type (get v-node type-index)
          desc (get v-node desc-index)
          props (get v-node props-index)
          children (get v-node children-index)]
      (and (= obj-key type)
           (map? desc)
           (or (nil? props) (map? props))
           (or (nil? children) (sequential? children))))
    false))


(defn object?
  "Does this virtual node represent an object?"
  [v-node]
  (= obj-key (get v-node type-index)))


(defn props
  "Return the virtual node's properties"
  [v-node]
  (get v-node props-index {}))


(defn type-desc
  "Return the virtual node's type description"
  [v-node]
  (get v-node desc-index))


(defn children
  "Gets the sequence of children from a virtual tree node."
  [v-node]
  (get v-node children-index))


(defn update-props
  "Returns the virtual node with its properties changed to the `new-props `."
  [v-node new-props]
  (assert (node? v-node))
  (assert (or (nil? new-props)
              (map? new-props)))
  (assoc v-node props-index new-props))


(defn update-children
  "Return a new virtual node with the same type and properties but with a new
  list of children."
  [v-node new-children]
  (assert (node? v-node))
  (assert (or (nil? new-children)
              (sequential? new-children)))
  (assoc v-node children-index new-children))


(defn node=
  "Test to see if the type and properties of each virtual graph node are the
  same. Does not verify the the child list is the same."
  [v-node-1 v-node-2]
  (and (= (get v-node-1 type-index)
          (get v-node-2 type-index))
       (= (type-desc v-node-1)
          (type-desc v-node-2))
       (= (props v-node-1)
          (props v-node-2))))


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


(defn swap-state
  [v-node f & args]
  (with-meta v-node (apply f (state v-node) args)))


(defn merge-state
  [v-node additional-state]
  (with-meta v-node (merge (state v-node) additional-state)))
