(ns rektify.virtual-tree
  "
  # Virtual tree creation and manipulation for Rektify.

  In Rektify a virtual tree is much like the virtual DOM in React. It is itself
  a tree which represents the desired state of an object tree. Under the hood,
  a virtual tree node is a map with 4 keys:

  ```clojure
  {:rektify/type ::v-node
   ::desc {}
   ::props {}
   ::children []}
  ```

  * `type` The keyword `::v-node` which is used to indicate this is a valid
    virtual tree node. Used to distinguish a map representing a virtual tree
    node from a map representing a generator.

  * `desc` A map which describes the object represented by the virtual node.
    See the `rektfy.object` namespaces for a further explanation of what this
    map contains.

  * `props` A map of this tree node's properties which are set on the node's
    object.

  * `children` A sequence of this node's children. Each child is also a valid
    virtual node.

  A virtual node may also contain state stored as a map in the map's metadata.

  When a node has been reified its state map will contain the key `::obj&`
  which is a reference to the JavaScript object instance that's represented
  by the virtual node.
  "
  (:refer-clojure :exclude [object?])
  (:require [rektify.object :as o]
            [clojure.zip :as z]))


(defn node
  "Return a virtual node, given an object descriptor and optionally
  its properties and a list of children."
  ([obj-desc]
    (node obj-desc {}))
  ([obj-desc props]
    (node obj-desc props []))
  ([obj-desc props children]
   (assert (map? obj-desc)
           "obj-desc must be a map")
   (assert (map? props)
           "props must be a map")
   (assert (sequential? children)
           "children must be sequential")
   {:rektify/type ::v-node
    ::desc obj-desc
    ::props props
    ::children children}))

(defn node?
  "Is a valid virtual tree node?"
  [v-node]
  (if (map? v-node)
    (and (= ::v-node (:rektify/type v-node))
         (map? (::desc v-node))
         (map? (::props v-node))
         (sequential? (::children v-node)))
    false))


(defn props
  "Return the virtual node's properties"
  [v-node]
  (::props v-node))


(defn desc
  "Return the virtual node's type description"
  [v-node]
  (::desc v-node))


(defn children
  "Gets the sequence of children from a virtual tree node."
  [v-node]
  (::children v-node))


(defn update-props
  "Returns the virtual node with its properties changed to the `new-props `."
  [v-node new-props]
  (assert (node? v-node))
  (assert (map? new-props))
  (assoc v-node ::props new-props))


(defn update-children
  "Return a new virtual node with the same type and properties but with a new
  list of children."
  [v-node new-children]
  (assert (node? v-node))
  (assert (sequential? new-children))
  (assoc v-node ::children new-children))


(defn node=
  "Test to see if the type and properties of each virtual graph node are the
  same. Does not verify the the child list is the same."
  [v-node-1 v-node-2]
  (let [node-keys [::desc ::props]]
    (= (select-keys v-node-1 node-keys)
       (select-keys v-node-2 node-keys))))


(defn with-state
  "Return a new virtual node with the provided state map."
  [v-node state]
  (assert (or (map? state) (nil? state))
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


(defn same-types?
  "Are the v-nodes the same type?"
  [v-node-1 v-node-2]
  (= (::desc v-node-1) (::desc v-node-2)))


(defn reified? [v-tree]
  "Is the v-tree reified?"
  (contains? (state v-tree) ::obj&))


(defn v-tree-zip
  "Create a zipper over a tree of virtual nodes"
  [head-v-node]
  (z/zipper node? children update-children head-v-node))


(defn obj& [v-node]
  "Reified object from provided reified v-node"
  (::obj& (state v-node)))


(defn ^:private attach-obj&-to-v-node [v-node node-obj&]
  (merge-state v-node {::obj& node-obj&}))


(defn destroy-node!
  "Destroy the v-node's object by calling its destructor, which should also
  destroy all of the object's children. If a parent is provided the child is
  first removed from the parent before it is destroyed.

  If v-tree is un-reified node then this function will do nothing. If the parent
  if un-reified then no child remove operation will be performed."
  ([v-tree] (destroy-node! v-tree nil))
  ([v-tree v-tree-parent]
   (assert (node? v-tree) "v-tree is a virtual tree node")
   (assert (or (nil? v-tree-parent) (node? v-tree-parent))
           "v-tree-parent is a virtual tree node")

   ;; no need to destroy an un-reified node
   (when (reified? v-tree)
     ;; Disconnect from parent before destroying
     (when (and v-tree-parent (reified? v-tree-parent))
       (o/remove-child!
         (desc v-tree-parent) (obj& v-tree-parent) (obj& v-tree)))

     (o/destroy! (desc v-tree) (obj& v-tree)))

   nil))


(defn reify-v-node
  "Reify the v-node and return it with the new object in the metadata. If a
  parent v-node is provided, the created object will be added to the parent's
  object."
  ([v-node] (reify-v-node v-node nil))
  ([v-node v-node-parent]
   (assert (not (reified? v-node)) "the provided v-node has already been reified")
   (assert (or (nil? v-node-parent) (reified? v-node-parent)) "v-node parent must be reified")
   (let [new-obj& (o/construct-obj! (desc v-node) (props v-node))
         reified-v-node (attach-obj&-to-v-node v-node new-obj&)]
     ;; Add to parent if parent provided
     (when v-node-parent
       (o/add-child! (desc v-node-parent) (obj& v-node-parent) new-obj&))
     reified-v-node)))


(defn reify-v-tree
  "Create the object tree described by the v-tree and return the v-tree with
  the object tree in the metadata. The children of the v-tree will be
  added recursively."
  ([v-tree] (reify-v-tree v-tree nil))
  ([v-tree v-tree-parent]
   (let [reified-v-node (reify-v-node v-tree v-tree-parent)]
     (update-children
       reified-v-node
       (mapv #(reify-v-tree % reified-v-node) (children v-tree))))))


(defn ^:private rektify-v-tree*
  ([new-v-tree reified-v-tree] (rektify-v-tree* new-v-tree reified-v-tree nil))
  ([new-v-tree reified-v-tree reified-v-tree-parent]
   (assert (or (nil? reified-v-tree) (reified? reified-v-tree))
           "provided reified-v-tree is either reified or nil")
   (assert (or (nil? new-v-tree) (not (reified? new-v-tree)))
           "provided new-v-tree is either not reified or nil")

   (cond
     ;; No changes, return the entire reified v-tree
     (= new-v-tree reified-v-tree)
     reified-v-tree

     ;; Reify new tree where a previous tree did not exist
     (nil? reified-v-tree)
     (reify-v-tree new-v-tree reified-v-tree-parent)

     ;; Destroy reified tree since this node doesn't exists in the new-v-tree
     (nil? new-v-tree)
     (destroy-node! reified-v-tree reified-v-tree-parent)

     ;; Node is same type so update properties and recurse children
     (same-types? new-v-tree reified-v-tree)
     (let [new-reified-v-tree (with-state new-v-tree (state reified-v-tree))
           new-v-tree-children (children new-v-tree)
           reified-v-tree-children (children reified-v-tree)
           max-child-count (max (count new-v-tree-children)
                                (count reified-v-tree-children))]
       (o/update-props! (desc reified-v-tree)
                        (obj& reified-v-tree)
                        (props new-v-tree)
                        (props reified-v-tree))
       (update-children
         new-reified-v-tree
         (if (=  new-v-tree-children reified-v-tree-children)
           ;; If the children are the same just return the previously reified children
           reified-v-tree-children
           ;; Otherwise recurse over each child
           (->> (map #(rektify-v-tree* (nth new-v-tree-children %1 nil)
                                       (nth reified-v-tree-children %1 nil)
                                       new-reified-v-tree)
                     (range max-child-count))
                (remove nil?)
                (vec)))))

     ;; New tree's head node is a different type than the old tree so destroy
     ;; old tree and reify a new one
     (not (same-types? new-v-tree reified-v-tree))
     (let [new-reified-v-tree (reify-v-tree new-v-tree)]
       (when reified-v-tree-parent
         (o/replace-child! (desc reified-v-tree-parent)
                           (obj& reified-v-tree-parent)
                           (obj& reified-v-tree)
                           (obj& new-reified-v-tree)))
       (destroy-node! reified-v-tree)
       new-reified-v-tree)

     :default
     (throw (js/Error. "No method found for rektifying v-tree.This should not happen.")))))


(defn rektify
  "Given a new v-tree and optionally, a previously reified v-tree, update all
  objects in the reified v-tree to match the new v-tree and return the
  new v-tree with the object tree in the metadata."
  ([new-v-tree]
   (reify-v-tree new-v-tree))
  ([new-v-tree reified-v-tree]
   (if (nil? reified-v-tree)
     (reify-v-tree new-v-tree)
     (rektify-v-tree* new-v-tree reified-v-tree))))
