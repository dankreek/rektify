(ns rektify.rektify
  (:require [rektify.object :as o]
            [rektify.virtual-tree :as vt]))


(defn reified? [v-tree]
  "Is the v-tree reified?"
  (contains? (vt/state v-tree) ::obj&))


(defn obj& [v-node]
  "Reified object from provided reified v-node"
  (::obj& (vt/state v-node)))


(defn destroy-v-node!
  "Destroy the v-node's object by calling its destructor, which should also
  destroy all of the object's children. If a parent is provided the child is
  first removed from the parent before it is destroyed.

  If v-tree is un-reified node then this function will do nothing. If the parent
  if un-reified then no child remove operation will be performed."
  ([v-tree] (destroy-v-node! v-tree nil))
  ([v-tree v-tree-parent]
   (assert (vt/node? v-tree) "v-tree is a virtual tree node")
   (assert (or (nil? v-tree-parent) (vt/node? v-tree-parent)) "v-tree-parent is a virtual tree node")

   ;; no need to destroy an un-reified node
   (when (reified? v-tree)
     ;; Disconnect from parent before destroying
     (when (and v-tree-parent (reified? v-tree-parent))
       (o/remove-child!
         (vt/type-desc v-tree-parent) (obj& v-tree-parent) (obj& v-tree)))

     (o/destroy! (vt/type-desc v-tree) (obj& v-tree)))

   nil))


(defn reify-v-node
  "Reify the v-node and return it with the new object in the metadata. If a
  parent v-node is provided, the created object will be added to the parent's
  object."
  ([v-node] (reify-v-node v-node nil))
  ([v-node v-node-parent]
   (assert (not (reified? v-node)) "the provided v-node has already been reified")
   (assert (or (nil? v-node-parent) (reified? v-node-parent)) "v-node parent must be reified")
   (let [new-obj& (o/construct-obj! (vt/type-desc v-node) (vt/props v-node))
         reified-v-node (vt/merge-state v-node {::obj& new-obj&})]
     ;; Add to parent if parent provided
     (when v-node-parent
       (o/add-child! (vt/type-desc v-node-parent) (obj& v-node-parent) new-obj&))
     reified-v-node)))


(defn reify-v-tree
  "Create the object tree described by the v-tree and return the v-tree with
  the object tree in the metadata. The children of the v-tree will be
  added recursively."
  ([v-tree] (reify-v-tree v-tree nil))
  ([v-tree v-tree-parent]
   (let [reified-v-node (reify-v-node v-tree v-tree-parent)]
     (vt/update-children
       reified-v-node
       (mapv #(reify-v-tree % reified-v-node) (vt/children v-tree))))))


(defn ^:private rektify* [new-v-tree reified-v-tree]
  (assert (reified? reified-v-tree) "the provided reified-v-tree has not been reified")
  (if (= new-v-tree reified-v-tree)
    ;; There are no changes so just return the reified v-tree
    reified-v-tree
    (throw (js/Error. "rektifying is not yet supported"))))


(defn rektify
  "Given a new v-tree and optionally, a previously reified v-tree, update all
  objects in the reified v-tree to match the new v-tree and return the
  new v-tree with the object tree in the metadata."
  ([new-v-tree]
   (rektify new-v-tree nil))
  ([new-v-tree reified-v-tree]
   (if (nil? reified-v-tree)
     (reify-v-tree new-v-tree)
     (rektify* new-v-tree reified-v-tree))))
