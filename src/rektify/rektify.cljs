(ns rektify.rektify
  "
  ## Generator state

  * `::&o-parent` - Reference to the object which is the parent of the
                    generator's object v-tree.
  * `::o-parent-desc - The object description of the parent object
  * `::v-tree` - The generator's most recently generated v-tree
  * `::child-generators` - A sequence of the generator child in the generator's
                           v-tree
  * `::local-state` - The generator's local state, made available to lifecycle
                      functions.

  ## V-tree state

  * `::&o-tree` - Reference to the object tree that the v-tree represents
  "
  (:require [rektify.virtual-tree :as vt]
            [rektify.generator :as g]
            [rektify.object :as o]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def ^:dynamic **cur-local-state*
  "An atom containing the local state of the generator which is currently being
  rektified. This is used to contain all state transitions during life-cycle
  functions."
  (atom nil
        :validator (fn [_]
                     (throw (js/Error. "Generator state manipulation can only happen during rektification.")))))


(def ^:dynamic *cur-global-state*
  "The current global state available to generators during rektification."
  nil)


;; XXX: If atoms work, try and make this a transient map instead
(def ^:dynamic **global-state-subscriptions*
  "A map atom which tracks the current generator's subscriptions to global
  state during reification or rektification. The keys are the key paths
  subscribed to by the generator in the form of [:key1 :key2] and the values
  are the value that was retrieved."
  (atom nil
        :validator (fn [_]
                     (throw (js/Error. "Global state can only be accessed during rektification.")))))


(declare reify-generator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn &o-tree
  "A generator or v-tree's object tree, returns `nil` if no object tree exists"
  [v-node]
  (if (vt/generator? v-node)
    (recur (get (vt/state v-node) ::v-tree))
    (get (vt/state v-node) ::&o-tree)))


(defn reify-v-tree
  "Given a raw v-tree, reify all the object node, attach them to their  parents
  and return the reified v-tree with the object instances contained in the
  state of each v-node.

  The parent-obj-desc and &parent-obj is the parent object & description for the
  object created at the root of the v-tree.

  The `*child-gens` atom is used to keep a sequence of all the child generators
  found in the v-tree, which are themselves reified.
  "
  ([v-tree *child-gens]
    (reify-v-tree v-tree *child-gens nil nil))
  ([v-tree *child-gens parent-obj-desc &parent-obj]
   (when v-tree
     (if (vt/object? v-tree)
       ;; Construct object, attach to parent and return v-node with state info
       (let [obj-desc (vt/type-desc v-tree)
             obj-props (vt/props v-tree)
             v-children (vt/children v-tree)
             &obj (o/construct-obj! obj-desc obj-props)]
         (vt/with-state
           (vt/object obj-desc obj-props
                      ;; Recursively reify each child and store in the return object
                      (mapv (fn [v-child]
                              (let [reified-v-tree (reify-v-tree
                                                     v-child *child-gens
                                                     obj-desc &obj)
                                    &new-o-tree (&o-tree reified-v-tree)]
                                (when &new-o-tree
                                  (o/add-child! obj-desc &obj &new-o-tree))
                                reified-v-tree))

                            v-children))
           ;; store a reference to the created object in the v-node's state
           {::&o-tree &obj}))
       ;; If this is a generator node, reify it, store it in *child-gens and return
       ;; note that the generator to be reified needs its parent object node info
       (let [reified-gen (reify-generator
                           (vt/with-state v-tree
                                          {::&o-parent &parent-obj
                                           ::o-parent-desc parent-obj-desc}))]
         (swap! *child-gens conj reified-gen)
         reified-gen)))))


(defn reify-generator
  ([gen global-state]
   (assert (or (nil? global-state) (map? global-state)
               "global-state must be nil or a map"))
    ;; Make the global state map available to generators for the remainder of the reification
   (binding [*cur-global-state* global-state]
     (reify-generator gen)))
  ([gen]
   (assert (g/generator? gen) (g/invalid-generator-msg gen))
   (binding [**cur-local-state* (atom nil)
             **global-state-subscriptions* (atom {})]
     (let [gen-state (vt/state gen)
           &o-parent (::&o-parent gen-state)
           o-parent-desc (::o-parent-desc gen-state)
           gen-desc (vt/type-desc gen)
           gen-props (vt/props gen)
           gen-children (vt/children gen)]
       (g/init gen-desc gen-props gen-children)
       (let [*v-tree-gen-children (atom nil)
             v-tree (-> gen-desc
                      (g/generate gen-props @**cur-local-state* gen-children)
                      (reify-v-tree *v-tree-gen-children o-parent-desc &o-parent))
             &o-tree (&o-tree v-tree)]
         (g/post-generate gen-desc gen-props @**cur-local-state* &o-tree)
         (vt/merge-state
           gen
           {::v-tree v-tree
            ::local-state @**cur-local-state*
            ::child-generators @*v-tree-gen-children
            ::global-state-subscriptions @**global-state-subscriptions*}))))))

(defn subscriptions
  [gen]
  "A generator's subscriptions"
  (get (vt/state gen) ::global-state-subscriptions))


(defn child-generators
  [gen]
  "The list of generator children that was produced by this generator's v-tree"
  (get (vt/state gen) ::child-generators))

(defn local-state
  [gen]
  (get (vt/state gen) ::local-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state retrieval and subscription functions for use inside generator
;; lifecycle functions

(defn get-in-state
  "Get a val from the global state without subscribing"
  [ks]
  (assert (map? *cur-global-state*)
          "Either no global state is defined or it is not a map")
  (get-in *cur-global-state* ks))


(defn subscribe
  "Subscribe to a key path and return the value found at the key path. A record
  of the key path and the value found is stored and used to determine if a
  regeneration is needed during rektification."
  [ks]
  (assert (vector? ks) "ks must be vector of a key path")
  (let [val (get-in-state ks)]
    (swap! **global-state-subscriptions* assoc ks val)
    val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local state manipulation functions for use inside generator lifecycle functions

(defn reset-local-state
  [new-state]
  (assert (or (map? new-state)
              (nil? new-state))
          "Local state is nil or a map")
  (reset! **cur-local-state* new-state))


(defn update-in-local-state
  [ks f & args]
  (apply swap! **cur-local-state* update-in ks f args))


(defn assoc-local-state
  ([key val]
   (swap! **cur-local-state* assoc key val))
  ([key val & kvs]
   (apply swap! **cur-local-state* assoc key val kvs)))


(defn assoc-in-local-state
  [ks val]
  (swap! **cur-local-state* assoc-in ks val))


(defn dissoc-local-state
  ([] (swap! **cur-local-state* dissoc))
  ([key] (swap! **cur-local-state* dissoc key))
  ([key & ks] (apply swap! **cur-local-state* dissoc key ks)))


(defn swap-local-state
  ([f] (swap! **cur-local-state* f))
  ([f & args] (apply swap! **cur-local-state* f args)))


