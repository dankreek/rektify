(ns rektify.rektify
  "
  ## Generator state

  A generator's state is kept in an atom on the node's metadata at a key called
  `::*gen-state`

  This atom contains the keys:

  * `:&o-parent` - Reference to the object which is the parent of the
                   generator's object v-tree.
  * `:o-parent-desc - The object description of the parent object
  * `:v-tree` - The generator's most recently generated v-tree
  * `:child-generators` - A sequence of the generator child in the generator's
                          v-tree
  * `:local-state` - The generator's local state, made available to lifecycle
                     functions.
  * `:state-subscriptions` - A map of key-paths -> values, where the key-paths
                             are the generator's subscriptions and the values
                             are the value of the key-paths into global state
                             during the last generate/regenerate.

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


(def ^:dynamic *global-state*
  "The current global state available to generators during rektification."
  nil)


;; TODO: If atoms work, try and make this a transient map instead
(def ^:dynamic **global-state-subscriptions*
  "A map atom which tracks the current generator's subscriptions to global
  state during reification or rektification. The keys are the key paths
  subscribed to by the generator in the form of [:key1 :key2] and the values
  are the value that was retrieved."
  (atom nil
        :validator (fn [_]
                     (throw (js/Error. "Global state can only be accessed during rektification.")))))


(defn ^:private reified-generator?
  "Is the generator a valid generator which as been reified?"
  [gen]
  (if (not (g/generator? gen))
    false
    (not (nil? (vt/state gen)))))


(defn ^:private unreified-generator-msg
  "Return an error message describing why a value is not a reified generator."
  [gen]
  (if (not (g/generator? gen))
    (g/invalid-generator-msg gen)
    (let [*gen-state (vt/state gen)]
      (if (not (instance? Atom *gen-state))
        "generator has not been reified"))))


(defn ^:private update-state-subscriptions
  "Return the new values for the key-paths in the current subscriptions map"
  [cur-subscriptions]
  (loop [key-paths (keys cur-subscriptions)
         !new-subscriptions (transient {})]
    (if (seq key-paths)
      (let [key-path (first key-paths)]
        (recur (rest key-paths)
               (assoc! !new-subscriptions
                       key-path (get-in *global-state* key-path))))
      (persistent! !new-subscriptions))))


(declare reify-generator)
(declare destroy-v-tree)
(declare regenerate)
(declare rektify-v-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn gen-state-atom
  "Return a generator's state atom"
  [gen]
  (get (vt/state gen) ::gen-state))


(defn subscriptions
  [gen]
  "A generator's subscriptions"
  (get @(gen-state-atom gen) :state-subscriptions))


(defn child-generators
  [gen]
  "The list of generator children that was produced by this generator's v-tree"
  (get @(gen-state-atom gen) :child-generators))


(defn local-state
  [gen]
  (get @(gen-state-atom gen) :local-state))


(defn gen-v-tree
  "Get a reified generator's v-tree"
  [gen]
  (get @(gen-state-atom gen) :v-tree))


(defn &o-tree
  "A generator or v-tree's object tree, returns `nil` if no object tree exists"
  [v-node]
  (if (vt/generator? v-node)
    (recur (gen-v-tree v-node))
    (get (vt/state v-node) ::&o-tree)))


(defn new-gen-state
  "A a new state atom to a generator."
  ([gen]
   (new-gen-state gen {}))
  ([gen init-state]
   (assert (map? init-state) "Generator state must be a map")
   (vt/with-state gen {::gen-state (atom init-state)})))


(defn merge-gen-state
  "Merge the new generator state with the current generator state and return
  the generator."
  [gen new-state]
  (swap! (gen-state-atom gen) merge new-state)
  gen)


(defn merge-new-gen-state
  "Merge the new state with the cur-gen's state and apply it to the new-gen"
  [cur-gen new-gen new-state]
  (let [*gen-state (gen-state-atom cur-gen)]
    (swap! *gen-state merge new-state)
    (vt/with-state new-gen {::gen-state *gen-state})))


;; TODO: write test
(defn collect-generator-children
  "Return a list of all child generators in the v-tree"
  ([v-tree]
    ;; TODO: Use a list as an accumulator instead, this is hacky and not how transients are supposed to work (they're not reference types)
   (let [generators! (transient [])]
     (collect-generator-children v-tree generators!)
     (persistent! generators!)))
  ([v-node generators!]
   (if (vt/generator? v-node)
     (conj! generators! v-node)
     (loop [children (vt/children v-node)]
       (when (seq children)
         (collect-generator-children (first children) generators!)
         (recur (rest children)))))))


(defn add-v-tree-to-obj
  "Add the root object of the reified v-tree to the parent object's children.
  If the v-tree is a generator update the state with the new parent object info."
  [v-tree o-parent-desc &o-parent]
  (let [&obj (&o-tree v-tree)]
    (when (and &obj o-parent-desc)
      (o/add-child! o-parent-desc &o-parent &obj)
      (when (vt/generator? v-tree)
        ;; Add the object parent info to the generator's state
        (merge-gen-state v-tree {:&o-parent &o-parent
                                 :o-parent-desc o-parent-desc})))
    v-tree))


(defn reify-v-tree
  "Given a raw v-tree, reify all the object node, attach them to their  parents
  and return the reified v-tree with the object instances contained in the
  state of each v-node.

  The parent-obj-desc and &parent-obj is the parent object & description for the
  object created at the root of the v-tree.

  The `*child-gens` atom is used to keep a sequence of all the child generators
  found in the v-tree, which are themselves reified.
  "
  [v-tree *child-gens]
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
                     (when (some? v-children)
                       (mapv (fn [v-child]
                               (let [reified-v-tree (reify-v-tree
                                                      v-child *child-gens)]
                                 (add-v-tree-to-obj reified-v-tree obj-desc &obj)
                                 reified-v-tree))
                             v-children)))
          ;; store a reference to the created object in the v-node's state
          {::&o-tree &obj}))
      ;; If this is a generator node, reify it, store it in *child-gens and return
      (let [reified-gen (reify-generator v-tree)]
        (swap! *child-gens conj reified-gen)
        reified-gen))))


(defn reify-generator
  ;; TODO: Docs
  ([gen global-state]
   (assert (or (nil? global-state) (map? global-state)
               "global-state must be nil or a map"))
    ;; Make the global state map available to generators for the remainder of the reification
   (binding [*global-state* global-state]
     (reify-generator gen)))
  ([gen]
   (assert (g/generator? gen) (g/invalid-generator-msg gen))
   (binding [**cur-local-state* (atom nil)
             **global-state-subscriptions* (atom {})]
     (let [gen-desc (vt/type-desc gen)
           gen-props (vt/props gen)
           gen-children (vt/children gen)]
       (g/init gen-desc gen-props gen-children)
       (let [*v-tree-gen-children (atom nil)
             v-tree (-> gen-desc
                      (g/generate gen-props @**cur-local-state* gen-children)
                      (reify-v-tree *v-tree-gen-children))
             &o-tree (&o-tree v-tree)]
         (g/post-generate gen-desc gen-props @**cur-local-state* &o-tree)
         (new-gen-state
           gen
           {:v-tree v-tree
            :local-state @**cur-local-state*
            :child-generators @*v-tree-gen-children
            :state-subscriptions @**global-state-subscriptions*}))))))


(defn destroy-generator
  "Destroy a generator, its v-tree, all of its descendants."
  ([gen global-state]
    (binding [*global-state* global-state]
      (destroy-generator gen)))
  ([gen]
    ;; TODO: Assert generator is reified
   (assert (g/generator? gen)
           (g/invalid-gen-desc-msg gen))
   (let [gen-desc (vt/type-desc gen)
         gen-props (vt/props gen)
         gen-state @(gen-state-atom gen)
         v-tree (get gen-state :v-tree)
         local-state (get gen-state :local-state)
         &obj (&o-tree v-tree)]
     ;; Recurse to children, depth-first
     (loop [children (get gen-state :child-generators)]
       (when (seq children)
         (destroy-generator (first children))
         (recur (rest children))))
     (g/pre-destroy gen-desc gen-props local-state &obj)
     ;; Destroy the v-tree's object tree
     (when &obj
       (o/destroy! (vt/type-desc v-tree) &obj))
     ;; Ensure nothing is returned, could cause issues in calling code
     nil)))


(defn destroy-v-tree
  "Destroy all objects in the provided v-tree, depth first."
  [v-tree]
  ;; Destroy all the child generators of this v-tree first
  (loop [child-gens (collect-generator-children v-tree)]
    (when (seq child-gens)
      (destroy-generator (first child-gens))
      (recur (rest child-gens))))

  ;; Destroy the remaining objects in the v-tree
  (o/destroy! (vt/type-desc v-tree) (&o-tree v-tree))
  nil)


(defn regeneration-required?
  "Given a new generator and the current generator's new subscriptions,
  determine if the current generator needs to be regenerated."
  [cur-gen new-gen]
  (assert (reified-generator? cur-gen)
          "cur-gen is not a reified generator")
  (assert (vt/generator? new-gen)
          "new-gen is not a generator")
  (assert (= (vt/type-desc cur-gen) (vt/type-desc new-gen))
          "Checking regeneration requires that both generators are the same type")

  (let [cur-subscriptions (:state-subscriptions @(gen-state-atom cur-gen))]
    (or
      ;; Have the props or children changed?
      (not= cur-gen new-gen)
      ;; Have the subscriptions changed?
      (not= cur-subscriptions (update-state-subscriptions cur-subscriptions)))))


(defn truncate-children
  [v-children new-size &parent parent-o-desc]
  ;; need parent info here to remove children before destroying
  (loop [del-children (take-last (- (count v-children) new-size) v-children)]
    (when-let [child (first del-children)]
      (o/remove-child! parent-o-desc &parent (&o-tree child))
      (destroy-v-tree child)
      (recur (rest del-children))))
  (take new-size v-children))


(defn normalize-cur-v-children
  [cur-v-children new-v-children &parent parent-o-desc]
  (js/console.log "cur-v-children" cur-v-children "new-v-children" new-v-children)
  (cond
    (< (count cur-v-children) (count new-v-children))
    (throw (js/Error. "Adding v-tree children not implemented"))

    (> (count cur-v-children) (count new-v-children))
    (truncate-children cur-v-children (count new-v-children) &parent parent-o-desc)

    :default
    cur-v-children))


(defn rektify-v-tree-children
  [cur-v-children new-v-children *gen-children &parent parent-o-desc]
  (let [new-v-children (when new-v-children (remove nil? new-v-children))
        cur-v-children (normalize-cur-v-children cur-v-children new-v-children &parent parent-o-desc)]
    (when (or cur-v-children new-v-children)
      (loop [cur-children cur-v-children
             new-children new-v-children
             rektified-children nil
             i 0]
        (if (and (seq cur-children) (seq new-children))
          (recur (rest cur-children)
                 (rest new-children)
                 (conj rektified-children (rektify-v-tree
                                            (first cur-children)
                                            (first new-children)
                                            *gen-children
                                            &parent parent-o-desc
                                            i))
                 (inc i))
          ;; Return the list of rektified children
          (vec rektified-children))))))


(defn replace-child-with-v-tree!
  "Replace an object's child described by `o-parent-desc` with the object in
  the reified v-tree. If no `o-parent-desc` is provided then do nothing. Returns
  the v-tree."
  [o-parent-desc &parent &cur-o v-tree]
  (when o-parent-desc
    (o/replace-child! o-parent-desc &parent &cur-o (&o-tree v-tree))
    (when (vt/generator? v-tree)
      (swap! (gen-state-atom v-tree)
             assoc
             :&o-parent &parent
             :o-parent-desc o-parent-desc)))
  v-tree)


(defn rektify-v-tree
  ([cur-v-tree new-v-tree *gen-children]
    (rektify-v-tree cur-v-tree new-v-tree *gen-children nil nil nil))
  ([cur-v-tree new-v-tree *gen-children &parent parent-o-desc]
   (let [child-index (when &parent
                       (o/child-index
                         parent-o-desc &parent (&o-tree cur-v-tree)))]
     (rektify-v-tree
       cur-v-tree new-v-tree *gen-children &parent parent-o-desc child-index)))
  ([cur-v-tree new-v-tree *gen-children &parent parent-o-desc child-index]
    ;; TODO: Account for v-trees being exactly the same
    ;; If new and cur type descs are the same
    (if (= (vt/type-desc new-v-tree) (vt/type-desc cur-v-tree))
      (if (vt/object? cur-v-tree)
        ;; If this is an object update the props and return the new v-node
        (let [obj-desc (vt/type-desc cur-v-tree)
              new-props (vt/props new-v-tree)
              &obj (o/update-props!
                     obj-desc (&o-tree cur-v-tree)
                     new-props (vt/props cur-v-tree))]
          (vt/with-state
            (vt/object
              obj-desc new-props
              (rektify-v-tree-children
                (vt/children cur-v-tree)
                (vt/children new-v-tree)
                *gen-children &obj obj-desc))
            {::&o-tree &obj}))
        ;; This is a generator regenerate and add to the *gen-children
        (let [new-gen (regenerate cur-v-tree new-v-tree)]
          (swap! *gen-children conj new-gen)
          new-gen))

      ;; These are different types, destroy the current and replace wth new
      (let [&cur-o (&o-tree cur-v-tree)
            reified-new (reify-v-tree new-v-tree *gen-children)]
        (replace-child-with-v-tree! parent-o-desc &parent &cur-o reified-new)
        (destroy-v-tree cur-v-tree)
        reified-new))))


(defn regenerate
  ;; TODO: Docs
  ([cur-gen new-gen global-state]
    (binding [*global-state* global-state]
      (regenerate cur-gen new-gen)))
  ([cur-gen new-gen]
   (assert (reified-generator? cur-gen)
           (str "cur-gen is not a reified generator: "
                (unreified-generator-msg cur-gen)))
   (assert (g/generator? new-gen)
           (str "new-gen is not a valid generator: "
                (g/invalid-generator-msg new-gen)))

   (let [*cur-state (gen-state-atom cur-gen)]
     (if (regeneration-required? cur-gen new-gen)
       (binding [**cur-local-state* (atom (:local-state @*cur-state))
                 **global-state-subscriptions* (atom {})]
         (let [*v-tree-gen-children (atom nil)
               gen-desc (vt/type-desc new-gen)
               gen-props (vt/props new-gen)
               gen-v-tree (g/generate gen-desc gen-props
                                      @**cur-local-state* (vt/children new-gen))
               cur-v-tree (:v-tree @*cur-state)
               v-tree (if (= gen-v-tree cur-v-tree)
                        ;; If the new v-tree is the same just return the cur
                        ;; v-tree and regenerate the child generators
                        (let [child-gens (:child-generators @*cur-state)]
                          (when (some? child-gens)
                            (reset! *v-tree-gen-children
                                    (mapv (fn [gen] (regenerate gen gen))
                                          child-gens)))
                          cur-v-tree)
                        ;; If the v-trees are different rektify them
                        (rektify-v-tree cur-v-tree gen-v-tree
                                        *v-tree-gen-children))
               &o-tree (&o-tree v-tree)]
           (g/post-generate gen-desc gen-props @**cur-local-state* &o-tree)

           ;; Return the new-gen with the updated state from the cur-gen
           (merge-new-gen-state
             cur-gen new-gen
             {:v-tree v-tree
              :local-state @**cur-local-state*
              :child-generators @*v-tree-gen-children
              :state-subscriptions @**global-state-subscriptions*})))

       ;; Return the cur-gen if regeneration is not required
       cur-gen))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state retrieval and subscription functions for use inside generator
;; lifecycle functions

(defn get-in-state
  "Get a val from the global state without subscribing"
  [ks]
  (assert (map? *global-state*)
          "Either no global state is defined or it is not a map")
  (get-in *global-state* ks))


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


