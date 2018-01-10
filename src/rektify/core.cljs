(ns rektify.core
  (:require [rektify.validation :as v]
            [rektify.virtual-graph :as v-graph]
            [rektify.graph :as g]
            [rektify.generators :as gen]
            [rektify.object :as o]
            [clojure.set :as set]
            [clojure.string :as str]))

;; TODO: Make generators their own trees of objects, and remove this hack
(def ^:private ^:dynamic *observed-key-paths*
  "Keeps a transient map of all key paths and values observed during a
  generator render call."
  nil)


(def ^:private ^:dynamic **next-state*
  "The state atom for the render cycle. All state updates will go here, but will
  not be reflected until next render."
  nil)


(def ^:private ^:dynamic *cur-state*
  "The contents of the state atom before the render cycle starts."
  nil)


;; A map of generators -> a list of all the state paths they are listening to.
(defonce ^:private *generator-registry (atom {}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols

(defprotocol IHasProperties
  "Methods for object property accessors and mutators"

  (get-props
    [this]
    "Return the props currently set on this object.")

  ;; TODO: The explicit prop should be used first, then a getter, than a default
  (get-prop
    [this prop]
    "Return the current value of the given property for this object. If the
    property has not been explicitly applied, its default value will be
    returned.")

  (set-props
    [this props]
    "Apply the given map of properties to the object. Returns the property map.
    This will change this object's virtual graph.")

  (destroy
    [this]
    "Perform all clean-up operations needed to destroy this object."))


(defprotocol ^:no-doc IHasVirtualGraph
  "Methods for types which can be represented by a virtual graph."

  (-get-virtual-graph
    [this]
    "Return this node's virtual graph")

  (-set-virtual-graph-children
    [this new-v-graph-children]
    "Update the children of the virtual graph, in the case where this node
    doesn't need to be updated, but its children do."))


(defprotocol ^:no-doc IGenerator
  "Methods defining behavior of a virtual graph generator."

  (-regenerate?
    [this props]
    "Does this generator need to generate a new v-graph given the provided
    props?")

  (-set-dirty
    [this]
    "Sets the dirty flag on this generator, indicating that is needs to be
    regenerated.")

  (-dirty?
    [this]
    "Does this generator need to be re-rendered, even if the props have not
    changed?")

  (-copy-generator
    [this other-obj]
    "Extend the other-obj with this generator.")

  (-generate-virtual-graph
    [this props]
    "Generate a virtual graph with the given props")

  (-get-generator-virtual-graph
    [this]
    "Get the generator virtual graph (not the object's virtual graph).")

  (-get-generator-desc
    [this]
    "Get this generator's description.")

  (-cleanup-generator
    [this]
    "If this generator has a cleanup function call it"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State watching functions

(defn- register-generator-obj!
  "Given an object and a map of key paths and their current vals, register the
each path with the current state value. "
  [obj ks->vals]
  (swap! *generator-registry assoc obj ks->vals))


(defn- unregister-generator-obj!
  "Remove an object from the generator registry"
  [obj]
  (swap! *generator-registry dissoc obj))


(defn- dirty-paths?
  "Given a state map, check the map of paths->vals to see if any path has a
  different value than the state map."
  [m paths->vals]
  (if (seq paths->vals)
    (not-any?
      (fn [[path val]] (= (get-in m path) val))
      paths->vals)
    false))


(defn- get-dirty-generators
  "Get all the registered generators that are currently in a dirty state."
  []
  (filterv #(-dirty? %) (keys @*generator-registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object construction and manipulation functions

;; TODO: Once IGraphNode and IObject are detangled, put these functions inside the object
(defn- apply-prop!
  "Given a property description, apply the value of the described property to
the provided object."
  [obj val {:keys [property setter] :as property-desc}]
  (assert (some? setter) "Can not set a read-only property")
  (setter obj property val))


;; TODO: This could be a lot smarter and faster
(defn- apply-new-props!
  "Returns the new property map"
  [prev-props new-props obj prop-map default-props]
  (when (not= prev-props new-props)
    ;; Set all provided props
    (loop [props (seq new-props)]
      (when (seq props)
        (let [[key val] (first props)
              prop-config (get prop-map key nil)]
          (assert (not (nil? prop-config))
                  (str "Could not find property config for the property: " key))
          (when (not= val (get prev-props key))
            (apply-prop! obj val prop-config))
          (recur (rest props)))))

    ;; Set default values for all props that are no longer present
    (loop [props (set/difference (set (keys prev-props)) (set (keys new-props)))]
      (when (seq props)
        (let [key (first props)
              default-val (get default-props key nil)]
          (assert (contains? default-props key)
                  (str "A value for the property " key " is required to be set since it doesn't have a default"))
          (when (not= default-val (get prev-props key))
            (apply-prop! obj default-val (get prop-map key)))
          (recur (rest props))))))
  new-props)


(defn- extend-graph-obj!
  "Extend the provided object as a graph node. Accepts this virtual graph's
  parameters as correct, but does not verify."
  [obj v-graph]
  (let [{:keys [prop-map default-props destructor]
         get-parent-fn :get-parent
         add-child-fn :add-child
         get-children-fn :get-children
         child-index-fn :child-index
         replace-child-at-fn :replace-child-at
         remove-child-at-fn :remove-child-at}
        (v-graph/type-desc v-graph)
        *v-graph (atom v-graph)]
    (specify! obj
      IHasProperties
      (get-props [this]
        (v-graph/props @*v-graph))
      (get-prop [this prop]
        (let [cur-props (v-graph/props @*v-graph)]
          (if (contains? cur-props prop)
            (get cur-props prop)
            (get default-props prop))))
      (set-props [this new-props]
        (apply-new-props!
          (v-graph/props @*v-graph) new-props this prop-map default-props)
        (swap! *v-graph v-graph/update-props new-props))
      (destroy [this]
        (when destructor (destructor this)))

      IHasVirtualGraph
      (-get-virtual-graph [this] @*v-graph)
      (-set-virtual-graph-children [this new-v-graph-children]
        (swap! *v-graph v-graph/update-children new-v-graph-children))

      g/IGraphNode
      (-get-parent [this]
        (get-parent-fn this))
      (-add-child! [this child]
        (add-child-fn this child))
      (-child-index [this child]
        (child-index-fn this child))
      (-get-children [this]
        (get-children-fn this))
      (-replace-child-at! [this new-child index]
        (replace-child-at-fn this new-child index))
      (-remove-child-at! [this index]
        (remove-child-at-fn this index)))
    obj))


(defn- render-v-graph-from-generator-map
  ([gen-map props]
   (render-v-graph-from-generator-map gen-map props nil))
  ([gen-map props head-obj]
   (let [v-graph ((:render gen-map) props head-obj)]
     (assert (or (nil? v-graph) (v-graph/object? v-graph))
             "A generator's render function must return a graph with an object as the head.")
     v-graph)))


(defn- resolve-generator
  "Creates a function map from a generator description. If the description is a
  function then it is called and the resulting generator map is returned,
  otherwise the map itself is just returned "
  [gen-desc]
  (let [result-gen-desc (if (fn? gen-desc)
                          (gen-desc)
                          gen-desc)]
    (assert (map? result-gen-desc)
            "A generator description should be either a map or a function that returns a map")
    (assert (v/required-generator-desc-keys? result-gen-desc)
            (str "The generator description map is missing the following required keys: "
                 (v/pp-set (v/missing-required-generator-desc-keys
                             result-gen-desc))))
    (assert (v/valid-generator-map? result-gen-desc)
            (str "The object description map contains the following invalid keys: "
                 (v/pp-set (v/invalid-generator-desc-keys result-gen-desc))))
    result-gen-desc))


(defn- extend-with-generator!
  [obj gen-desc props resolved-gen-map]
  (let [*dirty? (atom false)
        *props (atom props)]
    (specify! obj
      IGenerator
      (-regenerate? [this new-props]
        (or @*dirty? (not= new-props @*props)))

      (-set-dirty [this] (reset! *dirty? true))

      (-dirty? [this] @*dirty?)

      (-generate-virtual-graph [this new-props]
        (reset! *props new-props)
        (reset! *dirty? false)
        (binding [*observed-key-paths* (transient {})]
          (let [v-graph (render-v-graph-from-generator-map
                          resolved-gen-map new-props this)]
            (register-generator-obj! this (persistent! *observed-key-paths*))
            v-graph)))

      ;; XXX: throw this away once generators are straightened out
      ;; will be fixed once generators are their own object trees
      (-copy-generator
        [this other-obj]
        (extend-with-generator! other-obj gen-desc @*props resolved-gen-map))

      (-get-generator-virtual-graph
        [this]
        [v-graph/generator-key gen-desc @*props])

      (-get-generator-desc [this] gen-desc)

      (-cleanup-generator [this]
        (unregister-generator-obj! this)
        (when-let [cleanup-fn (get resolved-gen-map :cleanup)]
          (cleanup-fn this))))))


(defn- extend-graph-obj-with-generator!
  [obj v-node resolved-gen-map]
  (let [gen-type (v-graph/type-desc v-node)
        props (v-graph/props v-node)]
    (extend-with-generator! obj gen-type props resolved-gen-map)))


(defn- extend-obj!
  "Extend the provided object with the given type description."
  [obj type-desc props]
  (let [{:keys [prop-map default-props destructor post-constructor]} type-desc
        *props (atom props)]
    (specify! obj
      IHasProperties
      (get-props [this] @*props)
      (get-prop [this prop]
        (let [cur-props @*props]
          (if (contains? cur-props prop)
            (get cur-props prop)
            (get default-props prop))))
      (set-props [this new-props]
        (swap! *props apply-new-props! new-props this prop-map default-props))
      (destroy [this]
        (when destructor (destructor this))))
    (when post-constructor (post-constructor obj))
    obj))


(defn- construct-obj-hack
  ([constructor]
   (new constructor))
  ([constructor p1]
   (new constructor p1))
  ([constructor p1 p2]
   (new constructor p1 p2))
  ([constructor p1 p2 p3]
   (new constructor p1 p2 p3))
  ([constructor p1 p2 p3 p4]
   (new constructor p1 p2 p3 p4))
  ([constructor p1 p2 p3 p4 p5]
   (new constructor p1 p2 p3 p4 p5)))


(defn- find-object-constructor
  "Given a set of available keys, return the first constructor in the
  constructor list the can be called. Returns nil if no constructor could be
  found, or [] if no constructor list is given."
  [constructor-list available-props]
  (let [available-keys (set (keys available-props))]
    (if constructor-list
      (first (filter #(set/subset? (set %) available-keys) constructor-list))
      [])))


(defn- construct-object
  "Given a type description and set of initial properties, construct and return
  a new object with all properties set. Return the plain objects with no
  methods specified on it."
  [type-desc init-props]
  (let [{:keys [constructor
                constructor-list
                default-props
                prop-map
                post-constructor]} type-desc
        available-props (merge default-props init-props)
        constructor-keys (find-object-constructor
                           constructor-list available-props)
        constructor-args (mapv #(get available-props %) constructor-keys)
        new-obj (apply construct-obj-hack constructor constructor-args)]
    ;; TODO: May be worth skipping re-setting props used in constructor
    (when post-constructor
      (post-constructor new-obj))
    (loop [set-props (keys init-props)]
      (when-let [prop (first set-props)]
        ;; Skip properties without setters
        (when-let [setter (get-in prop-map [prop :setter])]
          (setter
            new-obj (get-in prop-map [prop :property]) (get init-props prop)))
        (recur (rest set-props))))
    new-obj))


(defn- new-object
  ([type-desc]
   (new-object type-desc {}))
  ([type-desc init-props]
   (let [new-obj (construct-object type-desc init-props)]
     (extend-obj! new-obj type-desc init-props))))


(defn- create-graph-object
  [v-node]
  (let [type-desc (v-graph/type-desc v-node)
        init-props (v-graph/props v-node)
        new-obj (construct-object type-desc init-props)]
    (extend-graph-obj! new-obj v-node)))


(defn- create-generator
  "Create a new generator instance and add the parent generator's children if
  a parent was provided. If there is no parent then set to nil."
  [v-graph gen-parent]
  (let [new-gen (gen/create-generator
                  (v-graph/type-desc v-graph) gen-parent)]
    (when gen-parent (g/-add-child! gen-parent new-gen))
    new-gen))


(defn- link-gen-and-object
  "Links the generator to the provided object, and links the object to the
  generator. Returns the generator instance."
  [gen obj]
  (gen/link-root-obj-to-gen obj gen)
  (gen/set-root-obj gen obj))


(defn- create-and-link-subtree-root
  [generator props]
  (let [generated-v-graph (gen/generate generator props)
        subtree-head (create-graph-object generated-v-graph)]
    (link-gen-and-object generator subtree-head)
    subtree-head))


(defn- generate-graph*
  ([v-graph]
   (generate-graph* v-graph nil nil))
  ([v-graph gen-parent obj-parent]
    (if (v-graph/generator? v-graph)
      ;; XXX: Factor this better!!!
      (let [new-gen (create-generator v-graph gen-parent)
            subtree-root (create-and-link-subtree-root
                           new-gen (v-graph/props v-graph))]
        (when obj-parent (g/-add-child! obj-parent subtree-root))
        (doseq [v-child-node (v-graph/children (gen/v-graph new-gen))]
          (generate-graph* v-child-node new-gen subtree-root))
        subtree-root)
      (let [new-obj (create-graph-object v-graph)]
        (when obj-parent (g/-add-child! obj-parent new-obj))
        (doseq [v-child-node (v-graph/children v-graph)]
          (generate-graph* v-child-node gen-parent new-obj))
        new-obj))))


(defn- destroy-graph!
  ([graph]
   (destroy-graph! graph nil nil))
  ([graph parent child-index]
   (let [children (g/-get-children graph)
         child-count (count children)]
     ;; If this node has a generator call its cleanup method before destroying its children
     (when (satisfies? IGenerator graph)
       (-cleanup-generator graph))
     ;; Removing children from right-to-left to leverage remove-child-at! properly
     (loop [i (dec child-count)]
       (when (>= i 0)
         (destroy-graph! (nth children i) graph i)
         (recur (dec i)))))
   (when parent
     (g/-remove-child-at! parent child-index))
   (destroy graph)
   nil))


(def ^:no-doc ^:dynamic **apply-graph-queue*
  "This is used during the process of applying a new virtual graph to an
  existing graph."
  nil)


(defn- truncate-children!
  "Truncate the given node's child list down to the size specified and update
  the v-node to reflect the new child list."
  [node new-child-size]
  (let [children (g/-get-children node)
        child-count (count children)]
    (loop [i (dec child-count)]
      (when (>= i new-child-size)
        (destroy-graph! (nth children i) node i)
        (recur (dec i))))
    (-set-virtual-graph-children
      node (drop-last (- child-count new-child-size)
                      (v-graph/children (-get-virtual-graph node))))
    children))


(defn- process-children!
  ;; TODO: Add ID matching here later (Issue #2)
  "If the new virtual child list is shorter than the old child list, truncate
  the old graph."
  [graph v-node-children]
  (let [graph-children (g/-get-children graph)
        graph-child-count (count graph-children)
        new-v-child-count (count v-node-children)]
    (if (<= graph-child-count new-v-child-count)
      graph-children
      (truncate-children! graph new-v-child-count))))


(defn- add-children-to-queue!
  [graph new-v-graph]
  (let [v-node-children (v-graph/children new-v-graph)
        graph-children (process-children! graph v-node-children)
        child-count (count v-node-children)]
    (loop [i 0]
      (when (< i child-count)
        (swap! **apply-graph-queue*
               conj {:graph (nth graph-children i nil)
                     :new-v-graph (nth v-node-children i nil)
                     :graph-parent graph
                     :parent-child-index i})
        (recur (inc i))))))


(defn- resolve-generator-obj
  "If the graph has a generator of the same type as the new v-graph then
  generate a new v-graph if the generator needs re-rendering "
  [graph new-v-graph]
  (let [new-props (v-graph/props new-v-graph)]
    ;; If the graph was made by a generator and the v-graph is a generator
    (if (and (v-graph/generator? new-v-graph)
             (satisfies? IGenerator graph))
      (do
        (when (not= (v-graph/type-desc new-v-graph)
                    (-get-generator-desc graph))
          ;; TODO: This a barfy hack until generators are straightened out
          (let [new-gen-desc (v-graph/type-desc new-v-graph)
                new-gen-map (resolve-generator new-gen-desc)]
            (extend-with-generator! graph new-gen-desc new-props new-gen-map)
            (-set-dirty graph)))
        (if (-regenerate? graph new-props)
           (-generate-virtual-graph graph new-props)
           (-get-virtual-graph graph)))
      new-v-graph)))


(defn- apply-virtual-node-diff
  [graph init-new-v-graph graph-parent parent-child-index]
  ;; TODO: Define -get-virtual-graph on the default object and return nil
  ;; TODO: All cond conditions should be their own functions
  (let [cur-v-graph (when graph (-get-virtual-graph graph))
        new-v-graph (resolve-generator-obj graph init-new-v-graph)]
    (cond
      ;; The new virtual graph is the same as the old, so no change is required
      ;; for this node or any descendants
      (= new-v-graph cur-v-graph)
      graph

      ;; There is no real node here so create one
      (nil? graph)
      (let [new-graph (generate-graph* new-v-graph)]
        (when graph-parent (g/-add-child! graph-parent new-graph))
        new-graph)

      ;; There is no virtual node here so remove and destroy the graph
      (nil? new-v-graph)
      (destroy-graph! graph graph-parent parent-child-index)

      ;; The node's type and properties are the same but there are children that need updates
      (v-graph/node= new-v-graph cur-v-graph)
      (do
        (-set-virtual-graph-children graph (v-graph/children new-v-graph))
        (add-children-to-queue! graph new-v-graph)
        graph)

      ;; The new virtual node is the same type as the current, so update the
      ;; properties and enqueue children for further compare
      (= (v-graph/type-desc new-v-graph) (v-graph/type-desc cur-v-graph))
      (do
        (set-props graph (v-graph/props new-v-graph))
        (-set-virtual-graph-children graph (v-graph/children new-v-graph))
        (add-children-to-queue! graph new-v-graph)
        graph)

      ;; The two virtual graph types are different, create the new and replace the old
      (not= (v-graph/type-desc new-v-graph) (v-graph/type-desc cur-v-graph))
      (let [new-graph (generate-graph* new-v-graph)]
        (when (satisfies? IGenerator graph)
          (-copy-generator graph new-graph))
        (when graph-parent
          (g/-replace-child-at! graph-parent new-graph parent-child-index))
        (destroy-graph! graph)
        new-graph)

      :default
      (throw (js/Error. "This situation can not be dealt with. This is a bug.")))))



(defn- add-dirty-generators!
  "Add all the dirty generators to the apply graph stack"
  ;; XXX: remove this once generators are straightened out
  []
  (let [dirty-generators (get-dirty-generators)
        gen-count (count dirty-generators)]
    (loop [i 0]
      (when (< i gen-count)
        (let [graph (nth dirty-generators i)
              new-v-graph (-get-generator-virtual-graph graph)
              graph-parent (g/-get-parent graph)
              parent-child-index (when graph-parent
                                   (g/-child-index graph-parent graph))]
          (swap! **apply-graph-queue*
                 conj {:graph graph
                       :new-v-graph new-v-graph
                       :graph-parent graph-parent
                       :parent-child-index parent-child-index})
          (recur (inc i)))))))


(defn- rektify-graph*
  [graph new-v-graph]
  (assert (nil? **apply-graph-queue*))
  (binding [**apply-graph-queue* (atom #queue [])]
    (let [head-parent (g/-get-parent graph)
          head-child-index (when head-parent (g/-child-index head-parent graph))
          head (apply-virtual-node-diff
                 graph new-v-graph head-parent head-child-index)]
      (loop []
        ;; Add all the dirty generators into the re-render stack
        (when (not (seq @**apply-graph-queue*))
          (add-dirty-generators!))
        (when-let [{:keys [graph-parent parent-child-index]
                    next-graph :graph
                    next-new-v-graph :new-v-graph}
                   (peek @**apply-graph-queue*)]
          (peek @**apply-graph-queue*)
          (swap! **apply-graph-queue* pop)
          (apply-virtual-node-diff
            next-graph next-new-v-graph graph-parent parent-child-index)
          (recur)))
      head)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn update-in-state
  "Update the value in the state map located at the provided key path by calling
  `f` with the `args` and setting the return value. This is essentially the same
  signature as the `update-in` function without the first parameter.

  Note that the updated values won't actually be visible until the next
  render cycle in order to maintain consistency amongst all generators during a
  single cycle.

  This is intended to be called from the `:render` function of a generator and
  will throw an exception if called outside of a `:render  function."
  {:doc/format :markdown}
  [ks f & args]
  (assert (some? **next-state*)
          "Can not call `update-in-state` outside of a generator's `:render` method")
  (apply swap! **next-state* update-in ks f args))


(defn assoc-in-state
  "Set a value in the render cycle's state at the location specified by the
  key sequence in `ks`. This function has the same signature as `assoc-in`
  but without the first parameter.

  Note that the updated values won't actually be visible until the next
  render cycle in order to maintain consistency amongst all generators during a
  single cycle.

  This is intended to be called from the `:render` function of a generator and
  will throw an exception if called outside of a `:render  function."
  {:doc/format :markdown}
  [ks v]
  (assert (some? **next-state*)
          "Can not call `assoc-in-state` outside of a generator's `:render` method")
  (swap! **next-state* assoc-in ks v))


(defn get-in-state
  "Given a key path, return a value from this render cycle's global state. This
  function is similar to `get-in` only without the first parameter."
  {:doc/format :markdown}
  ([ks]
   (get-in-state ks nil))
  ([ks not-found]
   (assert (and (some? *cur-state*) (some? *observed-key-paths*))
           "Can not call `get-in-state `outside of generator's render function and without a state atom.")
   (let [val (get-in *cur-state* ks not-found)]
     (assoc! *observed-key-paths* ks val)
     val)))


(defn rektify-graph
  "Given a real graph, and a virtual graph, apply changes in the
  virtual graph to the real graph and return a real graph. If head of the
  virtual graph is the same type of object as the current real graph, the
  same instance is returned. If the cur-graph is nil then a new graph will be
  created. If a state atom is provided it should be a map, if none is provided
  then the state will be empty."
  ([cur-graph new-virtual-graph]
   (rektify-graph cur-graph new-virtual-graph (atom {})))
  ([cur-graph new-virtual-graph *state]
   (assert (or (nil? new-virtual-graph) (v-graph/generator? new-virtual-graph))
           "The virtual graph's head must be a generator or nil")
   (assert (or (nil? *state) (map? @*state))
           "The state must be either nil or a reference to a map")
   (assert (or (nil? new-virtual-graph)
               (v/virtual-node? new-virtual-graph))
           "An invalid virtual graph was provided")
   (binding [*cur-state* @*state
             **next-state* *state]
     (if cur-graph
       (rektify-graph* cur-graph new-virtual-graph)
       (generate-graph* new-virtual-graph)))))


(defn generate-graph
  "Create an object graph from the given virtual graph. If a state atom is
  provided it should be a map, if none is provided then the state will be
  an empty map."
  ([v-graph]
   (generate-graph v-graph (atom {})))
  ([v-graph *state]
   (rektify-graph nil v-graph *state)))


(defn get-existing-object-properties
  "Given an object and its property map, generate a map of properties
  that reflect the current state of the object. This is useful for generating
  a map of default values from a newly instantiated object."
  [obj prop-map]
  (loop [props (keys prop-map)
         defaults {}]
    (if-some [prop (first props)]
      (let [{:keys [property getter]} (get prop-map prop)]
        (if getter
          (recur (rest props) (assoc defaults prop (getter obj property)))
          (recur (rest props) defaults)))
      defaults)))


(defn object-v-node
  "Create a new object virtual node with the provided object description, the
  object properties, and a vector of children the node may have."
  ([object-desc props]
   (object-v-node object-desc props []))
  ([object-desc props children]
   (assert (v/required-object-desc-keys? object-desc)
           (str "The object description map is missing the following required keys: "
                (v/pp-set (v/missing-required-object-desc-keys object-desc))))
   (assert (v/valid-object-desc-keys? object-desc)
           (str "The object description map contains the following invalid keys: "
                (v/pp-set (v/invalid-object-desc-keys object-desc))))
   [v-graph/object-key object-desc props children]))


(defn generator-v-node
  "Create a new generator virtual node with the provided generator description
  and optionally, the generator's props."
  ([gen-desc]
   (generator-v-node gen-desc {}))
  ([gen-desc props]
   (assert (or (fn? gen-desc)
               (map? gen-desc))
           "The generator description should be either a generator map or a function which returns a generator map.")
   [v-graph/generator-key gen-desc props nil]))


(defn extend-existing-obj!
  "Extend the provided JavaScript object with a type description. The object's
  property map will be generated by executing all of the object description's
  property getters."
  [obj type-desc]
  (let [props (get-existing-object-properties obj (:prop-map type-desc))]
    (extend-obj! obj type-desc props)))


(defn extend-existing-graph-obj!
  "Extends the provided graph object with the given prop-map. The graph should
  not have any children."
  [graph object-desc]
  (js/console.warn "extend-existing-graph-obj! does not currently consider children.")
  (let [props (get-existing-object-properties graph (:prop-map object-desc))]
    (extend-graph-obj! graph (object-v-node object-desc props))))



