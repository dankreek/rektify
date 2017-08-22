(ns rektify.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; TODO: Make generators their own trees of objects, and remove this hack
(def ^:no-doc ^:dynamic *observed-key-paths*
  "Keeps a transient map of all key paths and values observed during a
  generator render call."
  nil)

(def ^:no-doc ^:dynamic **cur-state*
  "The state atom for the render cycle. All state updates will go here, but will
  not be reflected until next render."
  nil)

(def ^:no-doc ^:dynamic *prev-state*
  "The contents of the state atom before the render cycle starts."
  nil)

;; A map of generators -> a list of all the state paths they are listening to.
(defonce ^:no-doc *generator-registry (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(def required-generator-life-cycles
  #{:render})


(def optional-generator-life-cycles
  #{:cleanup})


(def valid-generator-map-keys
  (set/union required-generator-life-cycles
             optional-generator-life-cycles))


(def required-obj-desc-keys
  #{:get-parent
    :add-child
    :child-index
    :replace-child-at
    :remove-child-at
    :get-children
    :constructor
    :prop-map})


(def optional-obj-desc-keys
  #{:default-props
    :constructor-list
    :post-constructor
    :destructor})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameter validator functions

(def ^:no-doc valid-obj-desc-keys
  (set/union required-obj-desc-keys optional-obj-desc-keys))


(defn- required-map-keys?
  "Check that all keys in the `required-key-set `are in the given `key-set `."
  [required-key-set m]
  (= required-key-set (set/intersection (set (keys m)) required-key-set)))


(def ^:no-doc required-object-desc-keys?
  (partial required-map-keys? required-obj-desc-keys))


(def ^:no-doc required-generator-desc-keys?
  (partial required-map-keys? required-generator-life-cycles))


(defn- missing-required-keys
  [required-key-set m]
  (set/difference required-key-set (set (keys m))))


(def ^:no-doc missing-required-object-desc-keys
  (partial missing-required-keys required-obj-desc-keys))


(def ^:no-doc missing-required-generator-desc-keys
  (partial missing-required-keys required-generator-life-cycles))


(defn- valid-keys?
  "Check that all keys in `key-set `are in the `valid-key-set `."
  [valid-key-set m]
  (set/subset? (set (keys m)) valid-key-set))


(def ^:no-doc valid-object-desc-keys?
  (partial valid-keys? valid-obj-desc-keys))


(defn- invalid-keys
  "Return the set of keys in the given map that are not valid"
  [valid-key-set m]
  (set/difference (set (keys m)) valid-key-set))


(def ^:no-doc invalid-generator-desc-keys
  (partial invalid-keys required-generator-life-cycles))


(def ^:no-doc invalid-object-desc-keys
  (partial invalid-keys valid-obj-desc-keys))


(def ^:no-doc valid-generator-map?
  (partial valid-keys? valid-generator-map-keys))


(defn- pp-set
  "Pretty print the set"
  [s]
  (str/join ", " s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtual graph functions

(defn virtual-node-props
  [virtual-node]
  (nth virtual-node 2 {}))


(defn update-virtual-node-props
  "Returns the virtual node with its properties changed to the `new-props `."
  [virtual-node new-props]
  (assoc virtual-node 2 new-props))


(defn update-virtual-node-children
  [virtual-node new-children]
  (assoc virtual-node 3 new-children))


(defn virtual-node-type-desc
  [virtual-node]
  (second virtual-node))

(defn virtual-node-children
  "Gets the sequence of children from a virtual graph node."
  [virtual-graph]
  (nth virtual-graph 3 nil))


(defn virtual-node=
  "Test to see if the type and properties of each virtual graph are the same"
  [v-graph-1 v-graph-2]
  (and (= (virtual-node-type-desc v-graph-1)
          (virtual-node-type-desc v-graph-2))
       (= (virtual-node-props v-graph-1)
          (virtual-node-props v-graph-2))))


(defn virtual-node-is-object?
  [virtual-node]
  (= ::object (first virtual-node)))


(defn virtual-node-is-generator?
  [virtual-node]
  (= ::generator (first virtual-node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IObject contains methods for property and child accessors and mutators

(defprotocol IObject
  (get-object-props
    [this]
    "Return the props currently set on this object.")

  (get-object-prop
    [this prop]
    "Return the current value of the given property for this object. If the
    property has not been explicitly applied, its default value will be
    returned.")

  (apply-props!
    [this props]
    "Apply the given map of properties to the object. Returns the property map.
    This will change this object's virtual graph.")

  (destroy!
    [this]
    "Perform all clean-up operations needed to destroy this object."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IGraphNode - Methods needed for creating and diffing graphs of objects

(defprotocol ^:no-doc IGraphNode
  (-get-virtual-graph
    [this]
    "Return this node's virtual graph")

  (-get-children
    [this]
    "Get the list of children of this object.")

  (-set-virtual-graph-children!
    [this new-v-graph-children]
    "Update the children of the virtual graph, in the case where this node
doesn't need to be updated, but its children do.")

  (-get-parent
    [this]
    "Get the parent of this object.")

  (-add-child!
    [this child]
    "Add a child to the end of the object's child list")

  (-child-index
    [this child]
    "Get the index in the child list of the provided child object.")

  (-replace-child-at!
    [this new-child index]
    "Replace the object at the given index with the provided child.")

  (-remove-child-at!
    [this index]
    "Remove the child of this object at the given index. Return the child
object that was removed."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IGenerator - methods defining behavior of a graph generator

(defprotocol ^:no-doc IGenerator
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
    "Is this generator dirty?")

  (-copy-generator
    [this other-obj]
    "Extend the other-obj with this generator.")

  (-generate-virtual-graph
    [this props]
    "Generate a virtual graph with the given props")

  (-get-generator-virtual-graph
    [this]
    "Get the generator virtual graph (not the object's virtual graph).")

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


(defn- dirty-generators!
  "Compare every generator's previous val for each state path and if the current
  state path's value is different, set the dirty flag."
  [new-state]
  (let [objects (keys @*generator-registry)
        objects-count (count objects)]
    (loop [i 0]
      (when (< i objects-count)
        (let [object (nth objects i)
              paths->vals (get @*generator-registry object)]
          (when (dirty-paths? new-state paths->vals)
            (-set-dirty (nth objects i))))
        (recur (inc i))))))


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


(defn- apply-new-prop!
  [prev-val new-val obj prop-config default-val]
  (let [new-val-or-default (or new-val default-val)]
    (if (not= prev-val new-val-or-default)
      (apply-prop! obj new-val-or-default prop-config)
      new-val)))


(defn- apply-new-props!
  "Returns the new property map"
  [prev-props new-props obj prop-map default-props]
  (when (not= prev-props new-props)
    (loop [props (set/union (set (keys new-props)) (keys prev-props))]
      (when-some [prop (first props)]
        (when (not (contains? prop-map prop))
          (throw (js/Error.
                   (str "Could not find property config for the property: " prop))))
        (try
          (apply-new-prop!
            (get prev-props prop)
            (get new-props prop)
            obj
            (get prop-map prop)
            (get default-props prop))
          (catch :default e
            (set! (.-message e) (str "Error applying properties " new-props
                                     "in property " prop ", " (.-message e)))
            (throw e)))
        (recur (rest props)))))
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
        (virtual-node-type-desc v-graph)
        *v-graph (atom v-graph)]
    (specify! obj
      IObject
      (get-object-props [this]
        (virtual-node-props @*v-graph))
      (get-object-prop [this prop]
        (let [cur-props (virtual-node-props @*v-graph)]
          (if (contains? cur-props prop)
            (get cur-props prop)
            (get default-props prop))))
      (apply-props! [this new-props]
        (apply-new-props! (virtual-node-props @*v-graph) new-props this prop-map default-props)
        (swap! *v-graph update-virtual-node-props new-props))
      (destroy! [this]
        (when destructor (destructor this)))

      IGraphNode
      (-set-virtual-graph-children! [this new-v-graph-children]
        (swap! *v-graph update-virtual-node-children new-v-graph-children))
      (-get-virtual-graph [this] @*v-graph)
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
     (assert (or (nil? v-graph) (virtual-node-is-object? v-graph))
             "A generator's render function must return a graph with an object as the head.")
     v-graph)))


(defn- extend-with-generator!
  [obj gen-type props resolved-gen-map]
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

      ;; TODO: there's very possibly a memory leak here, since closures will enclose each other
      ;; will be fixed once generators are their own objects
      (-copy-generator
        [this other-obj]
        (extend-with-generator! other-obj gen-type @*props resolved-gen-map))

      (-get-generator-virtual-graph [this] [::generator gen-type @*props])

      (-cleanup-generator [this]
        (unregister-generator-obj! this)
        (when-let [cleanup-fn (get resolved-gen-map :cleanup)]
          (cleanup-fn this))))))


(defn- extend-graph-obj-with-generator!
  [obj v-node resolved-gen-map]
  (let [gen-type (virtual-node-type-desc v-node)
        props (virtual-node-props v-node)]
    (extend-with-generator! obj gen-type props resolved-gen-map)))


(defn- extend-obj!
  "Extend the provided object with the given type description."
  [obj type-desc props]
  (let [{:keys [prop-map default-props destructor post-constructor]} type-desc
        *props (atom props)]
    (specify! obj
      IObject
      (get-object-props [this] @*props)
      (get-object-prop [this prop]
        (let [cur-props @*props]
          (if (contains? cur-props prop)
            (get cur-props prop)
            (get default-props prop))))
      (apply-props! [this new-props]
        (swap! *props apply-new-props! new-props this prop-map default-props))
      (destroy! [this] (post-constructor obj)))
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
  (let [{:keys [constructor constructor-list default-props prop-map post-constructor]} type-desc
        available-props (merge default-props init-props)
        constructor-keys (find-object-constructor constructor-list available-props)
        constructor-args (mapv #(get available-props %) constructor-keys)
        new-obj (apply construct-obj-hack constructor constructor-args)]
    ;; TODO: May be worth skipping re-setting props used in constructor
    (when post-constructor
      (post-constructor new-obj))
    (loop [set-props (keys init-props)]
      (when-let [prop (first set-props)]
        ;; Skip properties without setters
        (when-let [setter (get-in prop-map [prop :setter])]
          (setter new-obj (get-in prop-map [prop :property]) (get init-props prop)))
        (recur (rest set-props))))
    new-obj))


(defn- new-object
  ([type-desc]
   (new-object type-desc {}))
  ([type-desc init-props]
   (let [new-obj (construct-object type-desc init-props)]
     (extend-obj! new-obj type-desc init-props))))


(defn- new-graph-object
  [v-node]
  (let [type-desc (virtual-node-type-desc v-node)
        init-props (virtual-node-props v-node)
        new-obj (construct-object type-desc init-props)]
    (extend-graph-obj! new-obj v-node)))


(defn- resolve-generator
  "Creates a function map from a generator description. If the description is a
  function then it is called and the resulting generator map is returned,
  otherwise the map itself is just returned "
  [gen-desc]
  (let [result-gen-desc (if (fn? gen-desc)
                          (gen-desc)
                          gen-desc)]
    (assert (valid-generator-map? result-gen-desc)
            (str "A generator description must be either a function that returns "
                 "a map with a :render function, or a map with a :render function."))
    result-gen-desc))


(defn- new-generator-object
  [v-node]
  (binding [*observed-key-paths* (transient {})]
    (let [gen-desc (virtual-node-type-desc v-node)
          resolved-gen-map (resolve-generator gen-desc)
          init-props (virtual-node-props v-node)
          gen-v-graph (render-v-graph-from-generator-map resolved-gen-map init-props)
          new-obj (new-graph-object gen-v-graph)]
      (extend-graph-obj-with-generator! new-obj v-node resolved-gen-map)
      (register-generator-obj! new-obj (persistent! *observed-key-paths*))
      new-obj)))


(defn- reify-virtual-node
  [v-node]
  (if (virtual-node-is-generator? v-node)
    (new-generator-object v-node)
    (new-graph-object v-node)))


(defn- reify-virtual-graph-level
  "Reducer for `reify-virtual-graph `which reifies the v-node, attaches it the
  given parent and, if the new node has children, adds them to the queue."
  [parent queue v-node]
  (let [node (reify-virtual-node v-node)
        v-children (virtual-node-children (-get-virtual-graph node))]
    (-add-child! parent node)
    (if (seq v-children)
      (conj queue [node v-children])
      queue)))


(defn- reify-virtual-graph*
  ([v-graph]
   (let [head (reify-virtual-node v-graph)]
     (loop [node-queue #queue [[head (virtual-node-children (-get-virtual-graph head))]]]
       (let [[parent v-children] (first node-queue)
             next-queue (reduce (partial reify-virtual-graph-level parent)
                                (rest node-queue)
                                v-children)]
         (when (seq next-queue) (recur next-queue))))
     head)))


(defn- destroy-graph!
  ([graph]
   (destroy-graph! graph nil nil))
  ([graph parent child-index]
   (let [children (-get-children graph)
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
     (-remove-child-at! parent child-index))
   (destroy! graph)
   nil))


(def ^:no-doc ^:dynamic **apply-graph-stack*
  "This is used during the process of applying a new virtual graph to an
  existing graph. A stack is used to ensure children are removed from the end
  first, due to a dependence on using `remove-child-at! `."
  nil)


(defn- add-children-to-stack!
  [graph new-v-graph]
  (let [v-node-children (virtual-node-children new-v-graph)
        graph-children (-get-children graph)
        child-count (max (count v-node-children) (count graph-children))]
    (loop [i 0]
      (when (< i child-count)
        (swap! **apply-graph-stack* conj {:graph (nth graph-children i nil)
                                          :new-v-graph (nth v-node-children i nil)
                                          :graph-parent graph
                                          :parent-child-index i})
        (recur (inc i))))))

(defn- resolve-generator-obj
  "If the graph has a generator of the same type as the new v-graph then
  generate a new v-graph if the generator needs re-rerendering "
  [graph new-v-graph]
  (let [new-props (virtual-node-props new-v-graph)]
    (if (and (virtual-node-is-generator? new-v-graph)
             (satisfies? IGenerator graph)
             (virtual-node-type-desc new-v-graph))
      (if (-regenerate? graph new-props)
        (-generate-virtual-graph graph new-props)
        (-get-virtual-graph graph))
      new-v-graph)))

(defn- apply-virtual-node-diff
  [graph init-new-v-graph graph-parent parent-child-index]
  (let [cur-v-graph (when graph (-get-virtual-graph graph))
        new-v-graph (resolve-generator-obj graph init-new-v-graph)]
    (cond
      ;; The new virtual graph is the same as the old, so no change is required
      ;; for this node or any descendants
      (= new-v-graph cur-v-graph)
      graph

      ;; There is no real node here so create one
      (nil? graph)
      (let [new-graph (reify-virtual-graph* new-v-graph)]
        (when graph-parent (-add-child! graph-parent new-graph))
        new-graph)

      ;; There is no virtual node here so remove and destroy the graph
      (nil? new-v-graph)
      (destroy-graph! graph graph-parent parent-child-index)

      ;; The nodes and properties are the same but there are children that need updates
      (virtual-node= new-v-graph cur-v-graph)
      (do
        (-set-virtual-graph-children! graph (virtual-node-children new-v-graph))
        (add-children-to-stack! graph new-v-graph)
        graph)

      ;; The new virtual node is the same type as the current, so update the
      ;; properties and enqueue children for further compare
      (= (virtual-node-type-desc new-v-graph) (virtual-node-type-desc cur-v-graph))
      (do
        (apply-props! graph (virtual-node-props new-v-graph))
        (-set-virtual-graph-children! graph (virtual-node-children new-v-graph))
        (add-children-to-stack! graph new-v-graph)
        graph)

      ;; The two virtual graph types are different, create the new and replace the old
      (not= (virtual-node-type-desc new-v-graph) (virtual-node-type-desc cur-v-graph))
      (let [new-graph (reify-virtual-graph* new-v-graph)]
        (when (satisfies? IGenerator graph)
          (-copy-generator graph new-graph))
        (when graph-parent
          (-replace-child-at! graph-parent new-graph parent-child-index))
        (destroy-graph! graph)
        new-graph)

      :default
      (throw (js/Error. "This situation can not be dealt with. This is a bug.")))))


(defn- add-dirty-generators!
  "Add all the dirty generators to the apply graph stack"
  ;; XXX: Make this compare all paths and set the dirty flag. It's horribly inefficient but will fix later
  []
  (let [dirty-generators (get-dirty-generators)
        gen-count (count dirty-generators)]
    (loop [i 0]
      (when (< i gen-count)
        (let [graph (nth dirty-generators i)
              new-v-graph (-get-generator-virtual-graph graph)
              graph-parent (-get-parent graph)
              parent-child-index (when graph-parent (-child-index graph-parent graph))]
          (swap! **apply-graph-stack* conj {:graph graph
                                            :new-v-graph new-v-graph
                                            :graph-parent graph-parent
                                            :parent-child-index parent-child-index})
          (recur (inc i)))))))


(defn- apply-virtual-graph!*
  [graph new-v-graph]
  (binding [**apply-graph-stack* (atom [])]
    (let [head-parent (-get-parent graph)
          head-child-index (when head-parent (-child-index head-parent graph))
          head (apply-virtual-node-diff graph new-v-graph head-parent head-child-index)]
      (loop []
        ;; Add all the dirty generators into the re-render stack
        (when (not (seq @**apply-graph-stack*))
          (add-dirty-generators!))
        (when-let [{:keys [graph-parent parent-child-index]
                    next-graph :graph
                    next-new-v-graph :new-v-graph}
                   (peek @**apply-graph-stack*)]
          (apply-virtual-node-diff next-graph next-new-v-graph graph-parent parent-child-index)
          (swap! **apply-graph-stack* pop)
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
  (assert (some? **cur-state*)
          "Can not call `update-in-state` outside of a generator's `:render` method")
  (apply swap! **cur-state* update-in ks f args))


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
  (assert (some? **cur-state*)
          "Can not call `assoc-in-state` outside of a generator's `:render` method")
  (swap! **cur-state* assoc-in ks v))


(defn get-in-state
  "Given a key path, return a value from this render cycle's global state. This
  function is similar to `get-in` only without the first parameter."
  {:doc/format :markdown}
  ([ks]
   (get-in-state ks nil))
  ([ks not-found]
   (assert (and (some? *prev-state*) (some? *observed-key-paths*))
           "Can not call `get-in-state `outside of generator's render function and without a state atom.")
   (let [val (get-in *prev-state* ks not-found)]
     (assoc! *observed-key-paths* ks val)
     val)))


(defn re-render-graph!
  "Given a real graph, and a virtual graph, apply changes in the
  virtual graph to the real graph and return a real graph. If head of the
  virtual graph is the same type of object as the current real graph, the
  same instance is returned. If the cur-graph is nil then a new graph will be
  created. If a state atom is provided it should be a map, if none is provided
  then the state will be empty."
  ([cur-graph new-virtual-graph]
   (re-render-graph! cur-graph new-virtual-graph (atom {})))
  ([cur-graph new-virtual-graph *state]
   (assert (or (nil? *state) (map? @*state))
           "The state must be either nil or a reference to a map")
   (binding [*prev-state* (when *state @*state)
             **cur-state* *state]
     (dirty-generators! *prev-state*)
     (if cur-graph
       (apply-virtual-graph!* cur-graph new-virtual-graph)
       (reify-virtual-graph* new-virtual-graph)))))


(defn reify-virtual-graph
  "Create an object graph from the given virtual graph. If a state atom is
  provided it should be a map, if none is provided then the state will be
  an empty map."
  ([v-graph]
   (reify-virtual-graph v-graph (atom {})))
  ([v-graph *state]
   (re-render-graph! nil v-graph *state)))


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
   (assert (required-object-desc-keys? object-desc)
           (str "The object description map is missing the following required keys: "
                (pp-set (missing-required-object-desc-keys object-desc))))
   (assert (valid-object-desc-keys? object-desc)
           (str "The object description map contains the following invalid keys: "
                (pp-set (invalid-object-desc-keys object-desc))))
   [::object object-desc props children]))


(defn generator-v-node
  "Create a new generator virtual node with the provided generator description
  and optionally, the generator's props."
  ([gen-desc]
   (generator-v-node gen-desc {}))
  ([gen-desc props]
   (assert (required-generator-desc-keys? gen-desc)
           (str "The generator description map is missing the following required keys: "
                (pp-set (missing-required-generator-desc-keys gen-desc))))
   (assert (valid-generator-map? gen-desc)
           (str "The object description map contains the following invalid keys: "
                (pp-set (invalid-generator-desc-keys gen-desc))))
   [::generator gen-desc props nil]))


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
  (let [props (get-existing-object-properties graph (:prop-map object-desc))]
    (extend-graph-obj! graph (object-v-node object-desc props))))


(defn virtual-graph?
  "Is the provided `virtual-graph` a valid virtual graph?"
  [virtual-graph]
  (and (vector? virtual-graph)
       (keyword? (first virtual-graph))
       (map? (second virtual-graph))))


