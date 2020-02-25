(ns rektify.generator
  "
  # Generator creation, manipulation and generation for Rektify.

  A generator is a Clojure map which contains a descriptor with lifecycle
  functions, a map of properties and a list of v-node children. A generator's
  v-node children are only there for the generator itself to insert v-nodes
  into its generated v-tree where it needs to.

  The generator map has the following keys in it:

  ```clojure
  {:rektify/type ::generator   ;; internal identifier
   ::desc {}                   ;; descriptor with lifecycle functions
   ::props {}                  ;; generator's properties
   ::children []}              ;; a list of v-nodes for the generate function
  ```

  A generator's descriptor has the following keys defining lifecycle functions
  with the documented signatures:

  * `generate`: `[props children local-state]`: Generate a v-tree which will be
    later reified/rektified during a generation pass. The v-tree can also
    contain other generators.

  During execution of a generator's lifecycle functions the current local-state
  will be passed in as an immutable map. If the generator needs to mutate its
  state the state manipulation functions can be used to do so. The results
  of all local-state manipulation functions will be passed in during the next
  generation.
  "
  (:require [clojure.zip :as z]
            [rektify.virtual-tree :as vt]
            [clojure.set :as set]
            [clojure.string :as string]))


(def required-lifecycle-fns
  "Required generator lifecycle functions"
  #{:generate})


(defn valid-desc?
  "Is `gen-desc` a valid generator descriptor?"
  [gen-desc]
  (and (map? gen-desc)
       (fn? (:generate gen-desc))))


(defn invalid-gen-desc-msg
  "Return an error message describing why a generator descriptor is invalid"
  [gen-desc]
  (if (not (map? gen-desc))
    "gen-desc must be a map of lifecycle functions"
    (let [missing-required (set/difference required-lifecycle-fns (keys gen-desc))]
      (cond
        missing-required
        (str "gen-desc is missing required lifecycle functions: "
             (string/join ", " missing-required))))))


(defn generator
  "Return a new generator map, given a generator descriptor and
  optionally its properties and a list of children"
  ([gen-desc]
   (generator gen-desc {}))
  ([gen-desc props]
   (generator gen-desc props []))
  ([gen-desc props children]
   (assert (valid-desc? gen-desc) (invalid-gen-desc-msg gen-desc))
   (assert (map? props) "props must be a map")
   (assert (sequential? children) "children must be sequential")
   ;; XXX: validate the descriptor and ensure there is a generate key w/func value
   {:rektify/type ::generator
    ::desc gen-desc
    ::props props
    ::children children}))


(defn generator?
  "Is a valid generator map?"
  [gen]
  (= ::generator (:rektify/type gen)))


(defn valid-generator?
  "Is `gen` a generator with valid properties and children?"
  [gen]
  (and (map? gen)
       (generator? gen)
       (and (= ::generator (:rektify/type gen))
            (map? (::desc gen))
            (map? (::props gen))
            (sequential? (::children gen)))))


(defn props
  "Return the generator's properties"
  [gen]
  (::props gen))


(defn desc
  "Return the generator's descriptor"
  [gen]
  (::desc gen))


(defn children
  "Gets the sequence of children from the generator"
  [gen]
  (::children gen))


(defn update-props
  "Returns a generator with its properties update to the `new-props` map"
  [gen new-props]
  (assert (generator? gen))
  (assert (map? new-props))
  (assoc gen ::props new-props))


(defn update-children
  "Return a generator with the same type and props with its children
  changed to the new child list"
  [gen new-children]
  (assert (generator? gen))
  (assert (sequential? new-children))
  (assoc gen ::children new-children))


(defn same-types?
  "Are the generators the same type?"
  [gen1 gen2]
  (= (::desc gen1) (::desc gen2)))


(defn generated?
  "Has the generator generated a v-tree?"
  [gen]
  (contains? (meta gen) ::v-tree))


(defn state
  "Get the generator's state map"
  [gen]
  (get (meta gen) ::state))


(defn gen-children
  "Get a generated generator's children, not its v-tree children."
  [gen]
  (::gen-children (meta gen)))


(defn ^:private assoc-meta
  "Associate the key and value with an existing val's metadata and return it
  with modified metadata."
  [gen k v]
  (with-meta gen (assoc (meta gen) k v)))


(defn ^:private update-gen-children
  "Add or replace a list of generator children for a generator."
  [gen gen-children]
  (assoc-meta gen ::gen-children gen-children))


(defn gen-zip
  "Creates a zipper over a generator and its generator children, not its
  v-tree children."
  [gen]
  (z/zipper generator? gen-children update-gen-children gen))


(defn v-tree
  "Get a generator's v-tree"
  [gen]
  (::v-tree (meta gen)))


(defn rekt-v-tree
  "Return the rektified v-tree from the root generator."
  [gen]
  (::rekt-v-tree (meta gen)))


(defn obj&
  "Get the object at the head of the generator's rektified v-tree"
  [gen]
  (vt/obj& (rekt-v-tree gen)))


(defn ^:private merge-meta
  [gen new-metadata]
  (with-meta gen (merge (meta gen) new-metadata)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator state manipulation

(def ^:dynamic *active-gen-state*
  "State of the current generator being generated wrapped in an atom. This
  allows for state mutation inside of lifecycle functions, but the mutated
  state will not be passed into the generator until the next generation cycle."
  (atom nil :validator #(throw (js/Error. "Generator state can not be modified outside of a lifecycle function."))))


(defn swap-state!
  "Like the Clojure `swap!` function, executes a function which will be provided
  the current generator state and return a new state."
  [f & args]
  (swap! *active-gen-state* #(apply f % args))
  nil)


(defn reset-state!
  "Reset the generator's state to the provided value."
  [new-state]
  (reset! *active-gen-state* new-state)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation functionality

(defn ^:private call-generate
  [gen cur-state]
  (let [generate-fn (:generate (desc gen))]
    (generate-fn (props gen) (children gen) cur-state)))


(defn assemble-generated-v-tree
  "Walk the generator and its children and return its full v-tree"
  [gen]
  ;; Ignore children for now and just return head v-tree
  (v-tree gen))


(defn ^:private generate*
  [gen prev-gen]
  (assert (valid-generator? gen) "gen is not a valid generator")
  (assert (not (generated? gen)) "gen must not be previously generated")
  (assert (or (nil? prev-gen) (and (generator? prev-gen) (generated? prev-gen)))
          "prev-gen must either be nil or valid and previously generated")

  ;; XXX: Check to see if the new generator is a different type
  ;; If prev-gen has not been generated, use the default of an empty map
  (let [cur-state (or (state prev-gen) {})]
    ;; Wrap cur-state in an atom and bind it for mutation during generation
    (binding [*active-gen-state* (atom cur-state)]
      (let [new-v-tree (call-generate gen cur-state)]
        ;; attach updated state and new v-tree
        (merge-meta gen {::state (deref *active-gen-state*)
                         ::v-tree new-v-tree})))))


(defn generate
  ([gen] (generate gen nil))
  ([gen prev-gen]
   (let [generated-gen (generate* gen prev-gen)
         prev-v-tree (rekt-v-tree prev-gen)
         new-v-tree (assemble-generated-v-tree generated-gen)]
     ;; Add the rektified full v-tree to the head node's metadata
     (assoc-meta generated-gen ::rekt-v-tree (vt/rektify new-v-tree prev-v-tree)))))

