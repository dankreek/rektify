(ns rektify.generators
  (:require [rektify.validation :as v]
            [rektify.graph :as g]))

;; XXX: Move generator-specific validation code into this namespace
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


(defprotocol ^:no-doc IGenerator
  "Methods defining behavior of a virtual graph generator."

  (regenerate?
    [this props]
    "Does this generator need to generate a new v-graph given the provided
    props?")

  (set-dirty
    [this]
    "Sets the dirty flag on this generator, indicating that is needs to be
    regenerated.")

  (set-root-obj
    [this root-obj]
    "Sets the reference to this generator's root object. Returns this.")

  (dirty?
    [this]
    "Does this generator need to be regenerated, even if the props have not
    changed?")

  (generate
    [this props]
    "Generate a virtual graph with the given props")

  (v-graph
    [this]
    "Get this generator's most recent virtual graph")

  (cleanup
    [this]
    "If this generator has a cleanup function call it"))


(defprotocol ^:no-doc IGenerated
   "Methods defined on this root of an object tree which was generated."

  (generator
    [this]
    "Return the reference to the generator that generated this object tree."))


(deftype Generator [*state]
  IGenerator
  (dirty? [this] (:dirty? @*state))

  (regenerate?
    [this props]
    (or (dirty? this)
        (not= props (:props @*state)))
    ;; TODO: Account for global state changed
    )

  (set-dirty [this]
    (swap! *state assoc :dirty? true))

  (set-root-obj [this obj]
    (swap! *state assoc :root-obj obj)
    this)

  (generate
    [this props]
    (let [{:keys [generate-fn obj-root]} @*state
          v-graph (generate-fn props obj-root)]
      ;; TODO: Assert that returned v-graph is valid
      (swap! *state assoc
             :dirty? false
             :v-graph v-graph
             :props props)
      v-graph))

  (v-graph [this] (get @*state :v-graph))

  (cleanup
    [this]
    (let [{:keys [cleanup-fn obj-root]} @*state]
      (when (some? cleanup-fn)
        (cleanup-fn obj-root))))

  g/IGraphNode
  (-add-child!
    [this new-child]
    (swap! *state update-in [:children] conj new-child)))


(defn create-generator [generator-desc parent]
  (let [generator-fns (resolve-generator generator-desc)
        {:keys [generate cleanup]} generator-fns
        init-state {:generator-desc generator-desc
                    :generate-fn generate
                    :cleanup-fn cleanup
                    :props {}
                    :dirty? true
                    :root-obj nil
                    :v-graph nil
                    :parent parent
                    :children []}
        *state (atom init-state)]
    (->Generator *state)))


(defn link-root-obj-to-gen
  "Links the provided root object to the generator."
  [obj gen]
  (specify! obj
    IGenerated
    (generator [this] gen)))
