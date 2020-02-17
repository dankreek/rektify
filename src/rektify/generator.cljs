(ns rektify.generator
  (:require [clojure.zip :as z]))


(defn generator
  "Return a new generator map, given a generator descriptor and
  optionally its properties and a list of children"
  ([gen-desc]
   (generator gen-desc {}))
  ([gen-desc props]
   (generator gen-desc props []))
  ([gen-desc props children]
   (assert (map? gen-desc) "gen-desc must be a map")
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
  (if (map? gen)
    (and (= ::generator (:rektify/type gen))
         (map? (::desc gen))
         (map? (::props gen))
         (sequential? (::children gen)))
    false))


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


(defn with-state
  "Return the provided generator with the state replaced or added to it"
  [gen state]
  (assert (or (map? state) (nil? state))
          "A generator's state must be either a map or nil")
  (with-meta gen state))

(defn same-types?
  "Are the gnerators the same type?"
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


(defn gen-zip
  "Creates a zipper over a generator and its children"
  [gen]
  (z/zipper generator? children update-children gen))


(defn v-tree
  "Get a generator's v-tree"
  [gen]
  (::v-tree (meta gen)))


(defn ^:private update-v-tree
  [gen v-tree]
  (with-meta gen (assoc (meta gen) ::v-tree v-tree)))


(defn ^:private update-gen-children
  [gen gen-children]
  (with-meta gen (assoc (meta gen) ::gen-children gen-children)))


(defn update-state
  [gen new-state]
  "Update a generator's local sate with a new state map"
  (with-meta gen (assoc (meta gen) ::state new-state)))


(defn generate
  ([gen] (generate [gen nil]))
  ([gen prev-gen]
   (assert (generator? gen) "gen is not a valid generator")
   (assert (not (generated? gen)) "gen must not be previously generated")
   (assert (or (nil? prev-gen)
               (and (generator? prev-gen)
                    (generated? prev-gen)))
           "prev-gen must either be nil or valid and previously generated")
   (let [gen-fn (::generate (desc gen))
         ;; XXX: define the signature of the generate function
         new-vtree (gen-fn (props gen) ())]

     )
   )


  )

