(ns rektify.generator
  "
  # Generator helper functions

  A generator is type of virtual node that generates virtual trees which will
  later be reified into object trees. A generator virtual node contains a
  description, property map and children. Any state which needed to carry over
  for rektification is kept in the virtual node's metadata. This state map is
  further explained in the `rektify.rektify` namespace.

  During rektification the generator node's properties, children and local state
  are all passed into the various generator functions, object trees are created
  and updated, and then all the state that was updated it updated in the
  generator node and returned.

  During any lifecycle method the generator's local state and the global state
  are all immutable. An atom local to the rektify namespace will exist ot allow
  mutation of local state but it the changes won't be visible to the generator
  until the next lifecycle function is called.

  The lifecycle functions are defined in a generator description which is a
  map with the following keys which define functions. The functions signatures
  are listed following the key:

  * `:init` `[props]` _(optional)_: Called when a generator is about
    to generate its first v-tree. The props argument is the property map which
    is defined on the v-node which contains the generator description. Init is
    expected to return either `nil` or a map containing the generator's initial
    local state.

  * `:generate` `[props state children]`: Called when a new v-tree needs to be
    generated. The props come from the containing v-node, the state is the
    generator's local state before generate was called and children is a tree of
    child v-nodes which this generator's output is expected to contain somewhere,
    though they can be completely thrown away for filtered if need be. Children
    could possibly be `nil`. Returns a virtual graph which will be reified
    immediately after this function generate returns.

  * `:post-generate` `[props state object-tree&]` _(optional)_: Called after
    the v-tree returned from the generate function has been reified or rektified.
    The props and state are the generator's props and state at the time
    post-generate is called. The `object-tree&` argument is a reference to the
    top-most object which was either reified or rektified. If the generated
    virtual-tree contains no real objects, this will be `nil`.

  * `:pre-destroy` `[props state object-tree&]` _(optional)_: Called before a
    generator is about to be removed from the virtual-tree during rektification.
    The arguments are the same as those passed into `:post-generate`. The
    object tree will be manually destroyed during the rektification process, so
    it is not necessary to do any of that here.
  "
  (:require [rektify.virtual-graph :as vg]
            [rektify.validation :as valid]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def ^:private required-generator-methods
  #{:generate})


(def ^:private optional-generator-methods
  #{:init
    :post-generate
    :pre-destroy})


(def ^:private valid-generator-methods
  (set/union required-generator-methods optional-generator-methods))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn generator-desc?
  "Is this map a valid generator description?"
  [generator]
  (and (map? generator)
       (let [gen-key-set (set (keys generator))]
         (and (valid/valid-keys? valid-generator-methods gen-key-set)
              (valid/required-keys? required-generator-methods gen-key-set)))))


(defn init
  "Given a generator descriptor, call its init method with the provided props.

  Return its return value."
  [gen-desc props]
  (when-let [init-fn (get gen-desc :init)]
    (init-fn props)))


(defn generate
  "Given a generator description, call the generate function with the provided
  props, local-state child children.

  Return the generated v-tree."
  [gen-desc props local-state children]
  (let [generate-fn (get gen-desc :generate)]
    (generate-fn props local-state children)))


(defn post-generate
  "Given a generator description, call its post-generate function with the
  provided props, local state and object reference. If no post-generate function
  exists no operation is performed."
  [gen-desc props local-state obj-tree&]
  (when-let [post-generate-fn (get gen-desc :post-generate)]
    (post-generate-fn props local-state obj-tree&)))


(defn pre-destroy
  [gen-desc props local-state obj-tree&]
  (when-let [pre-destroy-fn (get gen-desc :pre-destroy)]
    (pre-destroy-fn props local-state obj-tree&)))


;; TODO: put these functions directly in core, the *cur-local-state** atom will probaly end up in the rektify namespace
;(def update-in-state
;  (partial swap! *cur-local-state** update-in))
;
;
;(def assoc-state
;  (partial swap! *cur-local-state** assoc))
;
;
;(def assoc-in-state
;  (partial swap! *cur-local-state** assoc-in))
;
;
;(def dissoc-state
;  (partial swap! *cur-local-state** dissoc))
;
;
;(def swap-state
;  (partial swap! *cur-local-state**)
