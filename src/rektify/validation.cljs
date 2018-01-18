(ns rektify.validation
  "Definitions and functions used to validate parameters passed to public
  functions."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rektify.virtual-graph :as v-graph]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(def required-generator-life-cycles
  #{:generate})


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

(def ^:no-doc valid-obj-desc-keys
  (set/union required-obj-desc-keys optional-obj-desc-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameter validator functions

(defn- required-map-keys?
  "Check that all keys in the `required-key-set `are in the given `key-set `."
  [required-key-set m]
  (= required-key-set (set/intersection (set (keys m)) required-key-set)))


(defn- missing-required-keys
  [required-key-set m]
  (set/difference required-key-set (set (keys m))))


(defn- valid-keys?
  "Check that all keys in `key-set `are in the `valid-key-set `."
  [valid-key-set m]
  (set/subset? (set (keys m)) valid-key-set))


(defn- invalid-keys
  "Return the set of keys in the given map that are not valid"
  [valid-key-set m]
  (set/difference (set (keys m)) valid-key-set))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(def ^:no-doc required-object-desc-keys?
  (partial required-map-keys? required-obj-desc-keys))


(def ^:no-doc required-generator-desc-keys?
  (partial required-map-keys? required-generator-life-cycles))


(def ^:no-doc missing-required-object-desc-keys
  (partial missing-required-keys required-obj-desc-keys))


(def ^:no-doc missing-required-generator-desc-keys
  (partial missing-required-keys required-generator-life-cycles))


(def ^:no-doc valid-object-desc-keys?
  (partial valid-keys? valid-obj-desc-keys))


(def ^:no-doc invalid-generator-desc-keys
  (partial invalid-keys required-generator-life-cycles))


(def ^:no-doc invalid-object-desc-keys
  (partial invalid-keys valid-obj-desc-keys))


(def ^:no-doc valid-generator-map?
  (partial valid-keys? valid-generator-map-keys))


(defn ^:no-doc pp-set
  "Return a string with the provided set's contents comma-separated."
  [s]
  (str/join ", " s))


(defn virtual-node?
  "Is the provided `virtual-node` a valid virtual node?"
  [virtual-node]
  (and (vector? virtual-node)
       (let [node-type (first virtual-node)
             type-desc (second virtual-node)
             props (nth virtual-node 2 nil)
             children (nth virtual-node 3 nil)]
         (and (keyword? node-type)
              (if (= node-type v-graph/gen-key)
                (or (map? type-desc) (fn? type-desc))
                (map? type-desc))
              (map? props)
              (or (nil? children) (seq? children))))))

