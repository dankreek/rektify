(ns rektify.validation
  "Definitions and functions used to validate parameters passed to functions."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rektify.virtual-tree :as v-graph]))


(defn- required-keys?
  "Check that all keys in the `required-key-set `are in the given `key-set `."
  [required-key-set key-set]
  (= required-key-set (set/intersection key-set required-key-set)))


(defn- missing-required-keys
  "Return the set of missing required keys from key-set"
  [required-key-set key-set]
  (set/difference required-key-set key-set))


(defn- valid-keys?
  "Check that all keys in `key-set `are in the `valid-key-set `."
  [valid-key-set key-set]
  (set/subset? key-set valid-key-set))


(defn- invalid-keys
  "Return the set of keys in the given map that are not valid"
  [valid-key-set key-set]
  (set/difference key-set valid-key-set))


(defn pp-set
  "Return a string with the provided set's contents comma-separated."
  [s]
  (str/join ", " s))


