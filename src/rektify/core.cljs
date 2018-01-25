(ns rektify.core
  (:require [rektify.rektify :as rekt]))


(defn update-in-state
  [ks f & args]
  (apply swap! rekt/*cur-local-state** update-in ks f args))


(defn assoc-state
  ([key val]
    (swap! rekt/*cur-local-state** assoc key val))
  ([key val & kvs]
    (apply swap! rekt/*cur-local-state** assoc key val kvs)))


(defn assoc-in-state
  [ks val]
  (swap! rekt/*cur-local-state** assoc-in ks val))


(defn dissoc-state
  ([] (swap! rekt/*cur-local-state** dissoc))
  ([key] (swap! rekt/*cur-local-state** dissoc key))
  ([key & ks] (apply swap! rekt/*cur-local-state** key ks)))


(defn swap-state
  ([f] (swap! rekt/*cur-local-state** f))
  ([f & args] (apply swap! rekt/*cur-local-state** f args)))
