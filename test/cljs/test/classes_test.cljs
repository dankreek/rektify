(ns test.classes-test
  "Functions for creating test class virtual nodes as well as test to ensure
  the JS test classes work in addition to the object description maps."
  (:require [clojure.test  :refer-macros [deftest is testing]]
            [test.classes :as classes]
            [clojure.zip :as z]
            [rektify.core :as rekt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(def OneFish classes/OneFish)
(def TwoFish classes/TwoFish)
(def RedFish classes/RedFish)
(def BlueFish classes/BlueFish)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree navigation functions for all test classes

(def get-parent #(.getParent %))


(defn add-child [obj new-child]
  (.addChild obj new-child))


(defn child-index [obj i]
  (.getChildIndex obj i))


(defn replace-child-at [obj new-child i]
  (.replaceChildAt obj new-child i))


(defn remove-child-at [obj i]
  (.removeChildAt obj i))


(defn get-children [obj]
  (.getChildren obj))


(defn destroy [obj]
  (.destroy obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customer getters and setters

(defn get-something-from-red
  [obj _]
  [(.-x obj) (.-y obj) (.-z obj)])


(defn set-something-on-red
  [obj _ [x y z]]
  (.setSomething obj x y z))


(def fishy-function-desc
  {:get-parent get-parent
   :add-child add-child
   :child-index child-index
   :replace-child-at replace-child-at
   :remove-child-at remove-child-at
   :get-children get-children
   :destructor destroy})


(def one-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:some-prop {:property "someProp" :setter aset :getter aget}}
     :constructor classes/OneFish
     :default-props {:some-prop false}}))


(def two-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:in-the-beginning {:property "first" :setter aset :getter aget}
                :and-then {:property "second" :setter aset :getter aget}}
     :constructor classes/TwoFish
     :post-constructor #(set! (.-postConstructorCalled %) true)
     :constructor-list [[:in-the-beginning :and-then]]
     :default-props {:in-the-beginning "it was before the end."}}))


(def red-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:something {:getter get-something-from-red
                            :setter set-something-on-red}}
     :constructor classes/RedFish
     :default-props {:something [0 1 -1]}}))


(def blue-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {} ; TODO: This should be optional (Issue #17)
     :constructor classes/BlueFish
     :constructor-list [[]]}))


(defn one-fish
  ([] (one-fish {}))
  ([props & children]
   (rekt/object-v-node one-fish-desc props children)))


(defn red-fish
  ([] (red-fish {}))
  ([props & children]
   (rekt/object-v-node red-fish-desc props children)))


(defn blue-fish
  ([] (blue-fish {}))
  ([props & children]
   (rekt/object-v-node blue-fish-desc props children)))


(deftest test-classes-work
  (let [f0 (new classes/Fish)
        f1 (new classes/Fish)
        f2 (new classes/Fish)]
    (testing "can add and remove children"
      (is (= 0 (count (.getChildren f0))))
      (is (nil? (.getParent f1)))
      (.addChild f0 f1)
      (is (= f0 (.getParent f1)))
      (is (= 1 (count (.getChildren f0))))
      (.addChild f0 f2)
      (is (= 2 (count (.getChildren f0))))
      (.removeChild f0 f1)
      (is (= 1 (count (.getChildren f0))))
      (is (nil? (.getParent f1)))
      (is (= f2 (.getChildAt f0 0)))
      (.removeChildAt f0 0)
      (is (= 0 (count (.getChildren f0))))))

  (testing "can replace a child"
    (let [f0 (new classes/Fish)
          f1 (new classes/Fish)
          f2 (new classes/Fish)]
      (.addChild f0 f1)
      (.addChild f0 f1)
      (.replaceChildAt f0 f2 0)
      (is (= f2 (.getChildAt f0 0)))
      (.replaceChildAt f0 f2 1)
      (is (= f2 (.getChildAt f0 1)))))

  (testing "destroying an object sets the destroyed flag"
    (let [f0 (new classes/Fish)]
      (.destroy f0)
      (is (= true (.isDestroyed f0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn fish-zip
  "Create a zipper over a tree of PixiJS objects."
  [head-object]
  (z/zipper (constantly true)
            (fn [o] (seq (.getChildren o)))
            (fn [node children]
              (doseq [child children]
                (.addChild node child)))
            head-object))
