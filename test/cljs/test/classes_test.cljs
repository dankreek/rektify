(ns test.classes-test
  "Functions for creating test class virtual nodes as well as test to ensure
  the JS test classes work in addition to the object description maps."
  (:require [clojure.test  :refer-macros [deftest is testing run-tests]]
            [test.classes :as classes]
            [clojure.zip :as z]
            [goog.object :as object]))

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


(defn replace-child [obj old-child new-child]
  (.replaceChild obj old-child new-child))


(defn remove-child-at [obj i]
  (.removeChildAt obj i))


(defn remove-child [obj child]
  (.removeChild obj child))


(defn get-children [obj]
  (.getChildren obj))


(defn destroy [obj-desc obj]
  (.destroy obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom getters and setters

(defn get-something-from-red
  [obj _]
  [(.-x obj) (.-y obj) (.-z obj)])


(defn set-something-on-red
  [obj _ [x y z]]
  (.setSomething obj x y z))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object descriptions

(def fishy-function-desc
  {:get-parent get-parent
   :add-child add-child
   :get-children get-children
   :destructor destroy
   :replace-child replace-child
   :remove-child remove-child

   ;; Not used by rektify yet
   ;:child-index child-index
   ;:replace-child-at replace-child-at
   ;:remove-child-at remove-child-at
   })


(def one-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:some-prop {:property "someProp"
;; TODO: Make setter and getter optional w/defaults (Issue #22)
                            :setter object/set
                            :getter object/get}
                :optional {:property "optionalParam"
                           :setter object/set
                           :getter object/get}}
     :constructor classes/OneFish
     :constructor-list [[:optional] []]
     :default-props {:some-prop false}}))


(def two-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:in-the-beginning {:property "first"
                                   :setter object/set
                                   :getter object/get}
                :and-then {:property "second"
                           :setter object/set
                           :getter object/get}}
     :constructor classes/TwoFish
     :post-constructor (fn [obj-desc obj& init-props]
                         (set! (.-postConstructorObjDesc obj&) obj-desc)
                         (set! (.-postConstructorProps obj&) init-props))
     :constructor-list [[:in-the-beginning :and-then]]
     :default-props {:in-the-beginning "it was before the end."}}))


(def red-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:something {:getter get-something-from-red
                            :setter set-something-on-red}}
     :constructor classes/RedFish}))


(def blue-fish-desc
  (merge
    fishy-function-desc
    {:prop-map {:kind-of-blue {:property "kindOfBlue"
                               :setter object/set
                               :getter object/get}}
     :constructor classes/BlueFish}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test tree manipulation methods of test classes

(deftest js-methods
  (let [f0 (new classes/Fish)
        f1 (new classes/Fish)
        f2 (new classes/Fish)]

    (testing "add and remove children by index"
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

  (testing "replace a child at index"
    (let [f0 (new classes/Fish)
          f1 (new classes/Fish)
          f2 (new classes/Fish)]
      (.addChild f0 f1)
      (.addChild f0 f1)
      (.replaceChildAt f0 f2 0)
      (is (= f2 (.getChildAt f0 0)))
      (.replaceChildAt f0 f2 1)
      (is (= f2 (.getChildAt f0 1)))))

  (testing "replace a child by reference"
    (let [f0 (new classes/Fish)
          f1 (new classes/Fish)
          f2 (new classes/Fish)
          f3 (new classes/Fish)]
      (.addChild f0 f1)
      (.addChild f0 f2)
      (.replaceChild f0 f1 f3)
      (is (= f3 (.getChildAt f0 0)))
      (.replaceChild f0 f2 f1)
      (is (= f1 (.getChildAt f0 1)))))

  (testing "destroying an object sets the destroyed flag"
    (let [f0 (new classes/Fish)]
      (.destroy f0)
      (is (= true (.isDestroyed f0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions

(defn fish-zip
  "Create a zipper over a tree of PixiJS objects."
  [head-object]
  (z/zipper (constantly true)
            (fn [o] (seq (.getChildren o)))
            (fn [node children]
              (doseq [child children]
                (.addChild node child)))
            head-object))
