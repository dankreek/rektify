(ns rektify.rektify-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [test.classes-test :as classes :refer [fish-zip]]
            [rektify.virtual-tree-test :refer [v-tree-zip]]
            [rektify.virtual-tree :as vt]
            [rektify.object :as o]
            [clojure.zip :as z]
            [rektify.rektify :as rekt]))

(def single-node (vt/object classes/one-fish-desc))

(def depth-1-tree
  (vt/object classes/one-fish-desc {}
             [(vt/object classes/two-fish-desc {:in-the-beginning "was darkness"
                                                :and-then "along came Ra"})
              (vt/object classes/red-fish-desc)
              (vt/object classes/blue-fish-desc)]))

(def depth-2-tree
  (vt/object classes/one-fish-desc {}
             [(vt/object classes/red-fish-desc {}
                         [(vt/object classes/one-fish-desc)
                          (vt/object classes/red-fish-desc)])
              (vt/object classes/blue-fish-desc {}
                         [(vt/object classes/two-fish-desc {:in-the-beginning "was darkness"
                                                            :and-then "along came Ra!"})])]))

(deftest reify-v-tree
  (testing "reify a single v-node"
    (let [reified-v-node (rekt/reify-v-tree single-node)
          obj& (rekt/obj& reified-v-node)]
      (is (rekt/reified? reified-v-node) "v-node was reified")
      (is (instance? classes/OneFish obj&) "obj& is instance of OneFish")))

  (testing "reify a v-node with children"
    (let [reified-v-tree (rekt/reify-v-tree depth-1-tree)
          reified-z (v-tree-zip reified-v-tree)
          root& (rekt/obj& reified-v-tree)
          root&-z (fish-zip root&)]
      
      (testing ": all v-nodes were reified"
        (is (rekt/reified? reified-v-tree) "v-tree root was reified")
        (is (rekt/reified? (-> reified-z z/down z/node)) "v-tree first child was reified")
        (is (rekt/reified? (-> reified-z z/down z/right z/node)) "v-tree second child was reified")
        (is (rekt/reified? (-> reified-z z/down z/right z/right z/node)) "v-tree third child was reified"))

      (testing ": all reified nodes have correct objects"
        (is (instance? classes/OneFish root&) "root& is instance of OneFish")
        (is (instance? classes/TwoFish (rekt/obj& (-> reified-z z/down z/node))))
        (is (instance? classes/RedFish (rekt/obj& (-> reified-z z/down z/right z/node))))
        (is (instance? classes/BlueFish (rekt/obj& (-> reified-z z/down z/right z/right z/node)))))

      (testing ": all child objects are connected to parent"
        (is (instance? classes/TwoFish (-> root&-z z/down z/node))
            "first child is instance of TwoFish")
        (is (instance? classes/RedFish (-> root&-z z/down z/right z/node))
            "second child is instance of RedFish")
        (is (instance? classes/BlueFish (-> root&-z z/down z/right z/right z/node))
            "third child is instance of BlueFish"))))

  (testing "reify a v-node with grandchildren"
    (let [reified-v-tree (rekt/reify-v-tree depth-2-tree)
          reified-z (v-tree-zip reified-v-tree)
          root& (rekt/obj& reified-v-tree)
          root&-z (fish-zip root&)]

      (testing ": root and all grandchild v-nodes were reified"
        (is (rekt/reified? reified-v-tree) "v-tree root was reified")
        (is (rekt/reified? (-> reified-z z/down z/down z/node)))
        (is (rekt/reified? (-> reified-z z/down z/down z/right z/node)))
        (is (rekt/reified? (-> reified-z z/down z/right z/down z/node))) )

      (testing ": all grandchild nodes have correct objects"
        (is (instance? classes/OneFish (rekt/obj& (-> reified-z z/down z/down z/node))))
        (is (instance? classes/RedFish (rekt/obj& (-> reified-z z/down z/down z/right z/node))))
        (is (instance? classes/TwoFish (rekt/obj& (-> reified-z z/down z/right z/down z/node)))))

      (testing ": all objects are connected to their parents"
        (is (instance? classes/OneFish root&))
        (is (instance? classes/RedFish (-> root&-z z/down z/node)))
        (is (instance? classes/BlueFish (-> root&-z z/down z/right z/node)))
        (is (instance? classes/OneFish (-> root&-z z/down z/down z/node)))
        (is (instance? classes/RedFish (-> root&-z z/down z/down z/right z/node)))
        (is (instance? classes/TwoFish (-> root&-z z/down z/right z/down z/node)))))))


(deftest destroy-v-node
  (testing "single node v-tree is destroyed"
    (let [v-node (rekt/reify-v-tree depth-1-tree)
          obj& (rekt/obj& v-node)]
      (rekt/destroy-v-node! v-node)
      (is (.isDestroyed obj&) "single node is destroyed")))

  (testing "entire v-tree is destroyed"
    (let [v-tree (rekt/reify-v-tree depth-1-tree)
          root& (rekt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          second-child& (-> root&-z z/down z/right z/node)
          third-child& (-> root&-z z/down z/right z/right z/node)]
      (rekt/destroy-v-node! v-tree)

      (is (.isDestroyed root&))
      (is (.isDestroyed first-child&))
      (is (.isDestroyed second-child&))
      (is (.isDestroyed third-child&))))

  (testing "leaf of v-tree is destroyed"
    (let [v-tree (rekt/reify-v-tree depth-1-tree)
          first-child (first (vt/children v-tree))
          root& (rekt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          second-child& (-> root&-z z/down z/right z/node)
          third-child& (-> root&-z z/down z/right z/right z/node)]
      (rekt/destroy-v-node! first-child v-tree)

      (is (= 2 (alength (.getChildren root&))))
      (is (not (.isDestroyed root&)))
      (is (.isDestroyed first-child&))
      (is (not (.isDestroyed second-child&)))
      (is (not (.isDestroyed third-child&)))))

  (testing "subtree of v-tree is destroyed"
    (let [v-tree (rekt/reify-v-tree depth-2-tree)
          first-child (first (vt/children v-tree))
          root& (rekt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          first-grandchild& (-> root&-z z/down z/down z/node)
          second-grandchild& (-> root&-z z/down z/down z/right z/node)]
      (rekt/destroy-v-node! first-child v-tree)

      (is (= 1 (alength (.getChildren root&))))
      (is (instance? classes/BlueFish (-> root&-z z/down z/node)))
      (is (= 1 (alength (.getChildren (-> root&-z z/down z/node)))))
      (is (instance? classes/TwoFish (-> root&-z z/down z/down z/node)))

      (is (.isDestroyed first-child&))
      (is (.isDestroyed first-grandchild&))
      (is (.isDestroyed second-grandchild&)))))

(deftest rektify-v-tree
  (testing "reifies a non-reified v-tree")
  (testing "updates properties")
  (testing "adds new objects")
  (testing "destroys removed objects"
    (testing "destroys entire tree if nil passed in"))
  )

