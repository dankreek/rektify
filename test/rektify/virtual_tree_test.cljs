(ns rektify.virtual-tree-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [test.classes-test :as classes]
            [clojure.zip :as z]
            [rektify.virtual-tree :as vt]))


(deftest v-node-create-and-test
  (let [o1 (vt/object classes/one-fish-desc)
        o2 (vt/object classes/one-fish-desc {})
        o3 (vt/object classes/one-fish-desc {} [])]

    (testing "test object v-nodes"
      (is (vt/object? o1))
      (is (vt/object? o2))
      (is (vt/object? o3)))

    (testing "test all v-nodes"
      (is (vt/node? o1))
      (is (vt/node? o2))
      (is (vt/node? o3)))))


(deftest props
  (testing "returns props"
    (let [props {:a 1}
          vn (vt/object classes/one-fish-desc props)]
      (is (= props (vt/props vn))))))


(deftest type-desc
  (testing "returns the node type description"
    (let [vn (vt/object classes/one-fish-desc {})]
      (is (= classes/one-fish-desc (vt/type-desc vn))))))


(deftest children
  (testing "returns the child list"
    (let [child (vt/object classes/one-fish-desc)
          parent (vt/object classes/two-fish-desc {} [child])]
      (is (= [child] (vt/children parent)))))

  (testing "returns no children if none created"
    (let [node (vt/object classes/one-fish-desc)]
      (is (= 0 (count (vt/children node)))))))


(deftest update-props
  (testing "props are updated"
    (let [init-props {:a 1}
          next-props {:b 2}
          v0 (vt/object classes/one-fish-desc init-props)
          v1 (vt/update-props v0 next-props)]
      (is (= next-props (vt/props v1))))))


(deftest update-children
  (testing "updates the child node list"
    (let [child1 (vt/object classes/one-fish-desc)
          child2 (vt/object classes/one-fish-desc)
          parent (vt/object classes/one-fish-desc)]
      (is (= [child1 child2])
          (vt/children (vt/update-children parent [child1 child2]))))))


(deftest node=
  (testing "two nodes are equivalent, disregarding their children"
    (let [node-1 (vt/object classes/one-fish-desc)
          node-2 (vt/object classes/one-fish-desc {:tt 1})
          node-3 (vt/object classes/two-fish-desc)]
      (is (vt/node= node-1 (vt/update-children node-1 [node-2])))
      (is (not (vt/node= node-1 node-2)))
      (is (not (vt/node= node-1 node-3))))))


(deftest write-and-read-state
  (let [node-1 (vt/object classes/one-fish-desc)
        state {:tt 1, :tootoo 3}]
    (is (nil? (vt/state node-1)))
    (is (= state (vt/state (vt/with-state node-1 state))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions

(defn v-tree-zip
  "Create a zipper over a tree of virtual nodes"
  [head-v-node]
  (z/zipper vt/node? vt/children vt/update-children head-v-node))
