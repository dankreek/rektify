(ns rektify.virtual-tree-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [test.classes-test :as classes]
            [rektify.virtual-tree :as vg]))


(deftest v-node-create-and-test
  (let [o1 (vg/object classes/one-fish-desc)
        o2 (vg/object classes/one-fish-desc {})
        o3 (vg/object classes/one-fish-desc {} [])

        g1 (vg/generator {})
        g2 (vg/generator {} {})
        g3 (vg/generator {} {} [])]

    (testing "test object v-nodes"
      (is (vg/object? o1))
      (is (vg/object? o2))
      (is (vg/object? o3)))

    (testing "test generator v-nodes"
      (is (vg/generator? g1))
      (is (vg/generator? g2))
      (is (vg/generator? g3)))

    (testing "test all v-nodes"
      (is (vg/node? o1))
      (is (vg/node? o2))
      (is (vg/node? o3))
      (is (vg/node? g1))
      (is (vg/node? g2))
      (is (vg/node? g3)))))


(deftest props
  (testing "returns props"
    (let [props {:a 1}
          vn (vg/object classes/one-fish-desc props)]
      (is (= props (vg/props vn))))))


(deftest type-desc
  (testing "returns the node type description"
    (let [vn (vg/object classes/one-fish-desc {})]
      (is (= classes/one-fish-desc (vg/type-desc vn))))))


(deftest children
  (testing "returns the child list"
    (let [child (vg/object classes/one-fish-desc)
          parent (vg/object classes/two-fish-desc {} [child])]
      (is (= [child] (vg/children parent)))))

  (testing "returns no children if none created"
    (let [node (vg/object classes/one-fish-desc)]
      (is (= 0 (count (vg/children node)))))))


(deftest update-props
  (testing "props are updated"
    (let [init-props {:a 1}
          next-props {:b 2}
          v0 (vg/object classes/one-fish-desc init-props)
          v1 (vg/update-props v0 next-props)]
      (is (= next-props (vg/props v1))))))


(deftest update-children
  (testing "updates the child node list"
    (let [child1 (vg/object classes/one-fish-desc)
          child2 (vg/object classes/one-fish-desc)
          parent (vg/object classes/one-fish-desc)]
      (is (= [child1 child2])
          (vg/children (vg/update-children parent [child1 child2]))))))


(deftest node=
  (testing "two nodes are equivalent, disregarding their children"
    (let [node-1 (vg/object classes/one-fish-desc)
          node-2 (vg/object classes/one-fish-desc {:tt 1})
          node-3 (vg/object classes/two-fish-desc)]
      (is (vg/node= node-1 (vg/update-children node-1 [node-2])))
      (is (not (vg/node= node-1 node-2)))
      (is (not (vg/node= node-1 node-3))))))


(deftest write-and-read-state
  (let [node-1 (vg/object classes/one-fish-desc)
        state {:tt 1, :tootoo 3}]
    (is (nil? (vg/state node-1)))
    (is (= state (vg/state (vg/with-state node-1 state))))))
