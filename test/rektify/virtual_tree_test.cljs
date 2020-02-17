(ns rektify.virtual-tree-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [test.classes-test :as classes :refer [fish-zip]]
             [clojure.zip :as z]
             [rektify.virtual-tree :as vt :refer [v-tree-zip]]
             [rektify.object :as o]))


(deftest v-node-create-and-test
  (let [o1 (vt/node classes/one-fish-desc)
        o2 (vt/node classes/one-fish-desc {})
        o3 (vt/node classes/one-fish-desc {} [])]

    (testing "test all v-nodes"
      (is (vt/node? o1))
      (is (vt/node? o2))
      (is (vt/node? o3)))))


(deftest props
  (testing "returns props"
    (let [props {:a 1}
          vn (vt/node classes/one-fish-desc props)]
      (is (= props (vt/props vn))))))


(deftest type-desc
  (testing "returns the node type description"
    (let [vn (vt/node classes/one-fish-desc {})]
      (is (= classes/one-fish-desc (vt/desc vn))))))


(deftest children
  (testing "returns the child list"
    (let [child (vt/node classes/one-fish-desc)
          parent (vt/node classes/two-fish-desc {} [child])]
      (is (= [child] (vt/children parent)))))

  (testing "returns no children if none created"
    (let [node (vt/node classes/one-fish-desc)]
      (is (= 0 (count (vt/children node)))))))


(deftest update-props
  (testing "props are updated"
    (let [init-props {:a 1}
          next-props {:b 2}
          v0 (vt/node classes/one-fish-desc init-props)
          v1 (vt/update-props v0 next-props)]
      (is (= next-props (vt/props v1))))))


(deftest update-children
  (testing "updates the child node list"
    (let [child1 (vt/node classes/one-fish-desc)
          child2 (vt/node classes/one-fish-desc)
          parent (vt/node classes/one-fish-desc)]
      (is (= [child1 child2])
          (vt/children (vt/update-children parent [child1 child2]))))))


(deftest node=
  (testing "two nodes are equivalent, disregarding their children"
    (let [node-1 (vt/node classes/one-fish-desc)
          node-2 (vt/node classes/one-fish-desc {:tt 1})
          node-3 (vt/node classes/two-fish-desc)]
      (is (vt/node= node-1 (vt/update-children node-1 [node-2])))
      (is (not (vt/node= node-1 node-2)))
      (is (not (vt/node= node-1 node-3))))))


(deftest write-and-read-state
  (let [node-1 (vt/node classes/one-fish-desc)
        state {:tt 1, :tootoo 3}]
    (is (nil? (vt/state node-1)))
    (is (= state (vt/state (vt/with-state node-1 state))))))


(def single-node (vt/node classes/one-fish-desc))

(def depth-2-tree
  (vt/node classes/one-fish-desc {}
           [(vt/node classes/two-fish-desc {:in-the-beginning "was darkness"
                                                :and-then     "along came Ra"})
              (vt/node classes/red-fish-desc)
              (vt/node classes/blue-fish-desc)]))


(def depth-3-tree
  (vt/node classes/one-fish-desc {}
           [(vt/node classes/red-fish-desc {}
                     [(vt/node classes/one-fish-desc)
                          (vt/node classes/red-fish-desc)])
              (vt/node classes/blue-fish-desc {}
                       [(vt/node classes/two-fish-desc {:in-the-beginning "was darkness"
                                                            :and-then     "along came Ra!"})])]))

(deftest reify-v-tree
  (testing "reify a single v-node"
    (let [reified-v-node (vt/reify-v-tree single-node)
          obj& (vt/obj& reified-v-node)]
      (is (vt/reified? reified-v-node) "v-node was reified")
      (is (instance? classes/OneFish obj&) "obj& is instance of OneFish")))

  (testing "reify a v-node with children"
    (let [reified-v-tree (vt/reify-v-tree depth-2-tree)
          reified-z (v-tree-zip reified-v-tree)
          root& (vt/obj& reified-v-tree)
          root&-z (fish-zip root&)]

      (testing ": all v-nodes were reified"
        (is (vt/reified? reified-v-tree) "v-tree root was reified")
        (is (vt/reified? (-> reified-z z/down z/node)) "v-tree first child was reified")
        (is (vt/reified? (-> reified-z z/down z/right z/node)) "v-tree second child was reified")
        (is (vt/reified? (-> reified-z z/down z/right z/right z/node)) "v-tree third child was reified"))

      (testing ": all reified nodes have correct objects"
        (is (instance? classes/OneFish root&) "root& is instance of OneFish")
        (is (instance? classes/TwoFish (vt/obj& (-> reified-z z/down z/node))))
        (is (instance? classes/RedFish (vt/obj& (-> reified-z z/down z/right z/node))))
        (is (instance? classes/BlueFish (vt/obj& (-> reified-z z/down z/right z/right z/node)))))

      (testing ": all child objects are connected to parent"
        (is (instance? classes/TwoFish (-> root&-z z/down z/node))
            "first child is instance of TwoFish")
        (is (instance? classes/RedFish (-> root&-z z/down z/right z/node))
            "second child is instance of RedFish")
        (is (instance? classes/BlueFish (-> root&-z z/down z/right z/right z/node))
            "third child is instance of BlueFish"))))

  (testing "reify a v-node with grandchildren"
    (let [reified-v-tree (vt/reify-v-tree depth-3-tree)
          reified-z (v-tree-zip reified-v-tree)
          root& (vt/obj& reified-v-tree)
          root&-z (fish-zip root&)]

      (testing ": root and all grandchild v-nodes were reified"
        (is (vt/reified? reified-v-tree) "v-tree root was reified")
        (is (vt/reified? (-> reified-z z/down z/down z/node)))
        (is (vt/reified? (-> reified-z z/down z/down z/right z/node)))
        (is (vt/reified? (-> reified-z z/down z/right z/down z/node))) )

      (testing ": all grandchild nodes have correct objects"
        (is (instance? classes/OneFish (vt/obj& (-> reified-z z/down z/down z/node))))
        (is (instance? classes/RedFish (vt/obj& (-> reified-z z/down z/down z/right z/node))))
        (is (instance? classes/TwoFish (vt/obj& (-> reified-z z/down z/right z/down z/node)))))

      (testing ": all objects are connected to their parents"
        (is (instance? classes/OneFish root&))
        (is (instance? classes/RedFish (-> root&-z z/down z/node)))
        (is (instance? classes/BlueFish (-> root&-z z/down z/right z/node)))
        (is (instance? classes/OneFish (-> root&-z z/down z/down z/node)))
        (is (instance? classes/RedFish (-> root&-z z/down z/down z/right z/node)))
        (is (instance? classes/TwoFish (-> root&-z z/down z/right z/down z/node)))))))


(deftest destroy-v-tree
  (testing "single node v-tree is destroyed"
    (let [v-node (vt/reify-v-tree depth-2-tree)
          obj& (vt/obj& v-node)]
      (vt/destroy-node! v-node)
      (is (.isDestroyed obj&) "single node is destroyed")))

  (testing "entire v-tree is destroyed"
    (let [v-tree (vt/reify-v-tree depth-2-tree)
          root& (vt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          second-child& (-> root&-z z/down z/right z/node)
          third-child& (-> root&-z z/down z/right z/right z/node)]
      (vt/destroy-node! v-tree)

      (is (.isDestroyed root&))
      (is (.isDestroyed first-child&))
      (is (.isDestroyed second-child&))
      (is (.isDestroyed third-child&))))

  (testing "leaf of v-tree is destroyed"
    (let [v-tree (vt/reify-v-tree depth-2-tree)
          first-child (first (vt/children v-tree))
          root& (vt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          second-child& (-> root&-z z/down z/right z/node)
          third-child& (-> root&-z z/down z/right z/right z/node)]
      (vt/destroy-node! first-child v-tree)

      (is (= 2 (alength (.getChildren root&))))
      (is (not (.isDestroyed root&)))
      (is (.isDestroyed first-child&))
      (is (not (.isDestroyed second-child&)))
      (is (not (.isDestroyed third-child&)))))

  (testing "subtree of v-tree is destroyed"
    (let [v-tree (vt/reify-v-tree depth-3-tree)
          first-child (first (vt/children v-tree))
          root& (vt/obj& v-tree)
          root&-z (fish-zip root&)
          first-child& (-> root&-z z/down z/node)
          first-grandchild& (-> root&-z z/down z/down z/node)
          second-grandchild& (-> root&-z z/down z/down z/right z/node)]
      (vt/destroy-node! first-child v-tree)

      (is (= 1 (alength (.getChildren root&))))
      (is (instance? classes/BlueFish (-> root&-z z/down z/node)))
      (is (= 1 (alength (.getChildren (-> root&-z z/down z/node)))))
      (is (instance? classes/TwoFish (-> root&-z z/down z/down z/node)))

      (is (.isDestroyed first-child&))
      (is (.isDestroyed first-grandchild&))
      (is (.isDestroyed second-grandchild&)))))


(deftest rektify-v-tree
  (testing "reifies a non-reified v-tree"
    (let [reified-v-tree (vt/rektify depth-2-tree)
          reified-z (v-tree-zip reified-v-tree)]

      (testing "all v-nodes were reified"
        (is (vt/reified? reified-v-tree) "v-tree root was reified")
        (is (vt/reified? (-> reified-z z/down z/node)) "v-tree first child was reified")
        (is (vt/reified? (-> reified-z z/down z/right z/node)) "v-tree second child was reified")
        (is (vt/reified? (-> reified-z z/down z/right z/right z/node)) "v-tree third child was reified"))))


  (testing "updates properties"
    (testing "single node tree"
      (let [reified-node (vt/rektify (vt/node classes/one-fish-desc
                                              {:some-prop "plop plop"
                                                 :optional  false}))
            initial-obj& (vt/obj& reified-node)
            updated-props {:some-prop "fizz fizz", :optional true}
            rektified-node (vt/rektify
                             (vt/node classes/one-fish-desc updated-props)
                             reified-node)
            updated-obj& (vt/obj& rektified-node)]
        (is (= classes/one-fish-desc (vt/desc rektified-node))
            "rektified v-node has same type")
        (is (= updated-props (vt/props rektified-node))
            "rektified v-node has new props")

        (is (identical? initial-obj& updated-obj&)
            "object instance remains the same")
        (is (= updated-props (o/props classes/one-fish-desc updated-obj&))
            "Object properties were updated")))

    (testing "depth-2 tree"
      (testing "parent node updates"
        (let [reified-tree (vt/rektify depth-2-tree)
              init-root& (vt/obj& reified-tree)
              obj&-z (fish-zip init-root&)
              init-child-1& (-> obj&-z z/down z/node)
              init-child-2& (-> obj&-z z/down z/right z/node)
              init-child-3& (-> obj&-z z/down z/right z/right z/node)

              updated-props {:some-prop "fizz fizz", :optional true}
              updated-v-tree (vt/update-props depth-2-tree updated-props)

              rektified-tree (vt/rektify updated-v-tree reified-tree)
              rekt-root& (vt/obj& rektified-tree)
              rekt-root&-z (fish-zip rekt-root&)]
          (is (= updated-v-tree rektified-tree)
              "virtual tree same as passed in")
          (is (= (vt/children reified-tree) (vt/children rektified-tree))
              "virtual tree children are unchanged")
          (is (= updated-props (o/props classes/one-fish-desc rekt-root&))
              "root object props were updated")

          (is (identical? init-root& rekt-root&))
          (is (identical? init-child-1& (-> rekt-root&-z z/down z/node)))
          (is (identical? init-child-2& (-> rekt-root&-z z/down z/right z/node)))
          (is (identical? init-child-3& (-> rekt-root&-z z/down z/right z/right z/node)))))

      (testing "only child nodes update"
        (let [reified-tree (vt/rektify depth-2-tree)
              init-root& (vt/obj& reified-tree)
              obj&-z (fish-zip init-root&)
              init-child-1& (-> obj&-z z/down z/node)
              init-child-2& (-> obj&-z z/down z/right z/node)
              init-child-3& (-> obj&-z z/down z/right z/right z/node)

              new-child-1-props {:in-the-beginning "tinkle", :and-then "spit"}
              new-child-2-props {:something [42 69 420]}
              new-child-3-props {:kind-of-blue "ballin'"}
              new-v-tree (vt/update-children
                           depth-2-tree
                           [(vt/node classes/two-fish-desc new-child-1-props)
                            (vt/node classes/red-fish-desc new-child-2-props)
                            (vt/node classes/blue-fish-desc new-child-3-props)])

              rektified-tree (vt/rektify new-v-tree reified-tree)
              rektified-tree-z (v-tree-zip rektified-tree)
              rekt-root& (vt/obj& rektified-tree)
              rekt-root&-z (fish-zip rekt-root&)
              rekt-child-1& (-> rekt-root&-z z/down z/node)
              rekt-child-2& (-> rekt-root&-z z/down z/right z/node)
              rekt-child-3& (-> rekt-root&-z z/down z/right z/right z/node)]
          (is (= new-v-tree rektified-tree)
              "virtual tree same as passed in")
          (is (vt/reified? rektified-tree))
          (is (vt/reified? (-> rektified-tree-z z/down z/node)))
          (is (vt/reified? (-> rektified-tree-z z/down z/right z/node)))
          (is (vt/reified? (-> rektified-tree-z z/down z/right z/right z/node)))

          (is (identical? init-root& rekt-root&))
          (is (identical? init-child-1& rekt-child-1&))
          (is (= new-child-1-props (o/props classes/two-fish-desc rekt-child-1&)))
          (is (identical? init-child-2& rekt-child-2&))
          (is (= new-child-2-props (o/props classes/red-fish-desc rekt-child-2&)))
          (is (identical? init-child-3& rekt-child-3&))
          (is (= new-child-3-props (o/props classes/blue-fish-desc rekt-child-3&)))))

      (testing "all nodes update"
        (let [reified-tree (vt/rektify depth-2-tree)
              init-root& (vt/obj& reified-tree)
              obj&-z (fish-zip init-root&)
              init-child-1& (-> obj&-z z/down z/node)
              init-child-2& (-> obj&-z z/down z/right z/node)
              init-child-3& (-> obj&-z z/down z/right z/right z/node)

              new-root-props {:optional true, :some-prop "some pig"}
              new-child-1-props {:in-the-beginning "tinkle", :and-then "spit"}
              new-child-2-props {:something [42 69 420]}
              new-child-3-props {:kind-of-blue "ballin'"}
              new-v-tree (vt/update-children
                           (vt/update-props depth-2-tree new-root-props)
                           [(vt/node classes/two-fish-desc new-child-1-props)
                            (vt/node classes/red-fish-desc new-child-2-props)
                            (vt/node classes/blue-fish-desc new-child-3-props)])

              rektified-tree (vt/rektify new-v-tree reified-tree)
              rektified-tree-z (v-tree-zip rektified-tree)
              rekt-root& (vt/obj& rektified-tree)
              rekt-root&-z (fish-zip rekt-root&)
              rekt-child-1& (-> rekt-root&-z z/down z/node)
              rekt-child-2& (-> rekt-root&-z z/down z/right z/node)
              rekt-child-3& (-> rekt-root&-z z/down z/right z/right z/node)]
          (is (= new-v-tree rektified-tree)
              "virtual tree same as passed in")
          (is (vt/reified? rektified-tree))
          (is (vt/reified? (-> rektified-tree-z z/down z/node)))
          (is (vt/reified? (-> rektified-tree-z z/down z/right z/node)))
          (is (vt/reified? (-> rektified-tree-z z/down z/right z/right z/node)))

          (is (identical? init-root& rekt-root&))
          (is (= new-root-props (o/props classes/one-fish-desc rekt-root&)))
          (is (identical? init-child-1& rekt-child-1&))
          (is (= new-child-1-props (o/props classes/two-fish-desc rekt-child-1&)))
          (is (identical? init-child-2& rekt-child-2&))
          (is (= new-child-2-props (o/props classes/red-fish-desc rekt-child-2&)))
          (is (identical? init-child-3& rekt-child-3&))
          (is (= new-child-3-props (o/props classes/blue-fish-desc rekt-child-3&)))))))


  (testing "adds new children"
    (testing "to a single node"
      (let [reified-node (vt/rektify single-node)
            original-node& (vt/obj& reified-node)
            new-v-tree (vt/update-children single-node
                                           [(vt/node classes/red-fish-desc)
                                            (vt/node classes/blue-fish-desc)])
            rektified-v-tree (vt/rektify new-v-tree reified-node)]

        (testing "objects were created and added to root"
          (let [new-root& (vt/obj& rektified-v-tree)
                new-root&-z (fish-zip new-root&)]
            (is (identical? original-node& new-root&) "original node same instance after rektifying")
            (is (instance? classes/RedFish (-> new-root&-z z/down z/node)))
            (is (instance? classes/BlueFish (-> new-root&-z z/down z/right z/node)))))

        (testing "v-tree children were reified"
          (let [rekt-tree-z (v-tree-zip rektified-v-tree)]
            (is (vt/reified? (-> rekt-tree-z z/down z/node)))
            (is (vt/reified? (-> rekt-tree-z z/down z/right z/node)))))))

    (testing "depth-2 tree"
      (let [reified-tree (vt/rektify depth-2-tree)
            orig-head& (vt/obj& reified-tree)
            head&-z (fish-zip orig-head&)
            orig-child-1& (-> head&-z z/down z/node)
            orig-child-2& (-> head&-z z/down z/right z/node)
            orig-child-3& (-> head&-z z/down z/right z/right z/node)]
        (testing "add to root"
          (let [v-tree-2 (z/root (z/append-child
                                   (-> (v-tree-zip depth-2-tree))
                                   (vt/node classes/one-fish-desc)))
                rekt-tree-2 (vt/rektify v-tree-2 reified-tree)
                head-2& (vt/obj& rekt-tree-2)
                head-2&-z (fish-zip head-2&)
                new-child& (-> head-2&-z z/down z/right z/right z/right z/node)]

            (testing "new object was added to root object"
              (is (identical? orig-head& head-2&))
              (is (identical? orig-child-1& (-> head-2&-z z/down z/node)))
              (is (identical? orig-child-2& (-> head-2&-z z/down z/right z/node)))
              (is (identical? orig-child-3& (-> head-2&-z z/down z/right z/right z/node)))
              (is (instance? classes/OneFish new-child&)))

            (testing "new v-tree child was reified"
              (let [new-child-node (-> (v-tree-zip rekt-tree-2)
                                       z/down z/right z/right z/right z/node)]
                (is (vt/reified? new-child-node))
                (is (instance? classes/OneFish (vt/obj& new-child-node)))))

            (testing "add to child node"
              (let [v-tree-3 (z/root (z/insert-child
                                       (-> (v-tree-zip v-tree-2) z/down)
                                       (vt/node classes/red-fish-desc)))
                    rekt-tree-3 (vt/rektify v-tree-3 rekt-tree-2)
                    head-3& (vt/obj& rekt-tree-3)
                    head-3&-z (fish-zip head-3&)
                    grandchild& (-> head-3&-z z/down z/down z/node)]

                (testing "new object was added to child"
                  (is (identical? orig-head& head-3&))
                  (is (identical? orig-child-1& (-> head-3&-z z/down z/node)))
                  (is (identical? orig-child-2& (-> head-3&-z z/down z/right z/node)))
                  (is (identical? orig-child-3& (-> head-3&-z z/down z/right z/right z/node)))
                  (is (identical? new-child& (-> head-3&-z z/down z/right z/right z/right z/node)))
                  (is (instance? classes/RedFish grandchild&)))

                (testing "new v-tree grandchild was reified"
                  (let [new-grandchild-node (-> (v-tree-zip rekt-tree-3) z/down z/down z/node)]
                    (is (vt/reified? new-grandchild-node))
                    (is (instance? classes/RedFish (vt/obj& new-grandchild-node))))))))))))

  (testing "destroys removed objects"
    (let [reified-tree (vt/rektify depth-2-tree)
          root& (vt/obj& reified-tree)
          root&-z (fish-zip root&)
          child-1& (-> root&-z z/down z/node)
          child-2& (-> root&-z z/down z/right z/node)
          child-3& (-> root&-z z/down z/right z/right z/node)]

      (testing "destroys child nodes"
        (let [new-tree-1 (z/root (z/remove
                                   (-> (v-tree-zip depth-2-tree) z/down z/right z/right)))
              rekt-tree-1 (vt/rektify new-tree-1 reified-tree)]
          (is (identical? root& (vt/obj& rekt-tree-1)))
          (is (= false (.isDestroyed root&)))
          (is (= false (.isDestroyed child-1&)))
          (is (= false (.isDestroyed child-2&)))
          (is (= true (.isDestroyed child-3&)))
          (is (= 2 (count (vt/children rekt-tree-1))))
          (is (= 2 (count (js->clj (.getChildren root&)))))

          (testing "destroys entire tree if nil passed in"
            (let [dead-tree (vt/rektify nil rekt-tree-1)]
              (is (nil? dead-tree))
              (is (= true (.isDestroyed root&)))
              (is (= true (.isDestroyed child-1&)))
              (is (= true (.isDestroyed child-2&)))))))))

  (testing "replaces objects"
    (testing "of a single node"
      (let [reified-node (vt/rektify single-node)
            head-1& (vt/obj& reified-node)
            rektified-node (vt/rektify
                             (vt/node classes/blue-fish-desc)
                             reified-node)
            head-2& (vt/obj& rektified-node)]
        (is (not (identical? head-1& head-2&)))
        (is (= true (.isDestroyed head-1&)))
        (is (instance? classes/BlueFish head-2&))))

    (testing "of a depth-1 tree"
      (let [reified-tree (vt/rektify depth-2-tree)
            head-1& (vt/obj& reified-tree)
            head-1&-z (fish-zip head-1&)
            first-child-1& (-> head-1&-z z/down z/node)
            second-child-1& (-> head-1&-z z/down z/right z/node)
            third-child-1& (-> head-1&-z z/down z/right z/right z/node)]

        (testing "-> first child change"
          (let [v-tree-2 (z/root (z/replace (-> (v-tree-zip depth-2-tree) z/down)
                                            (vt/node classes/blue-fish-desc)))
                rekt-tree-2 (vt/rektify v-tree-2 reified-tree)
                head-2& (vt/obj& rekt-tree-2)
                head-2&-z (fish-zip head-2&)
                first-child-2& (-> head-2&-z z/down z/node)
                second-child-2& (-> head-2&-z z/down z/right z/node)
                third-child-2& (-> head-2&-z z/down z/right z/right z/node)]
            (testing "and object tree was correctly modified"
              (is (identical? head-1& head-2&))
              (is (= true (.isDestroyed first-child-1&)))
              (is (not (identical? first-child-1& first-child-2&)))
              (is (instance? classes/BlueFish first-child-2&))
              (is (identical? second-child-1& second-child-2&))
              (is (identical? third-child-1& third-child-2&)))

            (testing "and v-node was correctly reified"
              (let [replaced-node (-> (v-tree-zip rekt-tree-2) z/down z/node)]
                (is (vt/reified? replaced-node))
                (is (identical? first-child-2& (vt/obj& replaced-node)))))

            (testing "-> middle child changed"
              (let [v-tree-3 (z/root (z/replace (-> (v-tree-zip v-tree-2) z/down z/right)
                                                (vt/node classes/two-fish-desc
                                                         {:in-the-beginning "dunkin'"
                                                            :and-then         "heinz"})))
                    rekt-tree-3 (vt/rektify v-tree-3 rekt-tree-2)
                    head-3& (vt/obj& rekt-tree-3)
                    head-3&-z (fish-zip head-3&)
                    first-child-3& (-> head-3&-z z/down z/node)
                    second-child-3& (-> head-3&-z z/down z/right z/node)
                    third-child-3& (-> head-3&-z z/down z/right z/right z/node)]

                (testing "and object tree was correctly modified"
                  (is (identical? head-2& head-3&))
                  (is (identical? first-child-2& first-child-3&))
                  (is (= true (.isDestroyed second-child-2&)))
                  (is (not (identical? second-child-2& second-child-3&)))
                  (is (instance? classes/TwoFish second-child-3&))
                  (is (identical? third-child-2& third-child-3&)))

                (testing "and v-node was correctly reified"
                  (let [replaced-node (-> (v-tree-zip rekt-tree-3) z/down z/right z/node)]
                    (is (vt/reified? replaced-node))
                    (is (identical? second-child-3& (vt/obj& replaced-node)))))

                (testing "-> last child changed"
                  (let [v-tree-4 (z/root (z/replace (-> (v-tree-zip v-tree-3)
                                                        z/down z/right z/right)
                                                    (vt/node classes/red-fish-desc)))
                        rekt-tree-4 (vt/rektify v-tree-4 rekt-tree-3)
                        head-4& (vt/obj& rekt-tree-4)
                        head-4&-z (fish-zip head-4&)
                        first-child-4& (-> head-4&-z z/down z/node)
                        second-child-4& (-> head-4&-z z/down z/right z/node)
                        third-child-4& (-> head-4&-z z/down z/right z/right z/node)]
                    (testing "and object tree was correctly modified"
                      (is (identical? head-4& head-3&))
                      (is (identical? first-child-3& first-child-4&))
                      (is (identical? second-child-3& second-child-4&))
                      (is (= true (.isDestroyed third-child-3&)))
                      (is (not (identical? third-child-3& third-child-4&)))
                      (is (instance? classes/RedFish third-child-4&)))

                    (testing "and v-node was correctly reified"
                      (let [replaced-node (-> (v-tree-zip rekt-tree-4) z/down z/right z/right z/node)]
                        (is (vt/reified? replaced-node))
                        (is (identical? third-child-4& (vt/obj& replaced-node)))))

                    (testing "-> root node changed"
                      (let [v-tree-5 (vt/node classes/red-fish-desc {}
                                              (vt/children v-tree-4))
                            rekt-tree-5 (vt/rektify v-tree-5 rekt-tree-4)
                            head-5& (vt/obj& rekt-tree-5)
                            head-5&-z (fish-zip head-5&)
                            first-child-5& (-> head-5&-z z/down z/node)
                            second-child-5& (-> head-5&-z z/down z/right z/node)
                            third-child-5& (-> head-5&-z z/down z/right z/right z/node)]
                        (testing "and object tree was correctly modified"
                          (is (not (identical? head-4& head-5&)))
                          (is (instance? classes/RedFish head-5&))
                          (is (= true (.isDestroyed head-4&)))

                          (is (not (identical? first-child-4& first-child-5&)))
                          (is (instance? classes/BlueFish first-child-5&))
                          (is (= true (.isDestroyed first-child-4&)))

                          (is (not (identical? second-child-4& second-child-5&)))
                          (is (instance? classes/TwoFish second-child-5&))
                          (is (= true (.isDestroyed second-child-4&)))

                          (is (not (identical? third-child-4& third-child-5&)))
                          (is (instance? classes/RedFish third-child-5&))
                          (is (= true (.isDestroyed third-child-4&))))

                        )))))))))))

  (testing "one add, one replace, one removal, simultaneously"
    ;; https://en.wikipedia.org/wiki/Roland_the_Farter
    (let [reified-tree (vt/rektify depth-2-tree)
          head& (vt/obj& reified-tree)
          head&-z (fish-zip head&)
          first-child& (-> head&-z z/down z/node)
          second-child& (-> head&-z z/down z/right z/node)
          third-child& (-> head&-z z/down z/right z/right z/node)

          new-head-props {:optional true, :some-prop "ploop"}
          new-v-tree (-> depth-2-tree
                         (vt/update-props new-head-props)
                         (vt/update-children
                           [(vt/node classes/blue-fish-desc)
                            (vt/node classes/one-fish-desc)]))
          rekt-tree (vt/rektify new-v-tree reified-tree)
          rekt-head& (vt/obj& rekt-tree)
          rekt-head&-z (fish-zip rekt-head&)]
      (is (identical? head& rekt-head&))
      (is (= new-head-props (o/props classes/one-fish-desc head&)))
      (is (= true (.isDestroyed first-child&)))
      (is (= true (.isDestroyed second-child&)))
      (is (= true (.isDestroyed third-child&)))
      (is (= 2 (count (o/children classes/one-fish-desc rekt-head&))))
      (let [first-rekt-child& (-> rekt-head&-z z/down z/node)
            second-rekt-child& (-> rekt-head&-z z/down z/right z/node)]
        (is (instance? classes/BlueFish first-rekt-child&))
        (is (instance? classes/OneFish second-rekt-child&))))))

