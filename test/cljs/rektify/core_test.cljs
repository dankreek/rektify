(ns rektify.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [rektify.core :as rekt]
            [goog.dom :as dom]
            [test.classes-test :as classes]
            [clojure.set :as set]
            [clojure.zip :as z]))

;; TODO: write tests for:
;; * get-existing-object-properties

(def simple-gen
  "A generator with a single render function which returns the virtual graph
  that is the value of the `v-graph` property. This allows for easily testing
  the diffing of graphs and ensuring that the generator is re-rendered since
  its props are changed."
  {:generate (fn [{:keys [v-graph]} _] v-graph)})


(defn simple-v-graph
  "Create a simple generator virtual node which will return the provided
  virtual graph."
  [v-graph]
  (rekt/generator-v-node simple-gen {:v-graph v-graph}))


#_(deftest single-object-construction
  (testing "An object with no constructor list can be created with the default constructor"
    (is (instance? classes/OneFish (rekt/new-object classes/one-fish-desc))))

  (testing "And object description with a defined default constructor creates an object"
    (is (instance? classes/BlueFish (rekt/new-object classes/blue-fish-desc))))

  (testing "Create an object with specified constructor and some default props"
    (is (instance? classes/TwoFish
                   (rekt/new-object classes/two-fish-desc {:and-then "again, I hesitate."}))))

  (testing "Create an object with specified constructor and explicit props"
    (is (instance? classes/TwoFish
                   (rekt/new-object
                     classes/two-fish-desc
                     {:in-the-beginning "there was darkness and darkness was ignorance"
                      :and-then "along came Ra"}))))

  (testing "Post-constructor is called"
    (let [obj (rekt/new-object
                classes/two-fish-desc {:and-then "I was a thing"})]
      (is (= true (.-postConstructorCalled obj))))))


#_(deftest single-object-properties
  (testing "A basic property can be set and got"
    (let [o (rekt/new-object classes/one-fish-desc)]
      (is (= false (rekt/get-prop o :some-prop))
          "The default value is set on instantiation")
      (rekt/set-props o {:some-prop true})
      (is (= true (.-someProp o)))
      (is (= true (rekt/get-prop o :some-prop)))))

  (testing "A property which is set to false is applied"
    (let [o (rekt/new-object
              classes/two-fish-desc {:in-the-beginning "a"
                                     :and-then "b"})]
      (is (= "a" (.-first o)))
      (is (= "b" (.-second o)))
      (rekt/set-props o {:in-the-beginning false
                         :and-then      false})
      (is (= false (.-first o)))
      (is (= false (.-second o)))))

  (testing "A composite property can be set and got"
    (let [o (rekt/new-object classes/red-fish-desc)]
      (is (= [0 1 -1] (rekt/get-prop o :something))
          "The default value is set on instantiation")
      (is (= 0 (.-x o)))
      (is (= 1 (.-y o)))
      (is (= -1 (.-z o)))
      (rekt/set-props o {:something [1 2 3]})
      (is (= [1 2 3] (rekt/get-prop o :something)))
      (is (= 1 (.-x o)))
      (is (= 2 (.-y o)))
      (is (= 3 (.-z o)))))

  (testing "Removing a previously set property uses the default"
    (let [o (rekt/new-object
              classes/red-fish-desc {:something [1 800 "pp5-1doodoo"]})]
      (is (= 1 (.-x o)))
      (is (= 800 (.-y o)))
      (is (= "pp5-1doodoo" (.-z o)))
      (rekt/set-props o {})
      (is (= [0 1 -1] (rekt/get-prop o :something))
          "Default props are set when they are no longer explicitly set")
      (is (= 0 (.-x o)))
      (is (= 1 (.-y o)))
      (is (= -1 (.-z o)))))

  (testing "Getters work with single property"
    (let [o (new classes/TwoFish "a" "b")
          props (rekt/get-existing-object-properties
                  o (:prop-map classes/two-fish-desc))]
      (is (= {:in-the-beginning "a", :and-then "b"} props))))

  (testing "Getters work with composite property"
    (let [o (new classes/RedFish)
          props (rekt/get-existing-object-properties
                  o (:prop-map classes/red-fish-desc))]
      (is (= {:something [0 1 -1]} props)))))


#_(deftest both-forms-of-generator-correctly-resolve
  (let [generate-fn (fn [props _] (classes/one-fish))
        gen-map {:generate generate-fn}
        gen-map-factory (fn [] gen-map)]
    (testing "A generator map generates a graph"
      (let [g (rekt/generate-graph
                (rekt/generator-v-node gen-map))]
        (is (instance? classes/OneFish g))))

    (testing "A generator map factory function generates a graph"
      (let [g (rekt/generate-graph
                (rekt/generator-v-node gen-map-factory))]
        (is (instance? classes/OneFish g))))))



#_(deftest create-new-graph-with-a-single-generator
  (testing "Create a single object"
    (let [one-fish-obj (rekt/generate-graph
                         (simple-v-graph (classes/one-fish)))]
      (is (instance? classes/OneFish one-fish-obj))))

  (testing "Create a depth-1 tree correctly"
    (let [g (rekt/generate-graph (simple-v-graph
                                   (classes/one-fish {}
                                     (classes/red-fish)
                                     (classes/blue-fish)
                                     (classes/one-fish))))
          zg (classes/fish-zip g)]
      (is (instance? classes/OneFish g))
      (is (instance? classes/RedFish (-> zg z/down z/node)))
      (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
      (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node)))))

  (testing "Create a depth-2 tree correctly"
    (let [g (rekt/generate-graph
              (simple-v-graph
                (classes/one-fish {}
                  (classes/red-fish {}
                    (classes/blue-fish))
                  (classes/blue-fish {}
                    (classes/red-fish)))))
          zg (classes/fish-zip g)]
      (is (instance? classes/OneFish g))

      (is (instance? classes/RedFish (-> zg z/down z/node))
          "First child was correctly created")

      (is (instance? classes/BlueFish (-> zg z/down z/right z/node))
          "Second child was correctly created")

      (is (instance? classes/BlueFish (-> zg z/down z/down z/node))
          "First grandchild was correctly created")

      (is (instance? classes/RedFish (-> zg z/down z/right z/down z/node)))))


  (testing "A tree of generators creates the correct graph"
    (let [g (rekt/generate-graph
              (simple-v-graph
                (classes/one-fish {})
                ))])))

;; TODO: This and the following test should be rearranged into a more methodical
;;       set of tests in a single test. The current arrangement is an artifact
;;       of the previous API which allowed non-generated v-graphs at the head
#_(deftest manipulate-basic-graph-with-new-virtual-graph
  (testing "Create an object"
    (let [o (rekt/generate-graph (simple-v-graph
                                        (classes/one-fish {})))]
      (is (= false (.-someProp o)))

      (testing "and apply the same virtual graph"
        (let [o1 (rekt/rektify-graph o (simple-v-graph
                                            (classes/one-fish {})))]
          (is (= o o1) "the same instance is returned")))

      (testing "and mutate it"
        (rekt/rektify-graph o (simple-v-graph
                                   (classes/one-fish {:some-prop true})))
        (is (= true (.-someProp o))
            "`someProp` was mutated and the object is the same instance"))

      (testing "and set a prop to false"
        (rekt/rektify-graph o (simple-v-graph
                                   (classes/one-fish {:some-prop false})))
        (is (= false (.-someProp o))))

      (testing "and destroy it"
        (let [o2 (rekt/rektify-graph o nil)]
          (is (nil? o2))
          (is (= true (.isDestroyed o)))))))

  (testing "Create a tree"
    (let [o (rekt/generate-graph (simple-v-graph
                                        (classes/one-fish {}
                                          (classes/red-fish)
                                          (classes/blue-fish))))]
      (is (= false (.-someProp o)))
      (is (= 2 (count (.getChildren o))))
      (let [children (.getChildren o)
            new-blue-fish (aget children 1)]
        (is (instance? classes/RedFish (aget children 0)))
        (is (instance? classes/BlueFish new-blue-fish))

        (testing "and replace a node"
          (rekt/rektify-graph o (simple-v-graph
                                     (classes/one-fish {:some-prop true}
                                       (classes/red-fish)
                                       (classes/red-fish))))
          (is (= true (.-someProp o)))
          (is (= 2 (count (.getChildren o))))
          (let [next-children (.getChildren o)
                second-red-fish (aget next-children 1)]
            (is (instance? classes/RedFish (aget next-children 0)))
            (is (instance? classes/RedFish second-red-fish))
            (is (= true (.isDestroyed new-blue-fish))
                "The BlueFish was destroyed after being removed from the graph and replaced")

            (testing "modify a child's properties"
              (rekt/rektify-graph
                o (simple-v-graph
                    (classes/one-fish {}
                      (classes/red-fish)
                      (classes/red-fish {:something [4 5 6]}))))
              (is (= second-red-fish (-> (classes/fish-zip o)
                                         z/down z/right z/node)))
              (is (= 4 (.-x second-red-fish)))
              (is (= 5 (.-y second-red-fish)))
              (is (= 6 (.-z second-red-fish))))

            (testing "then remove a node"
              (is (= o (rekt/rektify-graph o (simple-v-graph
                                                  (classes/one-fish {}
                                                    (classes/red-fish))))))
              (is (= 1 (count (.getChildren o))))
              (is (= true (.isDestroyed second-red-fish))
                  "The RedFish was destroyed after being removed from the graph with no replacement")

              (testing "then add a node"
                (rekt/rektify-graph o (simple-v-graph
                                           (classes/one-fish {}
                                             (classes/red-fish)
                                             (classes/blue-fish))))
                (is (= 2 (count (.getChildren o))))
                (is (instance? classes/RedFish (.getChildAt o 0)))
                (is (instance? classes/BlueFish (.getChildAt o 1))))))))))

  (testing "New siblings render in order"
    (let [g (rekt/generate-graph (simple-v-graph (classes/one-fish)))
          zg (classes/fish-zip g)]
      (rekt/rektify-graph g (simple-v-graph
                                 (classes/one-fish {}
                                   (classes/red-fish)
                                   (classes/blue-fish)
                                   (classes/one-fish))))
      (is (instance? classes/RedFish (-> zg z/down z/node)))
      (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
      (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node)))

      (testing "and re-render in order"
        (rekt/rektify-graph
          g (simple-v-graph (classes/one-fish {}
                              (classes/red-fish)
                              (classes/blue-fish)
                              (classes/one-fish))))
        (is (instance? classes/RedFish (-> zg z/down z/node)))
        (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
        (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node))))

      (testing "and can be removed"
        (rekt/rektify-graph
          g (simple-v-graph (classes/one-fish))))))

  (testing "A tree is destroyed and replaced when the head node's type changes"
    (let [g0 (rekt/generate-graph (simple-v-graph
                                         (classes/one-fish {}
                                           (classes/red-fish)
                                           (classes/blue-fish))))
          old-one-fish g0
          old-red-fish (.getChildAt old-one-fish 0)
          old-blue-fish (.getChildAt old-one-fish 1)
          g1 (rekt/rektify-graph g0 (simple-v-graph
                                         (classes/blue-fish {}
                                           (classes/one-fish)
                                           (classes/red-fish))))
          new-blue-fish g1
          new-one-fish (.getChildAt new-blue-fish 0)
          new-red-fish (.getChildAt new-blue-fish 1)]
      (is (not= old-one-fish new-blue-fish))
      (is (not= old-red-fish new-one-fish))
      (is (not= old-blue-fish new-red-fish))
      (is (= true (.isDestroyed old-one-fish)))
      (is (= true (.isDestroyed old-red-fish)))
      (is (= true (.isDestroyed old-blue-fish)))))

  (testing "A subtree is destroyed and replaced"
    (let [old-root (rekt/generate-graph
                     (simple-v-graph
                       (classes/one-fish {}
                         (classes/red-fish {}
                           (classes/blue-fish)
                           (classes/blue-fish)))))
          old-red-fish (.getChildAt old-root 0)
          old-blue-fish1 (.getChildAt old-red-fish 0)
          old-blue-fish2 (.getChildAt old-red-fish 1)
          new-root (rekt/rektify-graph
                     old-root (simple-v-graph
                                (classes/one-fish {}
                                  (classes/blue-fish {}
                                    (classes/red-fish)
                                    (classes/red-fish)))))
          new-head new-root
          new-blue-fish (.getChildAt new-head 0)
          new-red-fish1 (.getChildAt new-blue-fish 0)
          new-red-fish2 (.getChildAt new-blue-fish 1)]

      (testing "and the head object did not change"
        (is (= old-root new-head)))

      (testing "and the new subtree is different"
        (is (not= old-red-fish new-blue-fish))
        (is (not= old-blue-fish1 new-red-fish1))
        (is (not= old-blue-fish2 new-red-fish2)))

      (testing "and the old subtree was destroyed"
        (is (= true (.isDestroyed old-red-fish)))
        (is (= true (.isDestroyed old-blue-fish1)))
        (is (= true (.isDestroyed old-blue-fish2)))))))


#_(deftest manipulate-generated-graph
  (testing "A generator creates a new virtual graph"
    (let [*render-props (atom [])
          *cleaned-up (atom false)
          generator {:render (fn [props] (swap! *render-props conj props)
                               (:v-graph props))
                     :cleanup (fn [] (reset! *cleaned-up true))}
          v-graph-0 (classes/one-fish {} (classes/red-fish) (classes/blue-fish))
          g0 (rekt/generate-graph (rekt/generator-v-node
                                         generator {:v-graph v-graph-0}))
          old-one-fish g0
          old-red-fish (.getChildAt old-one-fish 0)
          old-blue-fish (.getChildAt old-one-fish 1)]
      (is (= [{:v-graph v-graph-0}] @*render-props))

      (testing "and is not re-rendered if the props don't change"
        (let [same-g (rekt/rektify-graph
                       g0 (rekt/generator-v-node
                            generator {:v-graph v-graph-0}))]
          (is (= g0 same-g))
          (is (= [{:v-graph v-graph-0}] @*render-props))))

      (testing "and is re-rendered correctly when props change"
        (let [v-graph-1 (classes/red-fish {}
                          (classes/blue-fish)
                          (classes/one-fish))
              g1 (rekt/rektify-graph
                   g0 (rekt/generator-v-node
                        generator {:v-graph v-graph-1}))
              new-red-fish g1
              new-blue-fish (.getChildAt new-red-fish 0)
              new-one-fish (.getChildAt new-red-fish 1)]
          (is (= [{:v-graph v-graph-0} {:v-graph v-graph-1}] @*render-props))
          (is (instance? classes/RedFish new-red-fish))
          (is (instance? classes/BlueFish new-blue-fish))
          (is (instance? classes/OneFish new-one-fish))
          (is (= true (.isDestroyed old-one-fish)))
          (is (= true (.isDestroyed old-red-fish)))
          (is (= true (.isDestroyed old-blue-fish)))

          (testing "and destroys its objects when it goes away"
            (let [g2 (rekt/rektify-graph
                       g1 (rekt/generator-v-node
                            generator {:v-graph nil}))]
              (is (nil? g2))
              (is (= [{:v-graph v-graph-0} {:v-graph v-graph-1} {:v-graph nil}]
                     @*render-props))
              (is @*cleaned-up "The generator had its cleanup method called")
              (is (= true (.isDestroyed new-red-fish)))
              (is (= true (.isDestroyed new-blue-fish)))
              (is (= true (.isDestroyed new-one-fish)))))))))

  (testing "A generator which is a child of an object creates a proper graph"
    (let [v-graph-0 (classes/red-fish {} (classes/blue-fish))
          r {:render (fn [props] (:v-graph props))}
          g0 (rekt/generate-graph
               (simple-v-graph (classes/one-fish {}
                                 (rekt/generator-v-node r {:v-graph v-graph-0}))))
          o g0]
      (is (instance? classes/OneFish o))
      (is (= 1 (count (.getChildren o))))
      (let [old-red-fish (.getChildAt o 0)
            old-blue-fish (.getChildAt old-red-fish 0)]
        (is (instance? classes/RedFish old-red-fish))
        (is (instance? classes/BlueFish old-blue-fish))
        (testing "and destroys its objects when it goes away"
          (rekt/rektify-graph
            g0 (simple-v-graph (classes/one-fish {}
                                 (rekt/generator-v-node r {:v-graph nil}))))
          (is (instance? classes/OneFish o))
          (is (= 0 (count (.getChildren o))))
          (is (= true (.isDestroyed old-red-fish)))
          (is (= true (.isDestroyed old-blue-fish)))))))

  (testing "A generator which yields a graph containing generators creates a proper graph"
    (let [gen1 {:render #(classes/blue-fish)}
          gen0 {:render #(classes/red-fish {} (rekt/generator-v-node gen1 {}))}
          o (rekt/generate-graph
              (simple-v-graph (classes/one-fish {}
                                (rekt/generator-v-node gen0 {}))))]
      (is (instance? classes/OneFish o))
      (is (= 1 (count (.getChildren o))))
      (let [old-red-fish (.getChildAt o 0)
            old-blue-fish (.getChildAt old-red-fish 0)]
        (is (instance? classes/RedFish old-red-fish))
        (is (instance? classes/BlueFish old-blue-fish))

        (testing "and they all fall down"
          (rekt/rektify-graph
            o (simple-v-graph (classes/one-fish)))
          (is (= 0 (count (.getChildren o))))
          (is (= 0 (count (.getChildren old-red-fish))))
          (is (= true (.isDestroyed old-red-fish)))
          (is (= true (.isDestroyed old-blue-fish)))))))

  (testing "A new generator creates a new graph in place of the old one"
    (let [gen1 {:render #(classes/one-fish {}
                           (classes/red-fish)
                           (classes/blue-fish))}
          gen2 {:render #(classes/one-fish {}
                           (classes/red-fish))}
          g (rekt/generate-graph
              (simple-v-graph
                (classes/one-fish {}
                  (rekt/generator-v-node gen1 {:a 1}))))
          gz (classes/fish-zip g)
          of-0 (-> gz z/down z/node)
          rf-0 (-> gz z/down z/down z/node)
          bf-0 (-> gz z/down z/down z/right z/node)]
      (is (instance? classes/OneFish g))
      (is (instance? classes/OneFish of-0))
      (is (instance? classes/RedFish rf-0))
      (is (instance? classes/BlueFish bf-0))

      (let [new-g (rekt/rektify-graph
                    g (simple-v-graph
                        (classes/one-fish {}
                          (rekt/generator-v-node gen2 {:b 3}))))
            of-1 (-> gz z/down z/node)
            rf-1 (-> gz z/down z/down z/node)]
        (is (= g new-g) "The re-rendered graph's root remains the same")
        (is (= of-1 of-0) "The re-rendered graph's first child remains the same")
        (is (= rf-1 rf-0) "The re-rendered graph's grandchild remains the same")
        (is (= true (.isDestroyed bf-0)) "The removed child was destroyed")
        (is (instance? classes/OneFish of-1))
        (is (instance? classes/RedFish rf-1))
        (is (= 1 (count (.getChildren of-1))))))))

#_(deftest generator-life-cycle-functions
  (let [*generate-order (atom [])
        *cleanup-order (atom [])
        *gen-a-head (atom nil)
        *gen-b-head (atom nil)
        *gen-c-head (atom nil)
        gen-c {:render (fn [_ head-obj]
                         (swap! *generate-order conj :c)
                         (reset! *gen-c-head head-obj)
                         (classes/red-fish))
               :cleanup (fn [head-obj]
                          (reset! *gen-c-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :c))}
        gen-b {:render (fn [_ head-obj]
                         (swap! *generate-order conj :b)
                         (reset! *gen-b-head head-obj)
                         (classes/one-fish))
               :cleanup (fn [head-obj]
                          (reset! *gen-b-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :b))}
        gen-a {:render (fn [props head-obj]
                         (swap! *generate-order conj :a)
                         (reset! *gen-a-head head-obj)
                         (classes/one-fish {}
                                   (rekt/generator-v-node gen-b props)
                                   (rekt/generator-v-node gen-c props)))
               :cleanup (fn [head-obj]
                          (reset! *gen-a-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :a))}
        v-graph #(rekt/generator-v-node gen-a %)]

    (testing "Generator life cycle functions are called in the correct order"
      (let [g (rekt/rektify-graph nil (v-graph {}))
            zg (classes/fish-zip g)]
        (testing "when creating a new graph"
          (is (= [:a :b :c] @*generate-order)))

        (testing "and have no head object available during first render"
          (is (nil? @*gen-a-head))
          (is (nil? @*gen-b-head))
          (is (nil? @*gen-c-head)))

        (testing "when re-rendering"
          (testing "doesn't re-render if it doesn't need to"
            (reset! *generate-order [])
            (rekt/rektify-graph g (v-graph {}))
            (is (= [] @*generate-order)))

          (testing "it re-renders in correct order when it receives new props"
            (rekt/rektify-graph g (v-graph {:poop "stewart"}))
            (is (= [:a :b :c] @*generate-order))

            (testing "and the correct head object references are passed in"
              (is (= g @*gen-a-head))
              (is (= (-> zg z/down z/node) @*gen-b-head))
              (is (= (-> zg z/down z/right z/node) @*gen-c-head)))))

        (testing "when being destroyed"
          (let [prev-a @*gen-a-head
                prev-b @*gen-b-head
                prev-c @*gen-c-head]
            (rekt/rektify-graph g nil)
            (is (= prev-a @*gen-a-head))
            (is (= prev-b @*gen-b-head))
            (is (= prev-c @*gen-c-head))
            (is (= true (.isDestroyed @*gen-a-head))
                "Object A destroyed after render")
            (is (= true (.isDestroyed @*gen-b-head))
                "Object B destroyed after render")
            (is (= true (.isDestroyed @*gen-c-head))
                "Object C destroyed after render")
            (is (= [:a :c :b] @*cleanup-order))))))))


#_(deftest generators-with-render-cycle-state
  (testing "a single generator"
    ;; TODO: Remove hack after #3 is resolved
    (swap! rekt/*generator-registry {})
    (let [*state (atom {:a 0})
          *state-history (atom [])
          *cleaned-up (atom false)
          gen {:render (fn [props]
                         (let [the-state (rekt/get-in-state [:a])]
                           (swap! *state-history conj the-state)
                           (is (= (:a @*state) the-state)
                               "The correct state was observed"))
                         (classes/blue-fish))
               :cleanup #(reset! *cleaned-up true)}
          g (rekt/generate-graph (simple-v-graph
                                        (classes/one-fish))
                                 *state)]

      (testing "can read the initial state"
        (rekt/rektify-graph
          g (simple-v-graph (classes/one-fish {}
                              (rekt/generator-v-node gen)))
          *state)
        (is (= [0] @*state-history) "The generator rendered once"))

      (testing "re-renders when state is changed externally"
        (reset! *state {:a 1})
        ;; TODO: There's an issue with reloading in this test somewhere (doesn't happen every time)
        ;;       It's most likely related to generators being a mess right now
        ;;       and should be solved with #3
        (rekt/rektify-graph
          g (simple-v-graph (classes/one-fish {}
                              (rekt/generator-v-node gen)))
          *state)
        (is (= [0 1] @*state-history) "The generator re-rendered once"))

      (testing "does not re-render if state did not change"
        (rekt/rektify-graph
          g (simple-v-graph (classes/one-fish {}
                              (rekt/generator-v-node gen)))
          *state)
        (is (= [0 1] @*state-history) "The generator did not re-render"))

      (testing "does not re-render when removed from graph"
        (reset! *state {:a 2})
        (rekt/rektify-graph g (simple-v-graph
                                   (classes/one-fish))
                            *state)
        (is (= [0 1] @*state-history) "The generator did not re-render")
        (is (= true @*cleaned-up) "The generator clean-up function was called"))))

  (testing "generator state mutators work correctly"
    ;; TODO: Remove hack after #3 is resolved
    (swap! rekt/*generator-registry {})
    (let [*render-count (atom 0)
          *state (atom {})
          gen {:render (fn [_]
                         (let [orig-val (rekt/get-in-state [:a :b])]
                           (swap! *render-count inc)
                           (rekt/assoc-in-state [:a :b] 1)
                           (is (= 1 (get-in @*state [:a :b])))
                           (rekt/update-in-state
                             [:a :b]
                             (fn [prev]
                               (is (= 1 prev) "mutator function receives previous value")
                               (+ prev 5)))
                           (is (= 6 (get-in @*state [:a :b])) "Value changed in the atom")
                           (is (= orig-val (rekt/get-in-state [:a :b])) "Value did not change during render")
                           (classes/one-fish)))}
          g (rekt/generate-graph (rekt/generator-v-node gen) *state)]
      (is (= 1 @*render-count))
      (swap! *state assoc-in [:a :b] 0)
      (rekt/rektify-graph g (rekt/generator-v-node gen) *state)
      (is (= 2 @*render-count)
          "Should have rendered once more because generator is watching `[:a :b]`"))))
