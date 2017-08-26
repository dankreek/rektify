(ns rektify.core-test
  (:require [cljs.test :refer-macros [is testing run-tests async use-fixtures deftest]]
            [rektify.core :as rekt]
            [goog.dom :as dom]
            [test.classes :as classes]
            [clojure.set :as set]
            [clojure.zip :as z]))

(defn fish-zip
  "Create a zipper over a tree of PixiJS objects."
  [head-object]
  (z/zipper (constantly true)
            (fn [o] (seq (.getChildren o)))
            (fn [node children]
              (doseq [child children]
                (.addChild node child)))
            head-object))


(defn get-something-from-red
  [obj _]
  [(.-x obj) (.-y obj) (.-z obj)])


(defn set-something-on-red
  [obj _ [x y z]]
  (.setSomething obj x y z))

(def fishy-object-functions
  {:destructor (fn [this]
                 (.destroy this))})

(def fishy-graph-functions
  (merge
    fishy-object-functions
    {:prop-map {:parent {:property "_parent" :getter aget :setter aset}}
     :get-parent (fn [this]
                   (.getParent this))
     :add-child (fn [this child]
                  (.addChild this child))
     :child-index (fn [this child]
                    (.getChildIndex this child))
     :replace-child-at (fn [this new-child index]
                         (.replaceChildAt this new-child index))
     :remove-child-at (fn [this index]
                        (.removeChildAt this index))
     :get-children (fn [this]
                     (.getChildren this))}))

(def one-fish-desc
  (merge
    fishy-graph-functions
    {:prop-map {:some-prop {:property "someProp" :setter aset :getter aget}}
     :constructor classes/OneFish
     :default-props {:some-prop false}}))


(def two-fish-desc
  (merge
    fishy-graph-functions
    {:prop-map {:in-the-beginning {:property "first" :setter aset :getter aget}
                :and-then {:property "second" :setter aset :getter aget}}
     :constructor classes/TwoFish
     :post-constructor (fn [this]
                         (set! (.-postConstructorCalled this) true))
     :constructor-list [[:in-the-beginning :and-then]]
     :default-props {:in-the-beginning "it was before the end."}}))


(def red-fish-desc
  (merge
    fishy-graph-functions
    {:prop-map {:something {:getter get-something-from-red
                            :setter set-something-on-red}}
     :constructor classes/RedFish
     :default-props {:something [0 1 -1]}}))


(def blue-fish-desc
  (merge
    fishy-graph-functions
    {:constructor classes/BlueFish
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

;; TODO: write tests for:
;; XXX: get-existing-object-properties
;; XXX: extend-existing-obj!
;; XXX: extend-existing-graph-obj!
;; XXX: virtual-graph?

(deftest re-rendering-second-children
  ;; Reproduces bug #7
  (let [g (rekt/reify-virtual-graph
            (one-fish {}
              (one-fish {}
                (red-fish {}) (one-fish {:some-prop 0} (red-fish {:something [1 2 3]})))))
        gz (fish-zip g)]

    (testing "The reification worked"
      (is (instance? classes/OneFish g))
      (is (instance? classes/OneFish (-> gz z/down z/node)))
      (is (instance? classes/RedFish (-> gz z/down z/down z/node)))
      (is (instance? classes/OneFish (-> gz z/down z/down z/right z/node)))
      (is (instance? classes/RedFish (-> gz z/down z/down z/right z/down z/node))))

    (testing "Grandchildren are re-rendered"
      (rekt/re-render-graph!
        g (one-fish {}
              (one-fish {}
                (red-fish {}) (one-fish {:some-prop 2} (red-fish {:something [42 42 42]})))))
      (is (instance? classes/OneFish g))
      (is (instance? classes/OneFish (-> gz z/down z/node)))
      (is (instance? classes/RedFish (-> gz z/down z/down z/node)))
      (is (instance? classes/OneFish (-> gz z/down z/down z/right z/node)))
      (is (instance? classes/RedFish (-> gz z/down z/down z/right z/down z/node)))

      (is (= 2 (.-someProp (-> gz z/down z/down z/right z/node))))
      (is (= 42 (.-x (-> gz z/down z/down z/right z/down z/node)))))))

(deftest single-object-construction
  (testing "An object with no constructor list can be created with the default constructor"
    (is (instance? classes/OneFish (rekt/new-object one-fish-desc))))

  (testing "And object description with a defined default constructor creates an object"
    (is (instance? classes/BlueFish (rekt/new-object blue-fish-desc))))

  (testing "Create an object with specified constructor and some default props"
    (is (instance? classes/TwoFish
                   (rekt/new-object two-fish-desc {:and-then "again, I hesitate."}))))

  (testing "Create an object with specified constructor and explicit props"
    (is (instance? classes/TwoFish
                   (rekt/new-object
                     two-fish-desc
                       {:in-the-beginning "there was darkness and darkness was ignorance"
                        :and-then "along came Ra"}))))

  (testing "Post-constructor is called"
    (let [obj (rekt/new-object two-fish-desc {:and-then "I was a thing"})]
      (is (= true (.-postConstructorCalled obj))))))

(deftest single-object-properties
  (testing "A basic property can be set and got"
    (let [o (rekt/new-object one-fish-desc)]
      (is (= false (rekt/get-object-prop o :some-prop))
          "The default value is set on instantiation")
      (rekt/apply-props! o {:some-prop true})
      (is (= true (.-someProp o)))
      (is (= true (rekt/get-object-prop o :some-prop)))))

  (testing "A composite property can be set and got"
    (let [o (rekt/new-object red-fish-desc)]
      (is (= [0 1 -1] (rekt/get-object-prop o :something))
          "The default value is set on instantiation")
      (is (= 0 (.-x o)))
      (is (= 1 (.-y o)))
      (is (= -1 (.-z o)))
      (rekt/apply-props! o {:something [1 2 3]})
      (is (= [1 2 3] (rekt/get-object-prop o :something)))
      (is (= 1 (.-x o)))
      (is (= 2 (.-y o)))
      (is (= 3 (.-z o)))))

  (testing "Removing a previously set property uses the default"
    (let [o (rekt/new-object red-fish-desc {:something [1 800 "pp5-1doodoo"]})]
      (is (= 1 (.-x o)))
      (is (= 800 (.-y o)))
      (is (= "pp5-1doodoo" (.-z o)))
      (rekt/apply-props! o {})
      (is (= [0 1 -1] (rekt/get-object-prop o :something))
          "Default props are set when they are no longer explicitly set")
      (is (= 0 (.-x o)))
      (is (= 1 (.-y o)))
      (is (= -1 (.-z o)))))

  (testing "Getters work with single property"
    (let [o (new classes/TwoFish "a" "b")
          props (rekt/get-existing-object-properties o (:prop-map two-fish-desc))]
      (is (= {:in-the-beginning "a", :and-then "b"} props))))

  (testing "Getters work with composite property"
    (let [o (new classes/RedFish)
          props (rekt/get-existing-object-properties o (:prop-map red-fish-desc))]
      (is (= {:something [0 1 -1]} props)))))


(deftest create-new-graph-from-virtual-graph
  (testing "Create a single object"
    (let [one-fish-obj (rekt/reify-virtual-graph (one-fish))]
      (is (instance? classes/OneFish one-fish-obj))))

  (testing "Create an object tree"
    (let [head (rekt/reify-virtual-graph
                 (one-fish {} (red-fish) (blue-fish)))
          children (.getChildren head)]
      (is (instance? classes/OneFish head))
      (is (= 2 (.-length children)))
      (is (instance? classes/RedFish (aget children 0)))
      (is (instance? classes/BlueFish (aget children 1)))))

  (testing "Siblings render in order"
    (let [g (rekt/reify-virtual-graph
              (one-fish {}
                        (red-fish) (blue-fish) (one-fish)))
          zg (fish-zip g)]
      (is (instance? classes/RedFish (-> zg z/down z/node)))
      (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
      (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node)))))

  (testing "Create an object from a generator"
    (let [g {:render #(one-fish)}
          head-node (rekt/re-render-graph!
                      nil (rekt/generator-v-node g {}))]
      (is (instance? classes/OneFish head-node))))

  (testing "Create an object tree from a generator"
    (let [g {:render #(one-fish {}
                        (red-fish) (blue-fish {}
                                     (red-fish)))}
          head (rekt/re-render-graph!
                 nil (rekt/generator-v-node g {}))
          children (.getChildren head)]
      (is (instance? classes/OneFish head))
      (is (= 2 (.-length children)))
      (let [red-fish-obj (aget children 0)
            blue-fish-obj (aget children 1)
            blue-fish-children (.getChildren blue-fish-obj)]
        (is (instance? classes/RedFish red-fish-obj))
        (is (instance? classes/BlueFish blue-fish-obj))
        (is (= 0 (.-length (.getChildren red-fish-obj))))
        (is (= 1 (.-length (.getChildren blue-fish-obj))))
        (is (instance? classes/RedFish (aget blue-fish-children 0)))))))


(deftest manipulate-object-graph-with-new-virtual-graph
  (testing "Create an object"
    (let [o (rekt/re-render-graph! nil (one-fish {}))]
      (is (= false (.-someProp o)))
      (testing "and apply the same virtual graph"
        (let [o1 (rekt/re-render-graph! o (one-fish {}))]
          (is (= o o1) "the same instance is returned")))
      (testing "and mutate it"
        (rekt/re-render-graph! o (one-fish {:some-prop true}))
        (is (= true (.-someProp o))
            "`someProp` was mutated and the object is the same instance"))
      (testing "and destroy it"
        (let [o2 (rekt/re-render-graph! o nil)]
          (is (nil? o2))
          (is (= true (.isDestroyed o)))))))

  (testing "Create a tree"
    (let [o (rekt/reify-virtual-graph (one-fish {} (red-fish) (blue-fish)))]
      (is (= false (.-someProp o)))
      (is (= 2 (count (.getChildren o))))
      (let [children (.getChildren o)
            new-blue-fish (aget children 1)]
        (is (instance? classes/RedFish (aget children 0)))
        (is (instance? classes/BlueFish new-blue-fish))
        (testing "and replace a node"
          (rekt/re-render-graph! o (one-fish {:some-prop true} (red-fish) (red-fish)))
          (is (= true (.-someProp o)))
          (is (= 2 (count (.getChildren o))))
          (let [next-children (.getChildren o)
                second-red-fish (aget next-children 1)]
            (is (instance? classes/RedFish (aget next-children 0)))
            (is (instance? classes/RedFish second-red-fish))
            (is (= true (.isDestroyed new-blue-fish))
                "The BlueFish was destroyed after being removed from the graph and replaced")
            (testing "modify a child's properties"
              (rekt/re-render-graph! o (one-fish {} (red-fish) (red-fish {:something [4 5 6]})))
              (is (= second-red-fish (-> (fish-zip o) z/down z/right z/node)))
              (is (= 4 (.-x second-red-fish)))
              (is (= 5 (.-y second-red-fish)))
              (is (= 6 (.-z second-red-fish))))
            (testing "then remove a node"
              (is (= o (rekt/re-render-graph! o (one-fish {} (red-fish)))))
              (is (= 1 (count (.getChildren o))))
              (is (= true (.isDestroyed second-red-fish))
                  "The RedFish was destroyed after being removed from the graph with no replacement")
              (testing "then add a node"
                (rekt/re-render-graph! o (one-fish {} (red-fish) (blue-fish)))
                (is (= 2 (count (.getChildren o))))
                (is (instance? classes/RedFish (.getChildAt o 0)))
                (is (instance? classes/BlueFish (.getChildAt o 1))))))))))

  (testing "New siblings render in order"
    (let [g (rekt/reify-virtual-graph
              (one-fish))
          zg (fish-zip (rekt/re-render-graph!
                          g (one-fish {} (red-fish) (blue-fish) (one-fish))))]
      (is (instance? classes/RedFish (-> zg z/down z/node)))
      (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
      (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node)))

      (testing "and re-render in order"
        (rekt/re-render-graph!
          g (one-fish {} (red-fish) (blue-fish) (one-fish)))
        (is (instance? classes/RedFish (-> zg z/down z/node)))
        (is (instance? classes/BlueFish (-> zg z/down z/right z/node)))
        (is (instance? classes/OneFish (-> zg z/down z/right z/right z/node))))

      (testing "and can be removed"
        (rekt/re-render-graph!
          g (one-fish)))))

  (testing "A tree is destroyed and replaced when the head node's type changes"
    (let [g0 (rekt/re-render-graph! nil (one-fish {} (red-fish) (blue-fish)))
          old-one-fish g0
          old-red-fish (.getChildAt old-one-fish 0)
          old-blue-fish (.getChildAt old-one-fish 1)
          g1 (rekt/re-render-graph! g0 (blue-fish {} (one-fish) (red-fish)))
          new-blue-fish g1
          new-one-fish (.getChildAt new-blue-fish 0)
          new-red-fish (.getChildAt new-blue-fish 1)]
      (is (not= old-one-fish blue-fish))
      (is (not= old-red-fish new-one-fish))
      (is (not= old-blue-fish new-red-fish))
      (is (= true (.isDestroyed old-one-fish)))
      (is (= true (.isDestroyed old-red-fish)))
      (is (= true (.isDestroyed old-blue-fish)))))

  (testing "A subtree is destroyed and replaced"
    (let [g0 (rekt/re-render-graph! nil (one-fish {}
                                                  (red-fish {}
                                   (blue-fish) (blue-fish))))
          old-head g0
          old-red-fish (.getChildAt old-head 0)
          old-blue-fish1 (.getChildAt old-red-fish 0)
          old-blue-fish2 (.getChildAt old-red-fish 1)
          g1 (rekt/re-render-graph! g0 (one-fish {}
                                                 (blue-fish {} (red-fish) (red-fish))))
          new-head g1
          new-blue-fish (.getChildAt new-head 0)
          new-red-fish1 (.getChildAt new-blue-fish 0)
          new-red-fish2 (.getChildAt new-blue-fish 1)]
      (testing "and the head object did not change"
        (is (= old-head new-head)))

      (testing "and the new subtree is different"
        (is (not= old-red-fish new-blue-fish))
        (is (not= old-blue-fish1 new-red-fish1))
        (is (not= old-blue-fish2 new-red-fish2)))

      (testing "and the old subtree was destroyed"
        (is (= true (.isDestroyed old-red-fish)))
        (is (= true (.isDestroyed old-blue-fish1)))
        (is (= true (.isDestroyed old-blue-fish2)))))))



(deftest manipulate-generated-graph
  (testing "A generator creates a new virtual graph"
    (let [*render-props (atom [])
          *cleaned-up (atom false)
          generator {:render (fn [props] (swap! *render-props conj props)
                               (:v-graph props))
                     :cleanup (fn [] (reset! *cleaned-up true))}
          v-graph-0 (one-fish {} (red-fish) (blue-fish))
          g0 (rekt/re-render-graph! nil
                                    (rekt/generator-v-node
                 generator {:v-graph v-graph-0}))
          old-one-fish g0
          old-red-fish (.getChildAt old-one-fish 0)
          old-blue-fish (.getChildAt old-one-fish 1)]
      (is (= [{:v-graph v-graph-0}] @*render-props))
      (testing "and is not re-rendered if the props don't change"
        (let [same-g (rekt/re-render-graph! g0
                                            (rekt/generator-v-node
                         generator {:v-graph v-graph-0}))]
          (is (= g0 same-g))
          (is (= [{:v-graph v-graph-0}] @*render-props))))
      (testing "and is re-rendered correctly when props change"
        (let [v-graph-1 (red-fish {} (blue-fish) (one-fish))
              g1 (rekt/re-render-graph!
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
            (let [g2 (rekt/re-render-graph!
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
    (let [v-graph-0 (red-fish {} (blue-fish))
          r {:render (fn [props] (:v-graph props))}
          g0 (rekt/re-render-graph!
               nil (one-fish {}
                             (rekt/generator-v-node r {:v-graph v-graph-0})))
          o g0]
      (is (instance? classes/OneFish o))
      (is (= 1 (count (.getChildren o))))
      (let [old-red-fish (.getChildAt o 0)
            old-blue-fish (.getChildAt old-red-fish 0)]
        (is (instance? classes/RedFish old-red-fish))
        (is (instance? classes/BlueFish old-blue-fish))
        (testing "and destroys its objects when it goes away"
          (rekt/re-render-graph! g0 (one-fish {}
                                              (rekt/generator-v-node r {:v-graph nil})))
          (is (instance? classes/OneFish o))
          (is (= 0 (count (.getChildren o))))
          (is (= true (.isDestroyed old-red-fish)))
          (is (= true (.isDestroyed old-blue-fish)))))))

  (testing "A generator which yields a graph containing generators creates a proper graph"
    (let [gen1 {:render #(blue-fish)}
          gen0 {:render #(red-fish {} (rekt/generator-v-node gen1 {}))}
          o (rekt/re-render-graph!
              nil (one-fish {} (rekt/generator-v-node gen0 {})))]
      (is (instance? classes/OneFish o))
      (is (= 1 (count (.getChildren o))))
      (let [old-red-fish (.getChildAt o 0)
            old-blue-fish (.getChildAt old-red-fish 0)]
        (is (instance? classes/RedFish old-red-fish))
        (is (instance? classes/BlueFish old-blue-fish))
        (testing "and they all fall down"
          (rekt/re-render-graph! o (one-fish))
          (is (= 0 (count (.getChildren o))))
          (is (= 0 (count (.getChildren old-red-fish))))
          (is (= true (.isDestroyed old-red-fish)))
          (is (= true (.isDestroyed old-blue-fish))))))))

(deftest generator-life-cycle-functions
  (let [*generate-order (atom [])
        *cleanup-order (atom [])
        *gen-a-head (atom nil)
        *gen-b-head (atom nil)
        *gen-c-head (atom nil)
        gen-c {:render (fn [_ head-obj]
                         (swap! *generate-order conj :c)
                         (reset! *gen-c-head head-obj)
                         (red-fish))
               :cleanup (fn [head-obj]
                          (reset! *gen-c-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :c))}
        gen-b {:render (fn [_ head-obj]
                         (swap! *generate-order conj :b)
                         (reset! *gen-b-head head-obj)
                         (one-fish))
               :cleanup (fn [head-obj]
                          (reset! *gen-b-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :b))}
        gen-a {:render (fn [props head-obj]
                         (swap! *generate-order conj :a)
                         (reset! *gen-a-head head-obj)
                         (one-fish {}
                                   (rekt/generator-v-node gen-b props)
                                   (rekt/generator-v-node gen-c props)))
               :cleanup (fn [head-obj]
                          (reset! *gen-a-head head-obj)
                          (is (not (.isDestroyed head-obj))
                              "The object is not destroyed yet")
                          (swap! *cleanup-order conj :a))}
        v-graph #(rekt/generator-v-node gen-a %)]

    (testing "Generator life cycle functions are called in the correct order"
      (let [g (rekt/re-render-graph! nil (v-graph {}))
            zg (fish-zip g)]
        (testing "when creating a new graph"
          (is (= [:a :b :c] @*generate-order)))

        (testing "and have no head object available during first render"
          (is (nil? @*gen-a-head))
          (is (nil? @*gen-b-head))
          (is (nil? @*gen-c-head)))

        (testing "when re-rendering"
          (testing "doesn't re-render if it doesn't need to"
            (reset! *generate-order [])
            (rekt/re-render-graph! g (v-graph {}))
            (is (= [] @*generate-order)))

          (testing "it re-renders in correct order when it receives new props"
            (rekt/re-render-graph! g (v-graph {:poop "stewart"}))
            (is (= [:a :b :c] @*generate-order))

            (testing "and the correct head object references are passed in"
              (is (= g @*gen-a-head))
              (is (= (-> zg z/down z/node) @*gen-b-head))
              (is (= (-> zg z/down z/right z/node) @*gen-c-head)))))

        (testing "when being destroyed"
          (let [prev-a @*gen-a-head
                prev-b @*gen-b-head
                prev-c @*gen-c-head]
            (rekt/re-render-graph! g nil)
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


(deftest generators-with-global-state
  (testing "a single generator"
    (let [*state (atom {:a 0})
          *state-history (atom [])
          *cleaned-up (atom false)
          gen {:render (fn [props]
                         (let [the-state (rekt/get-in-state [:a])]
                           (swap! *state-history conj the-state)
                           (is (= (:a @*state) the-state)
                               "The correct state was observed"))
                         (blue-fish))
               :cleanup #(reset! *cleaned-up true)}
          g (rekt/reify-virtual-graph (one-fish) *state)]

      (testing "can read the initial state"
        (rekt/re-render-graph!
          g (one-fish {}
                      (rekt/generator-v-node gen))
          *state)
        (is (= [0] @*state-history) "The generator rendered once"))

      (testing "re-renders when state is changed externally"
        (reset! *state {:a 1})
        ;; TODO: There's an issue with reloading in this test somewhere (doesn't happen every time)
        (rekt/re-render-graph!
          g (one-fish {}
                      (rekt/generator-v-node gen))
          *state)
        (is (= [0 1] @*state-history) "The generator re-rendered once"))

      (testing "does not re-render if state did not change"
        (rekt/re-render-graph!
          g (one-fish {}
                      (rekt/generator-v-node gen))
          *state)
        (is (= [0 1] @*state-history) "The generator did not re-render"))

      (testing "does not re-render when removed from graph"
        (reset! *state {:a 2})
        (rekt/re-render-graph! g (one-fish) *state)
        (is (= [0 1] @*state-history) "The generator did not re-render")
        (is (= true @*cleaned-up) "The generator clean-up function was called"))))

  (testing "generator state mutators work correctly"
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
                           (one-fish)))}
          g (rekt/reify-virtual-graph (rekt/generator-v-node gen) *state)]
      (is (= 1 @*render-count))
      (swap! *state assoc-in [:a :b] 0)
      (rekt/re-render-graph! g (rekt/generator-v-node gen) *state)
      (is (= 2 @*render-count)
          "Should have rendered once more because generator is watching `[:a :b]`"))))
