(ns rektify.rektify-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [test.classes-test :as classes]
            [rektify.virtual-tree :as vt]
            [rektify.object :as o]
            [rektify.rektify :as rekt]
            [clojure.zip :as z]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generators for test objects

(defn one-fish
  [props & children]
  (vt/object classes/one-fish-desc props children))


(defn two-fish
  [props & children]
  (vt/object classes/two-fish-desc props children))


(defn red-fish
  [props & children]
  (vt/object classes/red-fish-desc props children))


(defn blue-fish
  [props & children]
  (vt/object classes/blue-fish-desc props children))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(deftest new-gen-state
  (let [gen (vt/generator {})
        init-state {:a "a"}
        gen-w-state (rekt/new-gen-state gen init-state)]
    (is (= init-state @(rekt/gen-state-atom gen-w-state)))))


(deftest merge-gen-state
  (let [init-state {:a "a"}
        gen (rekt/new-gen-state (vt/generator {}) init-state)
        next-state {:a "A", :b "C"}]
    (rekt/merge-gen-state gen next-state)
    (is (= next-state @(rekt/gen-state-atom gen)))))



(deftest reify-generator
  (testing "create o-tree:"
    (testing "no tree"
      (let [gen (vt/generator {:generate (fn [_ _ _])})
            reified-gen (rekt/reify-generator gen)]
        (is (nil? (rekt/&o-tree reified-gen)))))

    (testing "single object"
      (let [gen (vt/generator {:generate (fn [_ _ _]
                                           (one-fish {:some-prop "works"}))})
            o-tree& (-> gen
                      (rekt/reify-generator nil)
                      (rekt/&o-tree))]
        (is (instance? classes/OneFish o-tree&))
        (is (= "works" (o/prop classes/one-fish-desc o-tree& :some-prop)))))

    (testing "tree with children"
      (let [gen (vt/generator
                  {:generate (fn [_ _ _]
                               (one-fish {}
                                 (red-fish {})
                                 (blue-fish {})))})
            o-tree& (-> gen
                      (rekt/reify-generator nil)
                      (rekt/&o-tree))
            fz (classes/fish-zip o-tree&)]
        (is (instance? classes/OneFish o-tree&))
        (is (instance? classes/RedFish (-> fz z/down z/node)))
        (is (instance? classes/BlueFish (-> fz z/down z/right z/node)))))

    (testing "tree with generator children"
      (let [child-gen (vt/generator
                        {:generate (fn [_ _ _]
                                     (red-fish {}
                                       (blue-fish {}) (one-fish {})))})
            gen (vt/generator
                  {:generate (fn [_ _ _]
                               (one-fish {}
                                 (blue-fish {}) child-gen (one-fish {})))})
            o-tree& (-> gen
                      (rekt/reify-generator)
                      (rekt/&o-tree))
            fz (classes/fish-zip o-tree&)]
        (is (instance? classes/OneFish o-tree&))
        (is (instance? classes/BlueFish
                       (-> fz z/down z/node)))
        (is (instance? classes/RedFish
                       (-> fz z/down z/right z/node)))
        (is (instance? classes/OneFish
                       (-> fz z/down z/right z/right z/node)))
        (is (instance? classes/BlueFish
                       (-> fz z/down z/right z/down z/node)))
        (is (instance? classes/OneFish
                       (-> fz z/down z/right z/down z/right z/node)))))

    (testing "generator with children"
      (let [gen-desc {:generate (fn [_ _ children]
                                  (apply one-fish {} children))}
            gen (vt/generator gen-desc {} [(red-fish {}) (blue-fish {})])
            o-tree& (->
                      gen
                      (rekt/reify-generator nil)
                      (rekt/&o-tree))
            fz (classes/fish-zip o-tree&)]
        (is (instance? classes/OneFish o-tree&))
        (is (instance? classes/RedFish (-> fz z/down z/node)))
        (is (instance? classes/BlueFish (-> fz z/down z/right z/node))))))

  (testing "lifecycle functions:"
    (testing "single generator:"
      (testing "called in order with correct arguments"
        (let [call-order* (atom 0)
              init-args* (atom nil)
              generate-args* (atom nil)
              post-generate-args* (atom nil)
              gen-props {:tt 1, :tt-2 true}
              gen-children []
              gen (vt/generator
                    {:init (fn [& args]
                             (is (= 0 @call-order*))
                             (swap! call-order* inc)
                             (reset! init-args* args))
                     :generate (fn [& args]
                                 (is (= 1 @call-order*))
                                 (swap! call-order* inc)
                                 (reset! generate-args* args)
                                 (one-fish {:some-prop "is set"}))
                     :post-generate (fn [& args]
                                      (is (= 2 @call-order*))
                                      (reset! post-generate-args* args))}
                    gen-props
                    gen-children)
              o-tree& (-> gen
                        (rekt/reify-generator nil)
                        (rekt/&o-tree))]
          (is (= [gen-props gen-children] @init-args*)
              "init received correct props")
          (is (= [gen-props nil gen-children] @generate-args*)
              "generate received correct props")
          (is (= [gen-props nil o-tree&] @post-generate-args*)
              "post-generate received correct props")
          (is (= "is set" (o/prop classes/one-fish-desc o-tree& :some-prop)))))

      (testing "local state propagates between functions"
        (let [state-1 {:a 1}
              state-2 {:b 2}
              state-3 {:c 3}
              gen (vt/generator
                    {:init (fn [_ _]
                             (is (nil? @rekt/**cur-local-state*))
                             (reset! rekt/**cur-local-state* state-1))
                     :generate (fn [_ state _]
                                 (is (= state-1 state))
                                 (reset! rekt/**cur-local-state* state-2)
                                 (one-fish {}))
                     :post-generate (fn [_ state _]
                                      (is (= state-2 state))
                                      (reset! rekt/**cur-local-state* state-3))})
              next-gen (rekt/reify-generator gen nil)]
          (is (= state-3 (rekt/local-state next-gen))))))

    (testing "generator with generator children:"
      (testing "called in order with correct arguments"
        (let [*call-order (atom 0)
              *&gen-obj (atom false)
              gen-child-props {:heinz 57}
              gen-desc-child {:init (fn [& args]
                                      (is (= 2 @*call-order)
                                          "child :init called in correct order")
                                      (is (= [gen-child-props nil] args)
                                          "child :init called with correct args")
                                      (swap! *call-order inc))
                              :generate (fn [& args]
                                          (is (= 3 @*call-order)
                                              "child :generate called in correct order")
                                          (is (= [gen-child-props nil nil] args)
                                              "child :generate called with correct args")
                                          (swap! *call-order inc)
                                          nil)
                              :post-generate (fn [props state &obj]
                                               (reset! *&gen-obj &obj)
                                               (is (= 4 @*call-order)
                                                   "child :post-generate called in correct order")
                                               (is (= [props state &obj]
                                                      [gen-child-props nil &obj])
                                                   "child :post-generate called with correct args")
                                               (swap! *call-order inc))}
              gen-props {:a 1 :steak "sauce"}
              gen-desc {:init (fn [& args]
                                (is (= 0 @*call-order)
                                    "parent :init called in correct order")
                                (is (= [gen-props nil] args)
                                    "parent :init called with correct args")
                                (swap! *call-order inc))
                        :generate (fn [& args]
                                    (is (= 1 @*call-order)
                                        "parent :generate called in correct order")
                                    (is (= [gen-props nil nil] args)
                                        "parent :generate is passed correct args")
                                    (swap! *call-order inc)
                                    (vt/generator gen-desc-child gen-child-props))

                        :post-generate (fn [props state &obj]
                                         (is (= 5 @*call-order)
                                             "parent :post-generate called in correct order")
                                         (is (= [gen-props nil &obj]
                                                [props state &obj])
                                             "parent :post-generate pass correct args"))}]
          (rekt/reify-generator (vt/generator gen-desc gen-props))
          (is (= (rekt/&o-tree reify-generator) @*&gen-obj)
              "&o-tree generated was passed into child :post-generate")))

      (testing "local state propagates between functions"
        (let [child-state-0 {:aa 0}
              child-state-1 {:bb 1}
              child-state-2 {:cc 2}
              child-gen-desc {:init (fn [_props _children]
                                      (is (= nil @rekt/**cur-local-state*))
                                      (reset! rekt/**cur-local-state* child-state-0))
                              :generate (fn [_props _state _children]
                                          (is (= child-state-0
                                                 @rekt/**cur-local-state*))
                                          (reset! rekt/**cur-local-state*
                                                  child-state-1)
                                          nil)
                              :post-generate (fn [_props _state _&obj]
                                               (is (= child-state-1
                                                      @rekt/**cur-local-state*))
                                               (reset! rekt/**cur-local-state*
                                                       child-state-2))}
              parent-state-0 {:a 0}
              parent-state-1 {:b 1}
              parent-state-2 {:c 0}
              gen-desc {:init (fn [_props _children]
                                (is (= nil @rekt/**cur-local-state*))
                                (reset! rekt/**cur-local-state* parent-state-0))
                        :generate (fn [_props _state _children]
                                    (is (= parent-state-0)
                                        @rekt/**cur-local-state*)
                                    (reset! rekt/**cur-local-state*
                                            parent-state-1)
                                    (vt/generator child-gen-desc))
                        :post-generate (fn [_props _state _&obj]
                                         (is (= parent-state-1
                                                @rekt/**cur-local-state*))
                                         (reset! rekt/**cur-local-state*
                                                 parent-state-2))}
              gen (rekt/reify-generator (vt/generator gen-desc))]
          (is (= parent-state-2 (rekt/local-state gen)))
          (is (= child-state-2 (-> gen
                                 rekt/child-generators
                                 first
                                 rekt/local-state)))))))

  (testing "global state subscriptions: "
    (let [global-state {:a {:b "c"}}]
      (testing "single generator"
        (let [gen-desc {:generate (fn [_ _ _]
                                    (is (= {:b "c"} (rekt/subscribe [:a])))
                                    (is (= "c" (rekt/subscribe [:a :b])))
                                    nil)}
              reified-gen (rekt/reify-generator (vt/generator gen-desc)
                                                global-state)]
          (is (= {[:a] {:b "c"}, [:a :b] "c"}
                 (rekt/subscriptions reified-gen)))))

      (testing "generator with generator children"
        (let [child-gen-desc {:generate (fn [_ _ _]
                                          (is (= "c" (rekt/subscribe [:a :b])))
                                          nil)}
              gen-desc {:generate (fn [_ _ _]
                                    (is (= {:b "c"} (rekt/subscribe [:a])))
                                    (vt/generator child-gen-desc))}
              reified-gen (rekt/reify-generator (vt/generator gen-desc)
                                                global-state)]
          (is (= {[:a] {:b "c"}}
                 (rekt/subscriptions reified-gen)))
          (is (= {[:a :b] "c"}
                 (-> reified-gen
                   rekt/child-generators
                   first
                   rekt/subscriptions))))))))


(deftest destroy-generator
  (testing "objects in v-tree are correctly destroyed:"

    (testing "generator with no v-tree"
      (let [gen-desc {:generate (fn [_ _ _] nil)}
            reified-gen (rekt/reify-generator (vt/generator gen-desc))]
        (is (= nil (rekt/&o-tree reified-gen)))
        (rekt/destroy-generator reified-gen)))

    (testing "generator has a v-tree with no child generators:"
      (let [gen-desc {:generate (fn [_ _ _]
                                  (one-fish {}
                                    (red-fish {}) (blue-fish {})))}
            reified-gen (rekt/reify-generator (vt/generator gen-desc))
            fz (classes/fish-zip (rekt/&o-tree reified-gen))
            o (z/node fz)
            r (-> fz z/down z/node)
            b (-> fz z/down z/right z/node)]
        (is (= false (.isDestroyed o)))
        (is (= false (.isDestroyed r)))
        (is (= false (.isDestroyed b)))

        (is (= nil (rekt/destroy-generator reified-gen))
            "destroy-generator returns nil")

        (is (= true (.isDestroyed o)))
        (is (= true (.isDestroyed r)))
        (is (= true (.isDestroyed b)))))

    (testing "generator has a v-tree with a nested generator"
      (let [child-gen-desc {:generate (fn [_ _ _]
                                        (one-fish {}))}
            parent-gen-desc {:generate (fn [_ _ _]
                                         (one-fish {}
                                           (red-fish {})
                                           (vt/generator child-gen-desc)
                                           (blue-fish {})))}
            reified-gen (rekt/reify-generator (vt/generator parent-gen-desc))
            fz (classes/fish-zip (rekt/&o-tree reified-gen))
            o1 (z/node fz)
            r (-> fz z/down z/node)
            o2 (-> fz z/down z/right z/node)
            b (-> fz z/down z/right z/right z/node)]
        (is (= false (.isDestroyed o1)))
        (is (= false (.isDestroyed r)))
        (is (= false (.isDestroyed o2)))
        (is (= false (.isDestroyed b)))

        (is (= nil (rekt/destroy-generator reified-gen)))

        (is (= true (.isDestroyed o1)))
        (is (= true (.isDestroyed r)))
        (is (= true (.isDestroyed o2)))
        (is (= true (.isDestroyed b)))))

    (testing ":pre-destroy lifecycle function:"
      (testing "called on generator that has no v-tree"
        (let [*was-called (atom false)
              gen-props {:a 1}
              gen-state {:b 12}
              gen-desc {:generate (fn [_ _ _]
                                    (rekt/reset-local-state gen-state)
                                    nil)
                        :pre-destroy (fn [props state &obj-tree]
                                       (reset! *was-called true)
                                       (is (= gen-state state))
                                       (is (= gen-props props))
                                       (is (= nil &obj-tree)))}
              reified-gen (rekt/reify-generator
                            (vt/generator gen-desc gen-props))]
          (rekt/destroy-generator reified-gen)
          (is (= true @*was-called))))

      (testing "called on generator with v-tree with no generator children"
        (let [*was-called (atom false)
              gen-props {:a 1}
              gen-state {:b 12}
              *&gen-obj (atom nil)
              gen-desc {:generate (fn [_ _ _]
                                    (rekt/reset-local-state gen-state)
                                    (one-fish {}))
                        :pre-destroy (fn [props state &obj-tree]
                                       (reset! *was-called true)
                                       (is (= gen-state state))
                                       (is (= gen-props props))
                                       (is (= @*&gen-obj &obj-tree)))}
              reified-gen (rekt/reify-generator
                            (vt/generator gen-desc gen-props))]
          (reset! *&gen-obj (rekt/&o-tree reified-gen))
          (rekt/destroy-generator reified-gen)
          (is (= true @*was-called))))

      (testing "is called in order on generator with nested generators in v-tree"
        (let [*call-order (atom 0)
              child-gen-props {:poop "stewart"}
              child-gen-state {:snoop "dogg"}
              child-gen-desc {:generate (fn [_ _ _]
                                          (rekt/reset-local-state child-gen-state)
                                          (red-fish {}))
                              :pre-destroy (fn [props state &obj-tree]
                                             (is (= 1 (swap! *call-order inc)))
                                             (is (= child-gen-props props))
                                             (is (= child-gen-state state))
                                             (is (instance? classes/RedFish
                                                            &obj-tree)))}
              parent-gen-props {:one "potato"}
              parent-gen-state {:two "potatothreepotatofour"}
              parent-gen-desc {:generate (fn [_ _ _]
                                           (rekt/reset-local-state parent-gen-state)
                                           (one-fish {}
                                             (vt/generator child-gen-desc
                                                           child-gen-props)))
                               :pre-destroy (fn [props state &obj-tree]
                                              (is (= 2 (swap! *call-order inc)))
                                              (is (= parent-gen-props props))
                                              (is (= parent-gen-state state))
                                              (is (instance? classes/OneFish
                                                             &obj-tree)))}
              reified-gen (rekt/reify-generator (vt/generator parent-gen-desc
                                                              parent-gen-props))]
          (rekt/destroy-generator reified-gen)
          (is (= 2 @*call-order)))))))


(deftest destroy-v-tree
  (testing "destroy v-tree with no generator children"
    (let [v-tree (one-fish {}
                   (red-fish {}) (blue-fish {}))
          reified-v-tree (rekt/reify-v-tree v-tree (atom []))
          &o-tree (rekt/&o-tree reified-v-tree)
          fz (classes/fish-zip &o-tree)
          &r (-> fz z/down z/node)
          &b (-> fz z/down z/right z/node)]
      (is (nil? (rekt/destroy-v-tree reified-v-tree)))
      (is (= true (.isDestroyed &o-tree)))
      (is (= true (.isDestroyed &r)))
      (is (= true (.isDestroyed &b)))))

  (testing "destroy v-tree with generator children"
    (let [gen-props {:too "hot"}
          gen-state {:in-the "hot tub"}
          *was-called (atom false)
          gen-desc {:generate (fn [_ _ _]
                                (rekt/reset-local-state gen-state)
                                (blue-fish {}))
                    :pre-destroy (fn [props state &obj-tree]
                                   (reset! *was-called true)
                                   (is (= gen-props props))
                                   (is (= gen-state state))
                                   (is (instance? classes/BlueFish &obj-tree)))}
          *child-gens (atom [])
          v-tree (one-fish {}
                   (red-fish {}) (vt/generator gen-desc gen-props))
          reified-v-tree (rekt/reify-v-tree v-tree *child-gens)
          fz (classes/fish-zip (rekt/&o-tree reified-v-tree))
          &o (z/node fz)
          &r (-> fz z/down z/node)
          &b (-> fz z/down z/right z/node)]
      (is (nil? (rekt/destroy-v-tree reified-v-tree)))
      (is (= true @*was-called)
          "Generator's :pre-destroy function called")
      (is (= true (.isDestroyed &o)))
      (is (= true (.isDestroyed &r)))
      (is (= true (.isDestroyed &b))))))


(deftest regenerate
  (testing ":generate not called on generator that doesn't need regeneration"
    (testing "without global state"
      (let [*call-count (atom 0)
            gen (vt/generator {:generate (fn [_ _ _]
                                           (swap! *call-count inc)
                                           (one-fish {}))}
                              {:a "prop"})
            reified-gen (rekt/reify-generator gen)
            *prev-state-atom (rekt/gen-state-atom reified-gen)
            prev-state @*prev-state-atom
            &obj (rekt/&o-tree reified-gen)
            regenerated-gen (rekt/regenerate reified-gen gen)]
        (is (= 1 @*call-count) ":generate is only called once")
        (is (= prev-state @(rekt/gen-state-atom regenerated-gen))
            "Generator state is unchanged after regeneration")
        (is (= *prev-state-atom (rekt/gen-state-atom regenerated-gen))
            "State atom is the same instance after regeneration")
        (is (= &obj (rekt/&o-tree regenerated-gen))
            "Original object is returned after regeneration")))

    (testing "with global state"
      (let [*call-count (atom 0)
            global-state {:beef "cake"}
            gen (vt/generator {:generate (fn [_ _ _]
                                           (swap! *call-count inc)
                                           (rekt/subscribe [:beef])
                                           (one-fish {}))}
                              {:a "prop"})
            reified-gen (rekt/reify-generator gen global-state)
            *prev-state-atom (rekt/gen-state-atom reified-gen)
            prev-state @*prev-state-atom
            &obj (rekt/&o-tree reified-gen)
            regenerated-gen (rekt/regenerate reified-gen gen global-state)]
        (is (= 1 @*call-count) ":generate is only called once")
        (is (= prev-state @(rekt/gen-state-atom regenerated-gen))
            "Generator state is unchanged after regeneration")
        (is (= *prev-state-atom (rekt/gen-state-atom regenerated-gen))
            "State atom is the same instance after regeneration")
        (is (= &obj (rekt/&o-tree regenerated-gen))
            "Original object is returned after regeneration"))))

  (testing "regenerate yields the same virtual tree"
    (testing "with single generator"
      (let [*call-count (atom 0)
            gen-desc {:generate (fn [_ _ _]
                                  (swap! *call-count inc)
                                  (one-fish {}))}
            reified-gen (rekt/reify-generator (vt/generator gen-desc {:a 1}))
            &obj (rekt/&o-tree reified-gen)
            *prev-state-atom (rekt/gen-state-atom reified-gen)
            prev-state @*prev-state-atom
            regenerated-gen (rekt/regenerate
                              reified-gen (vt/generator gen-desc {:b 12}))]
        (is (= 2 @*call-count)
            ":generate is called twice")
        (is (= prev-state @(rekt/gen-state-atom regenerated-gen))
            "Generator state is unchanged after regeneration")
        (is (= *prev-state-atom (rekt/gen-state-atom regenerated-gen))
            "State atom is the same instance after regeneration")
        (is (= &obj (rekt/&o-tree regenerated-gen))
            "Original object is returned after regeneration")) )

    (testing "with nested generators"
      (let [*child-call-count (atom 0)
            child-gen-desc {:generate (fn [_ _ _]
                                        (rekt/subscribe [:a])
                                        (swap! *child-call-count inc)
                                        (red-fish {}))}
            *parent-call-count (atom 0)
            parent-gen-desc {:generate
                             (fn [_ _ _]
                               (rekt/subscribe [:a])
                               (swap! *parent-call-count inc)
                               (one-fish {}
                                 (vt/generator child-gen-desc)))}
            reified-gen (rekt/reify-generator (vt/generator parent-gen-desc) {:a 1})
            &o-tree (rekt/&o-tree reified-gen)
            regenerated-gen (rekt/regenerate
                              reified-gen (vt/generator parent-gen-desc) {:a 2})]
        (is (= 2 @*parent-call-count)
            "parent :generate is called twice")
        (is (= 2 @*child-call-count)
            "child :generate is called twice")
        (is (= &o-tree (rekt/&o-tree regenerated-gen))
            "Original parent object is returned after regeneration")
        (is (= (.getChildAt &o-tree 0)
               (.getChildAt (rekt/&o-tree regenerated-gen) 0))
            "Original child object is returned after regeneration"))

      (testing "and lifecycle functions are called in order with correct args"
        (let [*call-count (atom 0)
              child-props {:child :props}
              *cur-child-state (atom nil)
              child-gen-desc {:init (fn [props]
                                      (swap! *call-count inc)
                                      (is (= child-props props)
                                          "props passed to child :init")
                                      (is (= 3 @*call-count)
                                          "child :init called in order")
                                      ;; Set initial state using call count
                                      (rekt/reset-local-state
                                        (reset! *cur-child-state
                                                {:child-state @*call-count})))

                              :generate (fn [props state children]
                                          (rekt/subscribe [:a])
                                          (swap! *call-count inc)
                                          (is (= child-props props))
                                          (is (nil? children)
                                              "child :generate passed correct children")

                                          (is (= @*cur-child-state state)
                                              "child :generate is passed correct local state")
                                          ;; Update local state for next lifecycle function
                                          (rekt/reset-local-state
                                            (reset! *cur-child-state {:child-state @*call-count}))

                                          (is (or (= 4 @*call-count)
                                                  (= 8 @*call-count))
                                              "child :generate called in order")
                                          (red-fish {}))

                              :post-generate (fn [props state &o-tree]
                                               (swap! *call-count inc)
                                               (is (= child-props props))
                                               (is (= @*cur-child-state state)
                                                   "child :post-generate is passed correct local state")
                                               (rekt/reset-local-state
                                                 (reset! *cur-child-state {:child-state @*call-count}))

                                               (is (instance? classes/RedFish &o-tree)
                                                   "child :post-generate is passed correct &o-tree")

                                               (is (or (= 5 @*call-count)
                                                       (= 9 @*call-count))
                                                   "child :post-generate called in order"))}
              parent-props {:parent :props}
              *cur-parent-state (atom nil)
              parent-gen-desc {:init (fn [props]
                                      (swap! *call-count inc)
                                      (is (= 1 @*call-count)
                                          "parent :init called in order")
                                      (is (= parent-props props)
                                          "parent :init passed correct props")
                                      (rekt/reset-local-state
                                        (reset! *cur-parent-state
                                                {:parent @*call-count})))
                              :generate (fn [props state children]
                                          (rekt/subscribe [:a])
                                          (swap! *call-count inc)
                                          (is (= parent-props props)
                                              "parent :generate passed correct props")
                                          (is (= @*cur-parent-state state)
                                              "parent :generate passed correct state")
                                          (is (nil? children)
                                              "parent :generate passed correct children")

                                          (rekt/reset-local-state
                                            (reset! *cur-parent-state
                                                    {:parent @*call-count}))

                                          (is (or (= 2 @*call-count)
                                                  (= 7 @*call-count))
                                              "parent :generate called in order")

                                          (one-fish {}
                                            (vt/generator child-gen-desc child-props)))
                              :post-generate (fn [props state &o-tree]
                                               (swap! *call-count inc)
                                               (is (= parent-props props)
                                                   "parent :post-generate passed correct props")
                                               (is (= state @*cur-parent-state)
                                                   "parent :post-generate passed correct state")
                                               (is (instance? classes/OneFish &o-tree)
                                                   "parent :post-generate passed correct &o-tree")

                                               (rekt/reset-local-state
                                                 (reset! *cur-parent-state
                                                         {:parent @*call-count}))

                                               (is (or (= 6 @*call-count)
                                                       (= 10 @*call-count))
                                                   "parent :post-generate called in order"))}
              generator (vt/generator parent-gen-desc parent-props)]
          (-> generator
            (rekt/reify-generator {:a 1})
            (rekt/regenerate generator {:a 2}))))))

  (testing "regenerate yields same o-tree with modified props"
    (testing "with a single generator"
      (let [gen (vt/generator {:generate (fn [_ _ _]
                                           (let [val (rekt/subscribe [:val])]
                                             (one-fish {:some-prop val}
                                               (red-fish {:something [val val val]})
                                               (blue-fish {:kind-of-blue val}))))})

            reified-gen (rekt/reify-generator gen {:val 1})
            &init-one (rekt/&o-tree reified-gen)
            &init-red (.getChildAt &init-one 0)
            &init-blue (.getChildAt &init-one 1)

            regenerated-gen (rekt/regenerate reified-gen gen {:val 2})
            &regen-one (rekt/&o-tree regenerated-gen)
            &regen-red (.getChildAt &regen-one 0)
            &regen-blue (.getChildAt &regen-one 1)]

        (is (= &init-one &regen-one)
            "Parent object is not replaced")
        (is (= &init-red &regen-red)
            "First child is not replaced")
        (is (= &init-blue &regen-blue)
            "Second child is not replaced")

        (is (= 2 (o/prop classes/one-fish-desc &regen-one :some-prop))
            "Parent object's property updated")
        (is (= [2 2 2] (o/prop classes/red-fish-desc &regen-red :something))
            "First child's property updated")
        (is (= 2 (o/prop classes/blue-fish-desc &regen-blue :kind-of-blue))
            "Second child's property updated")))

    (testing "with a nested generator"
      (let [child-gen (vt/generator {:generate (fn [_ _ _]
                                                 (let [red-prop (rekt/subscribe [:red])]
                                                   (red-fish {:something red-prop})))})
            gen (vt/generator {:generate (fn [_ _ _]
                                           (let [one-prop (rekt/subscribe [:one])]
                                             (one-fish {:some-prop one-prop}
                                               child-gen
                                               (blue-fish {:kind-of-blue one-prop})
                                               (one-fish {:some-prop one-prop}))))})
            reified-gen (rekt/reify-generator gen {:one 0
                                                   :red [1 2 3]})
            &init-one (rekt/&o-tree reified-gen)
            &init-blue (.getChildAt &init-one 1)
            &init-child-on (.getChildAt &init-one 2)
            regenerated-gen (rekt/regenerate reified-gen gen {:one 1
                                                              :red [4 5 6]})
            &regen-o (rekt/&o-tree regenerated-gen)
            &regen-blue (.getChildAt &regen-o 1)
            &regen-child-one (.getChildAt &regen-o 2)]

        (is (= &init-one &regen-o)
            "Parent object is not replaced")
        (is (= (.getChildAt &init-one 0) (.getChildAt &regen-o 0))
            "Generator child object is not replaced")
        (is (= &init-blue &regen-blue)
            "Second child is not replaced")
        (is (= &init-child-on &regen-child-one)
            "Third child is not replaced")

        (is (= 1 (o/prop classes/one-fish-desc &regen-o :some-prop))
            "Parent object's properties updated")
        (is (= [4 5 6] (o/prop classes/red-fish-desc
                               (.getChildAt &regen-o 0) :something))
            "Generator child object's properties updated")
        (is (= 1 (o/prop classes/blue-fish-desc &regen-blue :kind-of-blue))
            "Second child's properties updated")
        (is (= 1 (o/prop classes/one-fish-desc &regen-child-one :some-prop)))))


    (testing "and lifecycle functions are called in the correct order with correct args"
      (let [*call-count (atom 0)
            child-props {:child :props}
            *cur-child-state (atom nil)
            child-gen-desc {:init (fn [props]
                                    (swap! *call-count inc)
                                    (is (= child-props props)
                                        "props passed to child :init")
                                    (is (= 3 @*call-count)
                                        "child :init called in order")
                                    ;; Set initial state using call count
                                    (rekt/reset-local-state
                                      (reset! *cur-child-state
                                              {:child-state @*call-count})))

                            :generate (fn [props state children]
                                        (let [val (rekt/subscribe [:a])]
                                          (swap! *call-count inc)
                                          (is (= child-props props))
                                          (is (nil? children)
                                              "child :generate passed correct children")

                                          (is (= @*cur-child-state state)
                                              "child :generate is passed correct local state")
                                          ;; Update local state for next lifecycle function
                                          (rekt/reset-local-state
                                            (reset! *cur-child-state {:child-state @*call-count}))

                                          (is (or (= 4 @*call-count)
                                                  (= 8 @*call-count))
                                              "child :generate called in order")
                                          (red-fish {:something [val val val]})))

                            :post-generate (fn [props state &o-tree]
                                             (swap! *call-count inc)
                                             (is (= child-props props))
                                             (is (= @*cur-child-state state)
                                                 "child :post-generate is passed correct local state")
                                             (rekt/reset-local-state
                                               (reset! *cur-child-state {:child-state @*call-count}))

                                             (is (instance? classes/RedFish &o-tree)
                                                 "child :post-generate is passed correct &o-tree")

                                             (is (or (= 5 @*call-count)
                                                     (= 9 @*call-count))
                                                 "child :post-generate called in order"))}
            parent-props {:parent :props}
            *cur-parent-state (atom nil)
            parent-gen-desc {:init (fn [props]
                                     (swap! *call-count inc)
                                     (is (= 1 @*call-count)
                                         "parent :init called in order")
                                     (is (= parent-props props)
                                         "parent :init passed correct props")
                                     (rekt/reset-local-state
                                       (reset! *cur-parent-state
                                               {:parent @*call-count})))
                             :generate (fn [props state children]
                                         (let [val (rekt/subscribe [:a])]
                                           (swap! *call-count inc)
                                           (is (= parent-props props)
                                               "parent :generate passed correct props")
                                           (is (= @*cur-parent-state state)
                                               "parent :generate passed correct state")
                                           (is (nil? children)
                                               "parent :generate passed correct children")

                                           (rekt/reset-local-state
                                             (reset! *cur-parent-state
                                                     {:parent @*call-count}))

                                           (is (or (= 2 @*call-count)
                                                   (= 7 @*call-count))
                                               "parent :generate called in order")

                                           (one-fish {:some-prop val}
                                             (vt/generator child-gen-desc child-props))))

                             :post-generate (fn [props state &o-tree]
                                              (swap! *call-count inc)
                                              (is (= parent-props props)
                                                  "parent :post-generate passed correct props")
                                              (is (= state @*cur-parent-state)
                                                  "parent :post-generate passed correct state")
                                              (is (instance? classes/OneFish &o-tree)
                                                  "parent :post-generate passed correct &o-tree")

                                              (rekt/reset-local-state
                                                (reset! *cur-parent-state
                                                        {:parent @*call-count}))

                                              (is (or (= 6 @*call-count)
                                                      (= 10 @*call-count))
                                                  "parent :post-generate called in order"))}
            generator (vt/generator parent-gen-desc parent-props)]
        (-> generator
          (rekt/reify-generator {:a 1})
          (rekt/regenerate generator {:a 2}))))))


(deftest get-in-state
  (testing "global state values are returned"
    (binding [rekt/*global-state* {:a {:b "c"}}]
      (is (= {:b "c"} (rekt/get-in-state [:a])))
      (is (= "c" (rekt/get-in-state [:a :b])))))

  (testing "no defined global state throws an error"
    (is (thrown-with-msg?
          js/Error
          #"no global state is defined"
          (rekt/get-in-state [:tt-1 :tt-2])))))


(deftest subscribe
  (testing "global subscriptions are recorded"
    (binding [rekt/*global-state* {:a {:b "c"}}
              rekt/**global-state-subscriptions* (atom {})]
      (let [a-val (rekt/subscribe [:a])
            ab-val (rekt/subscribe [:a :b])]
        (is (= (get-in rekt/*global-state* [:a]) a-val))
        (is (= (get-in rekt/*global-state* [:a :b]) ab-val))
        (is (= {[:a] a-val, [:a :b] ab-val}
               @rekt/**global-state-subscriptions*)))))

  (testing "nested subscriptions are recorded correctly"
    (binding [rekt/*global-state* {:a {:b "c"}
                                       :c {3 "po"}}
              rekt/**global-state-subscriptions* (atom {})]
      (rekt/subscribe [:a])
      (binding [rekt/**global-state-subscriptions* (atom {})]
        (rekt/subscribe [:c])
        (rekt/subscribe [:c 3])
        (is (= {[:c] {3 "po"}
                [:c 3] "po"}
               @rekt/**global-state-subscriptions*)))
      (rekt/subscribe [:a :b])
      (is (= {[:a] {:b "c"}
              [:a :b] "c"}
             @rekt/**global-state-subscriptions*))))

  (testing "subscribe outside of rektification throws an error"
    (binding [rekt/*global-state* {}]
      (is (thrown-with-msg?
            js/Error
            #"can only be accessed during rektification"
            (rekt/subscribe [:tt-1 :tt-2]))))))

