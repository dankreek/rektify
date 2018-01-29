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
                      (rekt/reify-generator nil)
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
          (is (= state-3 (:rektify.rektify/local-state
                           (vt/state next-gen)))))))

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
                                             "parent :post-generate pass correct args"))}
              reified-gen (rekt/reify-generator
                            (vt/generator gen-desc gen-props))]
          (is (= (rekt/&o-tree reify-generator) @*&gen-obj)
              "&o-tree generated was passed into child :post-generate")))

      (testing "local state propagates between functions"
        (let [child-state-0 {:aa 0}
              child-state-1 {:bb 1}
              child-state-2 {:cc 2}
              child-gen-desc {:init (fn [_props _children]
                                      (is (= nil @rekt/**cur-local-state*))
                                      (reset! rekt/**cur-local-state* child-state-0))
                              :generate (fn [_props state _children]
                                          (is (= child-state-0
                                                 @rekt/**cur-local-state*))
                                          (reset! rekt/**cur-local-state*
                                                  child-state-1)
                                          nil)
                              :post-generate (fn [_props state _&obj]
                                               (is (= child-state-1
                                                      @rekt/**cur-local-state*))
                                               (reset! rekt/**cur-local-state*
                                                       child-state-2))}
              parent-state-0 {:a 0}
              parent-state-1 {:b 1}
              parent-state-2 {:c 0}
              gen-desc {:init (fn [_props _children]
                                (is (= nil @rekt/**cur-local-state*))
                                (reset! rekt/**cur-local-state*))
                        :generate (fn [_props state _children]
                                    (is (= parent-state-0)
                                        @rekt/**cur-local-state*)
                                    (reset! rekt/**cur-local-state*
                                            parent-state-1)
                                    (vt/generator child-gen-desc))
                        :post-generate (fn [_props state _&obj]
                                         (is (= parent-state-1
                                                @rekt/**cur-local-state*))
                                         (reset! rekt/**cur-local-state*
                                                 parent-state-2))}
              gen (rekt/reify-generator (vt/generator gen-desc))
              gen-state (vt/state gen)]
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


(deftest get-in-state
  (testing "global state values are returned"
    (binding [rekt/*cur-global-state* {:a {:b "c"}}]
      (is (= {:b "c"} (rekt/get-in-state [:a])))
      (is (= "c" (rekt/get-in-state [:a :b])))))

  (testing "no defined global state throws an error"
    (is (thrown-with-msg?
          js/Error
          #"no global state is defined"
          (rekt/get-in-state [:tt-1 :tt-2])))))


(deftest subscribe
  (testing "global subscriptions are recorded"
    (binding [rekt/*cur-global-state* {:a {:b "c"}}
              rekt/**global-state-subscriptions* (atom {})]
      (let [a-val (rekt/subscribe [:a])
            ab-val (rekt/subscribe [:a :b])]
        (is (= (get-in rekt/*cur-global-state* [:a]) a-val))
        (is (= (get-in rekt/*cur-global-state* [:a :b]) ab-val))
        (is (= {[:a] a-val, [:a :b] ab-val}
               @rekt/**global-state-subscriptions*)))))

  (testing "nested subscriptions are recorded correctly"
    (binding [rekt/*cur-global-state* {:a {:b "c"}
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
    (binding [rekt/*cur-global-state* {}]
      (is (thrown-with-msg?
            js/Error
            #"can only be accessed during rektification"
            (rekt/subscribe [:tt-1 :tt-2]))))))

