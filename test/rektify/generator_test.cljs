(ns rektify.generator-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [test.classes-test :as classes :refer [fish-zip]]
            [rektify.generator :as g]
            [rektify.virtual-tree :as vt]
            [clojure.zip :as z]))

(def no-gen-v-tree
  "V-tree with no generator children"
  (vt/node classes/one-fish-desc {}
           [(vt/node classes/two-fish-desc {:in-the-beginning "was darkness"
                                            :and-then "along camne ra"})
            (vt/node classes/red-fish-desc)
            (vt/node classes/blue-fish-desc)]))


(deftest generator-state
  (testing "state functions work as expected with mock state atom"
    (is (thrown? js/Error (g/reset-state! {}))
        "Error thrown when attempting to mutate state outside of generator lifecycle function")

    ;; Manually binding an atom to *active-gen-state* to simulate the execution
    ;; of a lifecycle function
    (binding [g/*active-gen-state* (atom {:a "bee"})]
      (g/swap-state! merge {:c "deezenutz"})
      (is (= {:a "bee", :c "deezenutz"} @g/*active-gen-state*))

      (g/reset-state! {:oh-my "goodness!"})
      (is (= {:oh-my "goodness!"} @g/*active-gen-state*))))
  (testing "state transitions between each lifecycle phase"

    )
  )


(deftest generate
  (testing "single generator"
    (testing "generate a reified v-tree"
      (let [gen-props {:a "bee" :c "dee"}
            expected-vals* (atom {:props gen-props
                                  :local-state nil
                                  :v-tree no-gen-v-tree})
            gen-desc {:generate
                      (fn [props local-state]
                        (g/reset-state! {:new "state"})
                        (is (= (:props @expected-vals*) props)
                            "correct props passed into generate function")
                        (is (= (:local-state @expected-vals*) local-state)
                            "correct local state passed into generate function")
                        (:v-tree @expected-vals*))}
            gen (g/generator gen-desc gen-props)
            generated-gen (g/generate gen)
            orig-head& (g/obj& generated-gen)]

        (is (= no-gen-v-tree (g/v-tree generated-gen)))
        (is (instance? classes/OneFish (g/obj& generated-gen)))
        (is (= {:new "state"} (g/state generated-gen)))

        (testing "and regenerate the tree"
          (let [new-v-tree (vt/update-children no-gen-v-tree [])
                _ (reset! expected-vals* {:props {:e "eff" :g "eych"}
                                         :local-state {:new "state"}
                                         :v-tree (vt/node classes/one-fish-desc)})
                regenerated-gen (g/generate
                                  (g/update-props generated-gen
                                                  {:e "eff" :g "eych"}))]
            (is (= new-v-tree (g/v-tree regenerated-gen)))
            (is (identical? orig-head& (g/obj& regenerated-gen)))
            (is (= 0 (count (.getChildren orig-head&))))
            (is (= {:new "state"} (g/state regenerated-gen)))))))))
