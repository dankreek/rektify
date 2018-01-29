(ns rektify.generator-test
  (:require [clojure.test :refer-macros [deftest is testing run-tests]]
            [rektify.generator :as gen]
            [rektify.virtual-tree :as vt]))


(deftest generator-desc?
  (testing "various forms of generator description are valid"
    (is (= true (gen/generator-desc? {:generate (fn [a b c])})))
    (is (= true (gen/generator-desc? {:init (fn [a])
                                      :generate (fn [a b c])})))
    (is (= true (gen/generator-desc? {:generate (fn [a b c])
                                      :post-generate (fn [a b])
                                      :pre-destroy (fn [a])}))))

  (testing "invalid generators return false"
    (is (= false (gen/generator-desc? {:init (fn [a])}))
        "Generators require a :render function")
    (is (= false (gen/generator-desc? {:invalid true}))
        ":invalid is not a valid generator method")))


(deftest init
  (testing "if defined, init is called"
    (let [called-with* (atom nil)
          props {:a 1}
          children [1 2 3]
          init-fn (fn [& args]
                    (reset! called-with* args))
          gen-desc {:init init-fn
                    :generate (fn [a b c])}]
      (gen/init gen-desc props children)
      (is (= [props children] @called-with*)
          "The correct args were passed to init")))

  (testing "no operation if no init function was defined"
    (is (nil? (gen/init {:generate (fn [a b c])} {:a 1} [])))))


(deftest generate
  (testing "generate is called and its return value is returned"
    (let [called-with* (atom nil)
          props {:tt 1}
          state {:tt 2}
          children []
          ret-val (vt/generator {:generate (fn [_ _ _])})
          gen-desc {:generate (fn [a b c]
                               (reset! called-with* [a b c])
                               ret-val)}]
      (is (= ret-val (gen/generate gen-desc props state children)))
      (is (= [props state children] @called-with*)))))


(deftest post-generate
  (testing "post-generate is called when defined"
    (let [called-with* (atom nil)
          props {:tt 1}
          state {:tt 2}
          tree& nil
          gen-desc {:generate (fn [a b c])
                    :post-generate (fn [& args]
                                     (reset! called-with* args))}]
      (gen/post-generate gen-desc props state tree&)
      (is (= [props state tree&] @called-with*))))

  (testing "post-generate not called if not defined"
    (is (= nil (gen/post-generate {:generate (fn [a b c])} nil nil nil)))))


(deftest pre-destroy
  (testing "pre-destroy is called when defined"
    (let [called-with* (atom nil)
          props {:tt 1}
          state {:tt 2}
          tree& nil
          gen-desc {:generate (fn [a b c])
                    :pre-destroy (fn [& args]
                                   (reset! called-with* args))}]
      (gen/pre-destroy gen-desc props state tree&)
      (is (= [props state tree&] @called-with*))))

  (testing "pre-destroy not called if not defined"
    (is (= nil (gen/pre-destroy {:generate (fn [a b c])} nil nil nil)))))
