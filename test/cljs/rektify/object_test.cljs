(ns rektify.object-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [test.classes-test :as classes]
            [goog.object :as object]
            [rektify.object :as o]))


(deftest parent
  (testing "return a reference to the parent using :get-parent"
    (let [one-fish (new classes/OneFish)
          red-fish (new classes/RedFish)]
      (.addChild one-fish red-fish)
      (= one-fish (o/parent classes/one-fish-desc one-fish)))))


(deftest children
  (testing "return a list of children using :get-children"
    (let [one-fish (new classes/OneFish)
          first-child (new classes/OneFish)
          second-child (new classes/OneFish)]
      (.addChild one-fish first-child)
      (.addChild one-fish second-child)
      (= [first-child second-child]
         (o/children classes/one-fish-desc one-fish)))))


(deftest add-child!
  (testing "add a child using :add-child"
    (let [one-fish (new classes/OneFish)
          red-fish (new classes/RedFish)]
      (is (= red-fish (o/add-child! classes/one-fish-desc one-fish red-fish))
          "did not return a reference to the new child")
      (is (= 1 (count (o/children classes/one-fish-desc one-fish))))
      (is (= red-fish (first (o/children classes/one-fish-desc one-fish)))))))


(deftest replace-child!
  (testing "replace a child using :replace-child"
    (let [one-fish (new classes/OneFish)
          red-fish (new classes/RedFish)
          blue-fish (new classes/BlueFish)]
      (o/add-child! classes/one-fish-desc one-fish red-fish)
      (is (= red-fish
             (o/replace-child! classes/one-fish-desc one-fish red-fish blue-fish))
          "did not return a reference to the old child")
      (is (= 1 (count (o/children classes/one-fish-desc one-fish)))
          "one-fish should still have 1 child after replacing the old child")
      (is (= blue-fish (first (o/children classes/one-fish-desc one-fish)))
          "the child was not replaced"))))


(deftest remove-child!
  (testing "remove a child using :remove-child"
    (let [one-fish (new classes/OneFish)
          red-fish (new classes/RedFish)]
      (o/add-child! classes/one-fish-desc one-fish red-fish)
      (is (= red-fish (o/remove-child! classes/one-fish-desc one-fish red-fish))
          "remove-child! did not return a reference to the removed child")
      (is (= 0 (count (o/children classes/one-fish-desc one-fish)))))))


(deftest child-index
  (let [&one-fish (new classes/OneFish)
        &red-fish (new classes/RedFish)]
    (testing "`nil` is returned when child does not exist"
      (is (nil? (o/child-index classes/one-fish-desc &one-fish &red-fish))))
    (testing "correct index of child is returned"
      (is (= &red-fish)
          (o/add-child! classes/one-fish-desc &one-fish &red-fish))
      (is (= 0 (o/child-index classes/one-fish-desc &one-fish &red-fish))))))


(deftest destroy!
  (testing "destroy! calls destructor function"
    (let [one-fish (new classes/OneFish)]
      (o/destroy! classes/one-fish-desc one-fish)
      (is (= true (.isDestroyed one-fish)))))

  (testing "destroy! does not call a destructor function if it doesn't exist"
    (let [one-fish (new classes/OneFish)]
      (o/destroy! (dissoc classes/one-fish-desc :destructor) one-fish)
      (is (= false (.isDestroyed one-fish))))))


(deftest prop
  (let [one-fish (new classes/OneFish)]
    (set! (.-someProp one-fish) "something")

    (testing "Returns an object's prop"
      (is (= "something" (o/prop classes/one-fish-desc one-fish :some-prop))))

    (testing "Throws an error if an invalid prop is requested"
      (is (thrown-with-msg?
            js/Error
            #"There is no definition for the property :invalid"
            (o/prop classes/one-fish-desc one-fish :invalid))))))


(deftest props
  (testing "Returning all of an object's props"
    (let [one-fish (new classes/OneFish)]
      (set! (.-someProp one-fish) "something")
      (set! (.-optionalParam one-fish) "another thing")
      (is (= {:some-prop "something"
              :optional "another thing"}
             (o/props classes/one-fish-desc one-fish))))))


(deftest set-prop!
  (let [one-fish (new classes/OneFish)
        prop-map (:prop-map classes/one-fish-desc)]
    (testing "Setting a prop"
      (o/set-prop! prop-map one-fish :some-prop "someval")
      (is (= "someval"
             (o/prop classes/one-fish-desc one-fish :some-prop))))

    (testing "Setting a non-existent prop throws an error"
      (is (thrown-with-msg?
            js/Error
            #"No :setter found for property :no-exist"
            (o/set-prop! prop-map one-fish :no-exist 0)))))

  ;; TODO: Make a :ready-only flag for this test (Issue #22)
  (testing "Setting a read-only prop throws an error"))


(deftest set-props!
  (testing "Setting a map of props"
    (let [one-fish (new classes/OneFish)]
      (o/set-props! classes/one-fish-desc one-fish
                    {:some-prop "in the way she moves"
                     :optional "attracts me like no other"})
      (is (= "in the way she moves"
             (o/prop classes/one-fish-desc one-fish :some-prop)))
      (is (= "attracts me like no other"
             (o/prop classes/one-fish-desc one-fish :optional))))))


(deftest set-default-prop!
  (testing "Sets the proper default property"
    (let [one-fish (new classes/OneFish)]
      (o/set-prop! (:prop-map classes/one-fish-desc) one-fish :some-prop "wacky")
      (o/set-default-prop! classes/one-fish-desc one-fish :some-prop)
      (is (= false (o/prop classes/one-fish-desc one-fish :some-prop)))))

  (testing "Throws an error if no default property is available"
    (let [red-fish (new classes/RedFish)]
      (is (thrown-with-msg?
            js/Error
            #"does not have a default value set"
            (o/set-default-prop! classes/red-fish-desc red-fish :something))))))


(deftest update-props!
  (let [one-fish (new classes/OneFish)
        first-props {:some-prop 12}
        next-props {:some-prop 13}]

    (testing "Sets properties when none previously set"
      (o/update-props! classes/one-fish-desc one-fish first-props {})
      (is (= 12 (o/prop classes/one-fish-desc one-fish :some-prop))))

    (testing "Sets newly set properties"
      (o/update-props! classes/one-fish-desc one-fish next-props first-props)
      (is (= 13 (o/prop classes/one-fish-desc one-fish :some-prop))))

    (testing "Sets defaults for properties that are no longer set"
      (o/update-props! classes/one-fish-desc one-fish {} next-props)
      (is (= (get-in classes/one-fish-desc [:default-props :some-prop])
             (o/prop classes/one-fish-desc one-fish :some-prop)))))

  (testing "An error is thrown if no default exists for a removed property"
    (let [init-props {:something [1 2 3]}
          red-fish (new classes/RedFish)]
      (is (thrown-with-msg?
            js/Error #"does not have a default value"
            (o/update-props! classes/red-fish-desc red-fish {} init-props))))))


(deftest construct-obj!
  (testing "Construct an object with no constructor list using default constructor"
    (let [blue-fish (o/construct-obj! classes/blue-fish-desc)]
      (is (instance? classes/BlueFish blue-fish))))

  (testing "Construct an object with the default constructor and then set props"
    (let [blue-fish (o/construct-obj! classes/blue-fish-desc {:kind-of-blue true})]
      (is (instance? classes/BlueFish blue-fish))
      (is (= true (o/prop classes/blue-fish-desc blue-fish :kind-of-blue))
          "props were not set after object construction")))

  (testing "Construct an object with all defined props of a parameterized constructor"
    (let [two-fish (o/construct-obj! classes/two-fish-desc
                                     {:in-the-beginning "was darkness"
                                      :and-then "along came Ra"})]
      (is (instance? classes/TwoFish two-fish))
      (is (= "was darkness"
             (o/prop classes/two-fish-desc two-fish :in-the-beginning)))
      (is (= "along came Ra"
             (o/prop classes/two-fish-desc two-fish :and-then)))))

  (testing "Construct an object with defined and default props of a parameterized constructor"
    (let [init-props {:and-then "I swang"}
          two-fish (o/construct-obj! classes/two-fish-desc init-props)]
      (is (instance? classes/TwoFish two-fish))
      (is (= (get-in classes/two-fish-desc [:default-props :in-the-beginning])
             (o/prop classes/two-fish-desc two-fish :in-the-beginning)))
      (is (= "I swang"
             (o/prop classes/two-fish-desc two-fish :and-then)))
      (is (= classes/two-fish-desc (.-postConstructorObjDesc two-fish))
          "post constructor was called with correct obj-desc")
      (is (= init-props (.-postConstructorProps two-fish))
          "post constructor was called with correct props")))

  (testing "The first constructor in a size-2 constructor list is called"
    (let [one-fish (o/construct-obj! classes/one-fish-desc {:optional true})]
      (is (instance? classes/OneFish one-fish))
      (is (= true (o/prop classes/one-fish-desc one-fish :optional)))))

  (testing "An error is thrown if no constructor available"
    (is (thrown? js/Error (o/construct-obj! classes/two-fish-desc)))))