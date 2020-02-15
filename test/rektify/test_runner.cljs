(ns rektify.test-runner
  (:require [test.classes]
            [test.classes-test]
            [rektify.object-test]
            [rektify.virtual-tree-test]
            [figwheel.main.testing :refer [run-tests]]))


(defn -main [& args]
  (run-tests 'test.classes-test)
  [:figwheel.main.async-result/wait 5000])

