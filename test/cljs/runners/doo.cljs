(ns runners.doo
  (:require [doo.runner :refer-macros [doo-tests]]
            [runners.tests]))

(doo-tests
  'test.classes-test
  'rektify.object-test
  'rektify.virtual-graph-test
  'rektify.generator-test)

