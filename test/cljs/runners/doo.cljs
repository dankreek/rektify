(ns runners.doo
  (:require [doo.runner :refer-macros [doo-tests]]
            [runners.tests]))

(doo-tests
  'rektify.core-test)

