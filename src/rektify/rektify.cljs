(ns rektify.rektify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def ^:dynamic *cur-local-state**
  "An atom containing the local state of the generate which is currently being
  rektified. This is used to contain all state transitions during life-cycle
  phases. The fully-transitioned state of state will not be available until the
  current lifecycle phase ends."
  nil)


