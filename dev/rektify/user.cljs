(ns rektify.user)

(defrecord A [v children])

(defrecord B [v children])

(deftype C [v children])

(def one
  (with-meta (A. 42 (B. 42 nil)) {:yes false}))

(def two
  (with-meta (A. 42 (B. 42 nil)) {:yes false}))

(def three
  (A. 42 (A. 42 nil)))

(defn do-meta
  [v]
  (println (meta v))
  (alter-meta! v assoc :yes true)
  v)

(defn fun []
  (let [ ;t (A. 42 nil {:bull "pucky"})
        t (with-meta [1 2 3] {:bull "pucky"})
        ]
    (println (meta t))
    ;(alter-meta! t assoc-in [:bull] "fucky")
    (reset-meta! t {:bull "sucky"})
    (println (meta t))
    t))