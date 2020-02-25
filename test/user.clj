(ns user
  (:require [figwheel.main.api :as fig]))


(defn repl []
  (fig/start "dev"))


(defn -main [& args] (repl))