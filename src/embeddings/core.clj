(ns embeddings.core
  (:require [embeddings.glove :as glove])
  (:gen-class))

(defn -main
  [& args]
  (glove/init)
  (println "vectors:"glove/vec-size " amount:" glove/amount))
