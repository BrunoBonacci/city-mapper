(ns city-mapper.graph
  (:require [clojure
             [edn :as edn]
             [string :as str]]
            [loom
             [alg :refer :all]
             [graph :refer :all]
             [io :refer [view render-to-bytes]]]
            [where.core :refer [where]]
            [clojure.java.io :as io]))

(defn read-edn-lines [file]
  (->> (slurp file) (str/split-lines) (map edn/read-string)))


;; some anomaly with Penge West station
(defn st-lookup [stations station]
  (let [st (if (= station "9100PEMEW1") "910GPENEW" station)]
    (stations st)))


(comment

  ;; load data
  (def stations (->> (read-edn-lines "./resources/stations.edn") (map (juxt :id :name)) (into {})))
  (def legs     (read-edn-lines "./resources/legs.edn"))

  ;; map edges
  (def named-legs (->> legs (map #(-> % (update :from (partial st-lookup stations)) (update :to (partial st-lookup stations))))))

  ;; create graph
  (def tubemap (apply digraph (map (juxt :from :to) named-legs)))

  )


(comment
  ;; view graph
  (view tubemap)

  (io/copy (render-to-bytes tubemap) (io/file "./doc/London-TubeMap-Graph.png"))

  (time (bf-path tubemap "Tooting Bec" "Manor House"))

  (time (bf-path tubemap "Woolwich Arsenal" "Tooting Bec"))
  )
