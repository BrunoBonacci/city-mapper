(ns city-mapper.graph
  (:require [clojure
             [edn :as edn]
             [string :as str]]
            [loom
             [alg :refer :all]
             [graph :refer :all]
             [attr :refer :all]
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


(defn add-station [g {:keys [name id] :as node}]
  (as-> g $
    (add-nodes $ id)
    (add-attr-to-nodes $ :label name [id])
    (reduce #(apply add-attr %1 id %2) $ node)))

(defn add-legs [g {:keys [from to line]}]
  (as-> g $
    (add-edges $ [from to])
    (add-attr-to-edges $ :line line [from to])
    (add-attr-to-edges $ :label line [[from to]])))


(comment

  ;; load data
  (def stations (->> (read-edn-lines "./resources/stations.edn") (map (juxt :id :name)) (into {})))
  (def legs     (read-edn-lines "./resources/legs.edn"))


  (def tubemap
    (as-> (digraph) $
      (reduce add-station $ (read-edn-lines "./resources/stations.edn"))
      (reduce add-legs    $ (read-edn-lines "./resources/legs.edn"))))

  )


(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (cond
        (and s1 s2)
        (cons (first s1) (cons (first s2)
                               (interleave-all (rest s1) (rest s2))))
        s1
        (cons (first s1) (interleave-all (rest s1) (rest s2)))

        s2
        (cons (first s2)
              (interleave-all (rest s1) (rest s2))))))))



(defn find-path [tubemap from to]
  (let [sts (bf-path tubemap from to)
        lines (partition 2 1 sts)]
    (->> (interleave-all
          (map (partial attrs tubemap) sts)
          (map (partial apply attrs tubemap) lines))
         (partition-all 2))))


(defn build-instructions [path]
  (loop [[[station line] & legs] (map (partial map :label) path)
         steps []
         last-line nil]
    (if-not station
      (str/join ",\n" steps)
      (if-not (seq steps)
        (recur legs
               (conj steps (str "Go to " station " and take the " line " line"))
               line)
        (if (not= last-line line)
          (recur legs
                 (if line
                   (conj steps (str "change at " station " and take the " line " line"))
                   (conj steps (str "exit at " station " station."))) line)
          (recur legs steps line))))))

(comment
  ;; view graph
  (view tubemap)

  (io/copy (render-to-bytes tubemap) (io/file "./doc/London-TubeMap-Graph.png"))

  (time (bf-path tubemap "Tooting Bec" "Manor House"))
  (time (bf-path tubemap "Woolwich Arsenal" "Tooting Bec"))


  (def p (find-path tubemap "940GZZLUTBC" "940GZZLUMRH"))

  (build-instructions p)

  )
