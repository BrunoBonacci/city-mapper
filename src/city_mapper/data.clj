(ns city-mapper.data
  (:require [clj-http.client :as http]
            [where.core :refer [where]]
            [clojure.string :as str]))

;;
;; APIs description available at: https://api.tfl.gov.uk/
;;

(defn GET [url]
  (println "(*) fetching:" url)
  (:body
   (http/get url {:accept :json :as :json})))


(defn- stations-sequence
  "Given a line (/Route/Sequence) returns all a list of stations id
  sequences for all branches"
  [data]
  (->> data :orderedLineRoutes (map :naptanIds) (map dedupe)))


(defn rename-key [map oldkey newkey]
  (if (and (not= oldkey newkey) (not= ::not-found (get map oldkey ::not-found)))
    (-> map
        (assoc newkey (get map oldkey))
        (dissoc oldkey))
    map))


(defn- stations-for-a-line
  "Given a line (/Route/Sequence) returns all stations for this line
  in all branches indexed by station id"
  [data]
  (->> data
       :stopPointSequences
       (mapcat :stopPoint)
       (map #(select-keys % [:name :stationId :zone]))
       (map #(-> %
                 (update :name str/replace #" Underground Station" "")
                 (update :name str/replace #" Rail Station" "")
                 (update :name str/replace #" DLR Station" "")
                 (update :name str/replace #" \(.*" "")
                 (update :zone (fn [z] (->> (when z (str/split z #"[^0-9]")) (into #{}))))
                 (rename-key :zone :zones)
                 (rename-key :stationId :id)))
       (map (juxt :id identity))
       (into {})))


(defn describe-line
  [lineid]
  (let [route-in  (GET (str "https://api.tfl.gov.uk/Line/" lineid "/Route/Sequence/inbound?excludeCrowding=True"))
        route-out (GET (str "https://api.tfl.gov.uk/Line/" lineid "/Route/Sequence/outbound?excludeCrowding=True"))
        sequences  (concat (stations-sequence route-in) (stations-sequence route-out))]
    {:line     lineid
     :stations (merge (stations-for-a-line route-in) (stations-for-a-line route-out))
     :sequences sequences
     :legs     (map (partial partition 2 1) sequences)}))


(defn all-lines
  "Returns a list of line services for the London underground system"
  []
  (->> (GET "https://api.tfl.gov.uk/Line/Route")
       (filter (where :modeName :IN? ["tube" "dlr" "overground"]))
       (map #(select-keys % [:name :modeName :id]))))



(defn all-stations-and-legs
  [lines]
  (doall (map describe-line (map :id lines))))


(defn all-stations-list [all-stations-and-legs]
  (->> all-stations-and-legs (map :stations) (apply merge) (map second) (sort-by :name)))


(defn all-legs-list [all-stations-and-legs]
  (->> all-stations-and-legs
       (mapcat (fn [{:keys [line legs]}]
                 (mapcat
                  (partial map (fn [[f t]]
                           {:from f :to t :line line})) legs)))
       (sort-by (juxt :line :from :to)) dedupe))


(defn write-lines-list [all-lines file]
  (spit file (str/join (map prn-str all-lines))))

(defn write-stations-list [all-stations-and-legs file]
  (spit file (str/join (map prn-str (all-stations-list all-stations-and-legs)))))

(defn write-legs-list [all-stations-and-legs file]
  (spit file (str/join (map prn-str (all-legs-list all-stations-and-legs)))))


(comment
  (describe-line "central")
  (def all (all-stations-and-legs (all-lines)))

  (write-lines-list    (all-lines) "./resources/lines.edn")
  (write-stations-list all "./resources/stations.edn")
  (write-legs-list     all "./resources/legs.edn")

  )
