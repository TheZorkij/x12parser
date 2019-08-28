(ns x12parser.core
  (:require [clojure.string :as str]))

(def schema {
  "835" {:nested #{"ISA"}}
  "ISA" {:nested #{"GS"}}
  "GS"  {:nested #{"ST"} :end #{"IEA"}}
  "ST"  {:nested #{"N1" "LX"} :end #{"GE" "ST"}}
  "N1"  {:end #{"SE" "LX" "N1"}}
  "LX"  {:nested #{"CLP"} :end #{"SE" "LX"}}
  "CLP" {:nested #{"SVC"} :end #{"SE" "LX" "CLP"}}
  "SVC" {:end #{"SE" "LX" "CLP" "SVC"}}})

(def input (slurp "/home/zorkij/HealthSamurai/x12parser/resources/sample.edi"))
(def segments (str/split (str/trim input ) #"~"))
(defn units [seg] (str/split (str/trim seg ) #"\*"))

(defn parser-core [id in acc]
  (let [seg-id (first (first in))
        {:keys [nested end]} (schema id)] ;takes nested and end from certain id
    (if (contains? nested seg-id) (parser-core seg-id in []))
     (if (or (empty? in) (and (contains? end seg-id) (not (empty? acc))))
       [acc in]
       (let [[v in-] (if (contains? nested seg-id)
                       (parser-core seg-id in [])
                       [(first in) (rest in)])]
         (recur id in- (conj acc v))))
    )
  )

(parser-core "835" (mapv units segments) [])
