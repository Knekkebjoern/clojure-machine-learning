(ns clojure-machine-learning.core
  (:require [clojure.math.numeric-tower :as math]))

(defn verify-inputs
  ""
  [s]
  (loop [stream s result {:datapoint-count 0
                          :cardinality nil
                          :min-data-range []
                          :max-data-range []
                          :error nil}]
    (if (or (empty? stream)
            (nil? (first stream)))
      result
      (let [data (first stream)
            cnt (count data)
            current-datapoint-index (:datapoint-count result)
            result (update-in result [:datapoint-count] inc)
            ;; Our first datapoint. Set some values.
            result (if (nil? (:cardinality result))
                     (-> result
                         (assoc :cardinality cnt)
                         (assoc :min-data-range data)
                         (assoc :max-data-range data))
                     (-> result
                         ;; Find the min and max values for each dimension in the datapoint
                         (update-in [:min-data-range] #(vec (map min % data)))
                         (update-in [:max-data-range] #(vec (map max % data)))))
            cardinality (:cardinality result)]
        (cond
         (not (= cardinality cnt)) (assoc result
                                     :error (str "Invalid cardinality. Expected ["
                                                 cardinality "] but got ["
                                                 cnt "] for data point at index ["
                                                 current-datapoint-index "]"))
         :else (recur (rest stream) result))))))


(defn random-in-ranges
  "Takes two collections of numbers and returns a collection of numbers randomly selected within the range of integers for each index in the collections."
  [a b]
  (let [mins (map min a b)
        maxs (map max a b)
        range-sizes (map inc (map - maxs mins))]
    (loop [mins mins range-sizes range-sizes res []]
      (if (or (empty? mins)
              (empty? range-sizes))
        res
        (recur (rest mins)
               (rest range-sizes)
               (conj res (+ (first mins)
                            (rand-int (int (first range-sizes))))))))))

(defn eucledian-distance
  "Squared euclidian distance. Takes two sequences (matrices)."
  [p q]
  (let [diffs (for [x (partition 2 (interleave p q))] (apply - x))
        squares (map #(* % %) diffs)
        sum (apply + squares)
        sqroot (math/sqrt sum)]
    sqroot))

(def data (for [x (range 100)] [(rand-int 100) (rand-int 100)]))
(def k 3)
(let [metadata (verify-inputs data)
      centroids (for [x (range k)] (random-in-ranges (:min-data-range metadata)
                                                     (:max-data-range metadata)))]
  (println centroids))
