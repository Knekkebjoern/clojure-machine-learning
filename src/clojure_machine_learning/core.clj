(ns clojure-machine-learning.core
  (:require [clojure.math.numeric-tower :as math]))

(defn filter-string-to-int-array [s]
  (cond
   (nil? s) nil
   (empty? s) :invalid
   :else (try
           (let [res (doall (for [x (clojure.string/split s #"\s+")] (Integer/parseInt x)))]
             (if (empty? res)
               :invalid
               res))
           (catch NullPointerException e :invalid)
           (catch java.lang.NumberFormatException e :invalid))))

(defn stream-from-file [filename line-filter]
  (defn helper [rdr]
    (lazy-seq
     (let [line (line-filter (.readLine rdr))]
       (if (nil? line)
         (do
           (.close rdr)
           (println "-- Closed stream [" filename "]")
           nil)
         (if (not (= :invalid line))
           (cons line (helper rdr))
           (recur))))))
  (lazy-seq
   (do (println "-- Opened stream [" filename "]")
       (helper (clojure.java.io/reader filename)))))

(defn verify-inputs
  ""
  [s]
  (println "Verifying inputs on stream")
  (loop [stream s result {:datapoint-count 0
                          :cardinality nil
                          :min-data-range []
                          :max-data-range []
                          :error nil}]
    (if (nil? (first stream))
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

(defn get-dimension-data [coll]
  (loop [coll coll res []]
    (let [r (map first coll)]
      (if (some nil? r)
        res
        (recur (map rest coll) (conj res r))))))

(defn eucledian-distance
  "Squared euclidian distance. Takes two sequences (matrices)."
  [p q]
  (let [diffs (for [x (get-dimension-data [p q])] (apply - x))
        squares (map #(* % %) diffs)
        sum (apply + squares)
        sqroot (math/sqrt sum)]
    sqroot))

(defn get-nearest-centroid [centroids f datapoint]
  (let [distances (for [c centroids] (f c datapoint))
        index-of-min (.indexOf distances (apply min distances))
        nearest-centroid (nth centroids index-of-min :unknown)]
    nearest-centroid))

(defn get-centroids [centroids f stream]
  (println "Locating centroids")
  (loop [stream stream res {}]
    (if (nil? (first stream))
      (keys res)
      (let [datapoint (first stream)
            nearest-centroid (get-nearest-centroid centroids f datapoint)
            centroid-data (or (get res nearest-centroid) {:count 0 :sums (for [x datapoint] 0)})
            new-centroid-data {:count (inc (:count centroid-data))
                               :sums (doall (for [x (:sums centroid-data)] (inc x)))}]
        (recur (rest stream)
               (assoc res nearest-centroid new-centroid-data))))))

(comment
(def filename "/Users/jorge/src/clojure-machine-learning/data.txt")
  (verify-inputs (stream-from-file filename filter-string-to-int-array))

(let [k 3
      filename "/Users/jorge/src/clojure-machine-learning/data.txt"
      stream (stream-from-file filename filter-string-to-int-array)
      metadata (verify-inputs stream)]
  (get-centroids (for [x (range k)]
                   (random-in-ranges (:min-data-range metadata)
                                     (:max-data-range metadata)))
                 eucledian-distance
                 (stream-from-file
                  filename
                  filter-string-to-int-array)))

(k-means 3 "/Users/jorge/src/clojure-machine-learning/data.txt")
  )

(defn k-means [k filename]
  (let [stream (stream-from-file filename filter-string-to-int-array)
        metadata (verify-inputs stream)
        centroids (loop [;; Start off with some random centroids
                         centroids (for [x (range k)]
                                     (random-in-ranges (:min-data-range metadata)
                                                       (:max-data-range metadata)))]
                    (let [new-centroids (get-centroids centroids
                                            eucledian-distance
                                            (stream-from-file filename filter-string-to-int-array))]
                      (println "Centroids :" new-centroids)
                      (if (= (set new-centroids)
                             (set centroids))
                        centroids
                        (recur new-centroids))))
        filehandles (doall
                     (for [index (range (count centroids))]
                       (clojure.java.io/writer (str filename ".centroid." index))))
        filehandles-map (apply array-map (interleave centroids filehandles))]
    (println "Writing results")
    ;; Write the data to files
    (loop [data (stream-from-file filename filter-string-to-int-array)]
      (if (empty? data)
        nil
        (let [datapoint (first data)
              nearest-centroid (get-nearest-centroid centroids eucledian-distance datapoint)
              filehandle (get filehandles-map nearest-centroid)
              outstring (apply str (interpose " " datapoint))]
          (if (not (nil? filehandle))
            (.write filehandle (str outstring "\n")))
          (recur (rest data)))))
    (println "Closing output files")
    (doall (map #(.close %) filehandles))
    centroids))
