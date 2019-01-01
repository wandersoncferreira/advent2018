(ns advent2018.day4
  (:require [clojure.string :as s]
            [clj-time.format :as f]
            [clj-time.core :as t]))

(def input (slurp "inputs/day4"))

;;; part 1
(defn guard-ids
  [input]
  (->> input
       (re-seq #"Guard\s#(\d+)")
       (map second)
       distinct))

(defn sort-input
  [input-line]
  (let [mtc (partial re-matcher #"\[(?<date>\d{4}-\d{2}-\d{2}\s\d{2}:\d{2})\]\s(?<msg>.*)")
        fmt (f/formatter "yyyy-MM-dd HH:mm")
        matcher (mtc input-line)]
    (if (.matches matcher)
      (hash-map (f/parse fmt (.group matcher "date")) (.group matcher "msg")))))

(defn guard-info
  [input guard-id]
  (let [entries (s/split input #"\n")
        data-sorted (->> entries
                         (map sort-input)
                         (into {})
                         sort)]
    (loop [entry data-sorted
           current-gId guard-id
           res []]
      (if (nil? (first entry))
        res
        (if-let [current-gId (second (re-find #"Guard\s#(\d+)" (second (first entry))))]
          (recur (rest entry) current-gId res)
          (if (= current-gId guard-id)
            (recur (rest entry) current-gId (cons [(t/day (first (first entry)))
                                                   (t/minute (first (first entry)))] res))
            (recur (rest entry) current-gId res)))))))

(defn guard-total-sleep
  [guard-info]
  (->> guard-info
       (map second)
       (partition-all 2)
       (map #(- (first %) (second %)))
       (reduce +)))

(defn guard-min-most-slept
  [guard-info]
  (->> guard-info
     (map second)
     (partition-all 2)
     (map #(range (second %) (first %)))
     flatten
     frequencies
     (sort-by val >)
     first))

(defn index-max-val
  [coll]
  (->> coll
       (map-indexed vector)
       (apply max-key second)
       first))

(defn solve-day4-part1
  [input]
  (let [ids (guard-ids input)
        infos (map (partial guard-info input) ids)
        total-sleeps (map guard-total-sleep infos)
        max-slept-id (index-max-val total-sleeps)]
    (* (first (guard-min-most-slept (nth infos max-slept-id)))
       (read-string (nth ids max-slept-id)))))

(solve-day4-part1 input)
;; => 101262


;;; part 2
(defn solve-day4-part2
  [input]
  (let [ids (guard-ids input)
        infos (map (partial guard-info input) ids)
        min-most-slepts (map guard-min-most-slept infos)
        choice-id (->> min-most-slepts
                       (remove nil?)
                       (map second)
                       index-max-val)]
    (* (read-string (nth ids choice-id))
         (first (nth min-most-slepts choice-id)))))

(solve-day4-part2 input)
;; => 71976
