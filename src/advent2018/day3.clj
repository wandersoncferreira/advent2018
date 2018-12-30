(ns advent2018.day3
  (:require [clojure.string :as s]))

(def input (slurp "inputs/day3"))

;;; part 1
(defn parse-instructions
  [input-string]
  (let [rgx #"#(?<id>\d+)\s@\s(?<x1>\d+),(?<x2>\d+):\s(?<width>\d+)x(?<height>\d+)"
        matcher (re-matcher rgx input-string)]
    (if (.matches matcher)
      (->> ["x1" "x2" "width" "height" "id"]
           (map #(read-string (.group matcher %)))
           (zipmap [:x1 :x2 :width :height :id])))))

(defn build-coordinates
  [parsed-info]
  (let [xm (+ (:x1 parsed-info) (:width parsed-info))
        ym (+ (:x2 parsed-info) (:height parsed-info))
        mx (max xm ym)]
    (for [x1 (range (+ 1 mx)) :when (and (>= x1 (+ 1 (:x1 parsed-info))) (<= x1 xm))
          x2 (range (+ 1 mx)) :when (and (>= x2 (+ 1 (:x2 parsed-info))) (<= x2 ym))]
      [x1 x2])))

(defn compute-all-coordinates
  [input]
  (->> (s/split input #"\n")
       (map parse-instructions)
       (map build-coordinates)
       (apply concat)))

(defn solve
  [input]
  (let [cords (compute-all-coordinates input)
        frqs (frequencies cords)]
    (->> (for [[id freq] frqs
               :when (> freq 1)]
           id)
         count)))

(solve input)
;; => 116140

;;; part 2

