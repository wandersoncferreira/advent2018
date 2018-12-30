(ns advent2018.day3
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]))

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

(defn parse-all-intructions
  [input]
  (->> (s/split input #"\n")
       (map parse-instructions)))

(defn build-coordinates
  [parsed-info]
  (let [xm (+ (:x1 parsed-info) (:width parsed-info))
        ym (+ (:x2 parsed-info) (:height parsed-info))
        mx (max xm ym)]
    (for [x1 (range (+ 1 mx)) :when (and (>= x1 (+ 1 (:x1 parsed-info))) (<= x1 xm))
          x2 (range (+ 1 mx)) :when (and (>= x2 (+ 1 (:x2 parsed-info))) (<= x2 ym))]
      [x1 x2])))


(defn solve
  [input]
  (let [intrs (parse-all-intructions input)
        cords (map build-coordinates intrs)
        unify (apply concat cords)
        frqs (frequencies unify)]
    (->> (for [[id freq] frqs
               :when (> freq 1)]
           id)
         count)))

(solve input)
;; => 116140

;;; part 2
(defn unique-coordinates
  [input]
  (let [unify (apply concat input)
        frqs (frequencies unify)]
    (for [[id f] frqs
          :when (= f 1)]
      id)))


(defn solve-part2
  [input]
  (let [intrs (parse-all-intructions input)
        cords (map build-coordinates intrs)
        conj-univ (set (unique-coordinates cords))]
    (->> (map-indexed (fn [idx keyy] (let [inter (intersection (set keyy) conj-univ)]
                                      (if (= (count inter) (count keyy))
                                        idx))) cords)
         (filter (complement nil?))
         first
         (nth intrs)
         :id)))

(solve-part2 input)
;; => 574

