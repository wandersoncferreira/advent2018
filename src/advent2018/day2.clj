(ns advent2018.day2
  (:require [clojure.set :as s]
            [clojure.string :as string]))

(def input (slurp "inputs/day2"))

;; part 1
(defn create-checksum
  [input]
  (let [words (clojure.string/split input #"\n")
        frq (map frequencies words)
        counters  {:words-f2 0 :words-f3 0}]
    (->> frq
     (map #(let [dt (clojure.set/map-invert %)]
             (cond
               (and (contains? dt 2) (contains? dt 3))
               (map (fn [x] (update counters x inc)) [:words-f3 :words-f2])
               (contains? dt 2)
               (update counters :words-f2 inc)
               (contains? dt 3)
               (update counters :words-f3 inc))))
     (filter not-empty)
     flatten
     (apply (partial merge-with +))
     (#(* (:words-f3 %) (:words-f2 %))))))

(create-checksum input)
;; => 7533

;; part 2
(defn diff-amount
  [str1 str2]
  (->> (map vector str1 str2)
       (filter #(= (first %) (second %)))))

(defn shared-latters
  [input]
  (let [words (string/split input #"\n")
        candidate (for [w1 words
                        w2 words
                        :let [letters (diff-amount w1 w2)
                              d (- (count w1) (count letters))]
                        :when (and (not= w1 w2) (= d 1))]
                    [w1 w2])
        similar (first candidate)]
    candidate
    (->> (diff-amount (first similar) (second similar))
         (map first)
         (apply str))))

(shared-latters input)
;; => "mphcuasvrnjzzkbgdtqeoylva"
