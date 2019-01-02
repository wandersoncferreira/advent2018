(ns advent2018.day5
  (:require [clojure.string :as s]))

(def input (slurp "inputs/day5"))

;;; part 1
(defn get-candidates
  [input rgx-string]
  (->> input
       (re-seq (re-pattern rgx-string))
       flatten
       (filter #(= (count %) 2))))

(defn get-reactions-pattern
  [input]
  (->> (into (get-candidates input "[A-Z.][a-z]")
             (get-candidates input "[a-z.][A-Z]"))
       distinct
       (filter #(= (count (distinct (s/lower-case %))) 1))
       (s/join "|")))

(defn remove-reactions
  [input]
  (let [reactions (get-reactions-pattern input)]
    (if (empty? reactions)
      input
      (->> (s/replace input (re-pattern reactions) "")
           remove-reactions))))

(count (remove-reactions input))
;; => 11298
