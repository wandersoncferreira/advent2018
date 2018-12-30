(ns advent2018.day1)

(def input (slurp "inputs/day1"))

;; part 1
(defn check-frequencies
  [input]
  (->> (clojure.string/split input #"\n")
       (reduce #(str (+ (read-string %1) (read-string %2))))))

(check-frequencies input)
;; => "556"

;; part 2
(defn aux
  [pre-process func]
  (->> pre-process
       (map-indexed hash-map)
       (map clojure.set/map-invert)
       (apply func)))

(defn first-freq-twice?
  [input]
  (let [pre-process (->> (clojure.string/split input #"\n")
                            (reductions #(str (+ (read-string %1)
                                                 (read-string %2)))))
        process (aux pre-process merge)
        pos-process (aux pre-process (partial merge-with +))
        diff-data (clojure.set/difference (set process)
                                          (set pos-process))]
    (if (not-empty diff-data)
      (->> diff-data
           (sort-by second)
           ((comp first first)))
      (recur (str input input)))))

(first-freq-twice? input)
;; => "448"
