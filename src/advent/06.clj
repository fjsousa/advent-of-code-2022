(ns advent.06)

(defn count-until-sop [n]
  (->> (slurp (clojure.java.io/resource "06.txt"))
       (partition n 1)
       (map-indexed #(list %1 %2))
       (filter #(-> % last set count (= n)))
       ffirst
       (+ n)))

(count-until-sop 4);;1283
(count-until-sop 14);;3513
