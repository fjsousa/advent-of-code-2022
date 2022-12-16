(ns advent.12
  (:require [clojure.string :as str]))

;;This solution to part one if complete rubbish - the search algorithm gets stuck - naive approach

(def char-int-map
  {"E" 26
   "S" 1
   "a" 1
   "b" 2
   "c" 3
   "d" 4
   "e" 5
   "f" 6
   "g" 7
   "h" 8
   "i" 9
   "j" 10
   "k" 11
   "l" 12
   "m" 13
   "n" 14
   "o" 15
   "p" 16
   "q" 17
   "r" 18
   "s" 19
   "t" 20
   "u" 21
   "v" 22
   "w" 23
   "x" 24
   "y" 25
   "z" 26
   })

(def lines (str/split-lines (slurp (clojure.java.io/resource "12-ex.txt"))))

(def rows (count lines))
(def cols (-> lines first count))

(def starting-point (atom [0 0]))

(def end-point (atom [0 0]))

(def parsed-input
  (for [[v line] (map-indexed #(vector %1 %2) lines)
        [h char] (map-indexed #(vector %1 %2) (str/split line #""))]
    (do (when (= "S" char) (reset! starting-point [h v]))
        (when (= "E" char (reset! end-point [h v])))
        (get char-int-map char))))


;;build all possible paths

;;We need to model all possible options

;;starting at [0,0]
;;Sabqponm
;;abcryxxl
;;accszExk
;;acctuvwj
;;abdefghi

(defn get-point [[h v]]
  (when (and (not (neg? (* h v)))
             (< h cols)
             (< v rows))
      (let [idx (+ h (* v cols))
         idx (if (and (< idx (* cols rows)) (not (neg? idx))) idx nil)]
     (when idx
       (nth parsed-input idx)))))

(get-point '(3 0))


(defn get-neighbour [point neighbour-delta]
  (get-point (map + point neighbour-delta)))

(defn get-north [point]
  (get-neighbour point [0 -1]))

(defn get-south [point]
  (get-neighbour point [0 1]))

(defn get-east [point]
  (get-neighbour point [1 0]))

(defn get-west [point]
  (get-neighbour point [-1 0]))

(defn get-north-point [point]
  (map + [0 -1]  point))

(defn get-south-point [point]
  (map + [0 1]  point))

(defn get-east-point [point]
  (map + [1 0] point))

(defn get-west-point [point]
  (map + [-1 0] point))

(and
 (nil? (get-north [0 0]))
 (nil? (get-west [0 0]))
 (= 1 (get-south [0 0]))
 (= 1 (get-east [0 0]))
 (= 1 (get-point [0 0])))

(= '(7 4) @end-point)
(defn calculate-knot
  [point skip?]
  (println :visiting point)
  (if (= point @end-point)
    (do
      (println "end")
      {:point point
       :count 1})
    (let [point-value (get-point point)
          north (get-north point)
          go-north? (when (and (not (= skip? :skip-north)) north)
                      (>= 1 (- north point-value )))
          south (get-south point)
          go-south? (when (and (not (= skip? :skip-south)) south)
                      (>= 1 (- south point-value )))

          east (get-east point)
          go-east? (when (and (not (= skip? :skip-east)) east)
                     (>= 1 (- east point-value )))

          west (get-west point)
          go-west? (when (and (not (= skip? :skip-west)) west)
                     (>= 1 (- west point-value )))
          nodes (cond-> []
                  go-north? (conj (calculate-knot (get-north-point point) :skip-south))
                  go-south? (conj (calculate-knot (get-south-point point) :skip-north))
                  go-east? (conj (calculate-knot (get-east-point point) :skip-west))
                  go-west? (conj (calculate-knot (get-west-point point) :skip-east)))
          count (if (not-empty nodes)
                  (apply + (map :count nodes))
                  0)]
      {:points nodes
       :count count
       :point point
       }
      )

      ))

(calculate-knot [0 0] false #_@starting-point)
