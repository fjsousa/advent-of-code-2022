(ns advent.15)

;;Part 1 fully functional and quite concise TBH
(def y 2000000)

(defn ΔM
  "Taxicab dist"
  [p1 p2]
  (reduce + (map #(Math/abs (- %1 %2)) p1 p2)))

(defn point-within-coverage?
  "returns true if point is within the area defined by the manhattan distance between two points"
  [p1 p2 p]
  (<= (ΔM p1 p) (ΔM p1 p2)))

(comment (point-within-coverage? [8 7] [2 10] [17 7]))

(defn ->x-interval
  "Given a pair of points for beacon and a horizontal line Y, return the interval [x1 x2]
  where it intersects with the area covered"
  [y [[xs ys :as S] B ]]
  (let [Δh (memoize
            (fn []
              (- (ΔM B S) (Math/abs (- ys y )))))]

    (when (point-within-coverage? S B [xs y])
        [(- xs (Δh)) (+ xs (Δh))])))

(comment
  (->x-interval y [[8 7] [2 10]])
  (->x-interval y [[2 18] [-2 15]]))

(->> (re-seq #"-?\d+" (slurp (clojure.java.io/resource "15.txt")))
     (map #(Integer/parseInt %) )
     (partition 4)
     (map #(partition 2 %))
     (map (partial ->x-interval y))
     (remove nil?)
     (sort-by first)
     (reduce (fn [acc [x1 x2 :as interval]]
               (let [[x1_ x2_ :as last-interval] (last acc)]
                 (cond (nil? last-interval)
                       (conj acc interval)
                       (< x2_ x1) (conj acc interval)
                       :else (conj (drop-last acc) [x1_ (max x2 x2_)])))) [])
     (reduce #(+ %1 (apply - (reverse %2))) 0))
