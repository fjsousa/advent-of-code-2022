(ns advent.10)

(def parsed-input
  (reduce (fn [acc operation]
            (cond (= "noop" operation) (conj acc [:noop])
                  :else (conj acc [:addx (Integer/parseInt (last (clojure.string/split operation #" ")))])))
          [] (clojure.string/split-lines (slurp (clojure.java.io/resource "10.txt")))))

(def x-at-the-clock
  "This gets the value of X at each tic"
  (loop [x 1
         output [x]
         [operation & rest-operations] parsed-input]
    (cond
      (not operation) output
      (= :noop (first operation)) (recur x (conj output x) rest-operations)
      (= :addx (first operation)) (let [new-x (+ x (last operation))]
                                    (recur new-x (into output [x new-x]) rest-operations)))))

;; noop
;; addx 3
;; addx -5

;;returns

;;[1 1 1 4 4 -1]

;; 1 - noop goes from idx = 0 to idx = 1 -> cycle 1
;; 2 - addx 3 goes from idx = 1 to idx = 3 -> cycle 2 and 3

 ;; 1 [1
 ;; 2 1
 ;; 3 1
 ;; 4 4
 ;; 5 4
 ;; 6 4
 ;; 7 -1]


;; the value of X during the cycle n is the value at n -1 -> the value of X in the beginning of the cycle
(reduce + (map (fn [n]
                 (* n (nth x-at-the-clock (dec n))))
               [20 60 100 140 180 220]));;15880


;;PART2

;; (1 1 1 1 1 4 4 24 24 24)
;; each entry the above array is the value of X at each tic

;;Take 240 value of X, one for each tick

(defn print-crt-matrix [matrix]
  (doseq [crt-line matrix]
    (println crt-line))
  (println "\n"))

(def initial-crt-matrix (vec (repeat 6 (vec (repeat 40 ".")))))

(print-crt-matrix initial-crt-matrix)

;;Empty CRT
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]
;;[. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .]



(defn idx->hvcoord
  [idx]
  (let [horizontal-coord (mod idx 40)
        vertical-coord (int (Math/floor (/ idx 40)))]
    [horizontal-coord vertical-coord]))

;;tests
(and (= (idx->hvcoord 0) [0 0])
     (= (idx->hvcoord 40) [0 1])
     (= (idx->hvcoord 1) [1 0])
     (= (idx->hvcoord 39) [39 0])
     (= (idx->hvcoord 239) [39 5]))



(->> x-at-the-clock
     (take 240)
     (map-indexed (fn [idx x]
                    (let [[h v] (idx->hvcoord idx)]
                      [h v x])))

)

(defn update-crt-matrix-with-x [ctr-matrix [h v x]]
  (update-in ctr-matrix [v h] (fn [old-pixel-value x]
                                (if (and (<= h (inc x))
                                         (>= h (dec x)))
                                  "#"
                                  ".")) x))


(->> x-at-the-clock
     (take 240)
     (map-indexed (fn [idx x]
                    (let [[h v] (idx->hvcoord idx)]
                      [h v x])))
     (reduce (fn [crt-matrix point-vector]
               (update-crt-matrix-with-x crt-matrix point-vector)
               ) initial-crt-matrix)
     print-crt-matrix

)

;;[# # # . . # . . . . . # # . . # # # # . # . . # . . # # . . # # # # . . # # . .]
;;[# . . # . # . . . . # . . # . # . . . . # . # . . # . . # . . . . # . # . . # .]
;;[# . . # . # . . . . # . . . . # # # . . # # . . . # . . # . . . # . . # . . . .]
;;[# # # . . # . . . . # . # # . # . . . . # . # . . # # # # . . # . . . # . # # .]
;;[# . . . . # . . . . # . . # . # . . . . # . # . . # . . # . # . . . . # . . # .]
;;[# . . . . # # # # . . # # # . # . . . . # . . # . # . . # . # # # # . . # # # .]
