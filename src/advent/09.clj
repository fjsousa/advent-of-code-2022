(ns advent.09)


;;part 1 is readable byt part 2
;; NEEDS HEAVY REFACTORING

(def parsed-input
  (-> (slurp (clojure.java.io/resource "09.txt"))
      (clojure.string/split #"\n| ")
      (as-> x (partition 2 x))
      (as-> x (map (fn [[l n]]
                     (repeat (Integer/parseInt n) l)) x))
      flatten))


;;BOTTOM left position
(def initial-position [11 5])

(defn array-sum [one two]
  (mapv + one two))

(defn array-diff [one two]
  (mapv - one two))

(defn array-mult [one n]
  (mapv #(* -1 %) one))

;;(array-mult [1 1] -1) [-1 -1]

(defn letter->numeric-vector [letter]
  (case letter
    "R" [ 1 0]
    "L" [-1 0]
    "U" [0  1]
    "D" [0 -1]))

;; ...     ...
;; .H. and .H.
;; ...     ...

;; [0 0]  [* *] = [0 0] Tail doesn't move

;; ...     ...
;; .TH and ..TH
;; ...     ...

;; [-1 0] [1 0] = [1 0] Tail moves

;; ...     ..H
;; .TH and .T.
;; ...     ...

;; [-1 0] [0 1] = [0 0] Tail doens't move

;; ...     ...
;; .TH and .H.
;; ...     ...

;; [-1 0] [-1 0] = [0 0] Tail doesn't move

;; ...     ...
;; .H. and HT.
;; ...     ...

;; [0 0] [-1 0] = [-1 0] Tail moves

;; Generalizing:
;; dhtx (delta between head and tail in a coordinate is zero)
;; dt tail delta
;; dhx head delta

;; - if dhtx = 0  and dhx = *  -> dtx = 0

;; - if dhtx = -1 and dhx = 1  -> dtx = 1   <----- moves here
;; - if dhtx = -1 and dhx = 0  -> dtx = 0
;; - if dhtx = -1 and dhx = -1 -> dtx = 0

;; - if dhtx = 1 and dhx = 1  -> dtx = 0
;; - if dhtx = 1 and dhx = 0  -> dtx = 0
;; - if dhtx = 1 and dhx = -1 -> dtx = -1    <----- moves here

;; Tail only moves in one coord if the the deltas in that coord multiplied by themselves are -1

;;Diagonal movement

;; ...     ...
;; .H. and .TH
;; T..     ...

;; [-1 -1]  [1 0] = [1 1] Tail moves diagonally

;; T..     ...
;; .H. and .TH
;; ...     ...

;; [-1 1]  [1 0] = [1 -1] Tail moves diagonally

;; ..T     ...
;; .H. and HT.
;; ...     ...

;; [1 1]  [-1 0] = [-1 -1] Tail moves diagonally


;; ..T     ...
;; .H. and .T.
;; ...     .H.

;; [1 1]  [0 -1] = [-1 -1] Tail moves diagonally


;;generalizing:

;;if tail is diagonal and head is "moving away from tail, invert dht vector"

(defn tail-diagonal?
  "tail is diagonal if both dht coord are not zero"
  [[x y]]
  (not (zero? (* x y))))


(defn moving-away-diagonally?
  "tail moves away diagonnaly if dht + dh nullifies at least one coord"
  [dht dh]
  (= 1 (count (filter zero? (array-sum dht dh)))))


(defn delta-tail-coord
  "new delta for generic coord"
  [dht dh]
  (if (= -1 (* dht dh))
    dh
    0))

(defn delta-tail [dht-vec dh-vector]
  (if (and (tail-diagonal? dht-vec) (moving-away-diagonally? dht-vec dh-vector))
    ;;diagonal case
    (array-mult dht-vec -1)
    ;;up-down, left-right case
    (mapv delta-tail-coord dht-vec dh-vector)))

;;(delta-tail [0 0] [-1 0]) ;;[0 0]
;;(delta-tail [0 1] [-1 0])

(defn update-tail [delta-head-tail delta-head last-tail]
  (array-sum (delta-tail delta-head-tail delta-head)
             last-tail))


(defn update-fn [positions letter]
  (let [delta-head (letter->numeric-vector letter)
        [last-head last-tail] (-> positions last)
        delta-head-tail (array-diff last-tail last-head)
        new-tail (update-tail delta-head-tail delta-head last-tail)
        new-head (array-sum delta-head last-head )]
    (conj positions [new-head new-tail])))

(->> (reduce update-fn [[initial-position initial-position]] parsed-input)
     (map last)
     distinct
     count )

;;PART 2

;;Part two is a general case of part
;; instead of now being one head and tail, there's multiple of these pairs.
;;but each one of the pairs follows the same logic

;; 9 pairs:
;; H 1
;; 1 2
;; 2 3
;; 3 4
;; 4 5
;; 5 6
;; 6 7
;; 7 8
;; 8 9

;; ......
;; ......
;; ......
;; ....H.
;; 4321..

;; ......
;; ......
;; ......
;; ....H.
;; ..TH..

;; ......
;; ......
;; ....H.
;; ....1.
;; 432...

;; now we have H going in diagonal moves...

;; ...     ...   ...
;; ... and ..H = .TH
;; TH.     ...   ...

;; [-1 0] [1 1] = [1 1]

;; ...     ...   ...
;; ... and ..H = ..H
;; .H.     ...   ..T
;; .T.

;; [0 -1] [1 1] = [1 1]

(defn head-makes-diagonal-move [[x y]]
  (not (zero? (* x y))))






;; ... .H.    .H.
;; ..H ...    .T.
;; .T. .T.    ...


(defn delta-tail-2 [dht-vec dh-vector new-head old-tail]
  (let [head-makes-diagonal-move? (head-makes-diagonal-move dh-vector) ]

    (cond (and head-makes-diagonal-move?
               (= dht-vec [0 0])) [0 0]

          (and head-makes-diagonal-move?
               (= [2 0] (array-diff new-head old-tail))) [1 0]

          (and head-makes-diagonal-move?
               (= [0 2] (array-diff new-head old-tail))) [0 1]

          (and head-makes-diagonal-move?
               (= [-2 0] (array-diff new-head old-tail))) [-1 0]

          (and head-makes-diagonal-move?
               (= [0 -2] (array-diff new-head old-tail))) [0 -1]

          (and head-makes-diagonal-move?
               (= 1 (count (filter zero? (array-diff new-head old-tail))))) [0 0]

          (and head-makes-diagonal-move?
               (= (array-diff new-head old-tail) [0 0])) [0 0]

          head-makes-diagonal-move? dh-vector


          ;;diagonal case
          (and (tail-diagonal? dht-vec)
               (moving-away-diagonally? dht-vec dh-vector))
          (array-mult dht-vec -1)
          ;;up-down, left-right case
          :else (mapv delta-tail-coord dht-vec dh-vector))))


(defn update-tail-2 [delta-head-tail delta-head last-tail new-head old-tail]
  (array-sum (delta-tail-2 delta-head-tail delta-head new-head old-tail)
             last-tail))


(defn update-fn-2 [positions letter]
  (let [current-positions (last positions)
        delta-head (letter->numeric-vector letter)
        last-head (first current-positions)
        new-head (array-sum delta-head last-head)]
    (conj positions
          (loop [new-positions [new-head]
                 [last-knot-head & rest-knots] current-positions
                 delta-knot-head delta-head]
            (if (not-empty rest-knots)
              (let [last-knot-tail (first rest-knots)
                    delta-head-tail (array-diff last-knot-tail last-knot-head)
                    new-knot-tail (update-tail-2 delta-head-tail delta-knot-head last-knot-tail (last new-positions) last-knot-tail)
                    delta-knot-tail (array-diff new-knot-tail last-knot-tail)]
                (recur (conj new-positions  new-knot-tail)
                       rest-knots
                       delta-knot-tail ;;delta-tail is the new delta-head of the next pair
                       ))
              new-positions
              )
            ))))

(->> (reduce update-fn-2 [(vec (repeat 10 initial-position))] parsed-input)
     (map last)
     distinct
     count );;2471

#_(def base-output
  [[".""."".""."".""."]
   [".""."".""."".""."]
   [".""."".""."".""."]
   [".""."".""."".""."]
   [".""."".""."".""."]
   ])

(comment (def base-output
   (vec (repeat 21 (vec (repeat 26 ".")))))

 (def solution (reduce update-fn-2 [(vec (repeat 10 initial-position))] parsed-input))

 (reduce update-fn-2 [(vec (repeat 10 initial-position))] parsed-input))

#_(->> solution
     (map (fn [step-output]
            (reverse (->>
                      step-output
                      (map-indexed (fn [index v] [index v]))
                      reverse
                      (reduce (fn [acc [index v]]
                                (try
                                  (update-in acc (reverse v) (fn [_ new] (format "%s" new)) index)
                                  (catch Exception e
                                    acc))) base-output)))))
     (map (fn [solution-matrix]
            (doseq [line solution-matrix]
              (println line))
            (println "-----"))))


#_[[3 4] [4 3] [4 2] [3 2] [2 2] [1 1] [0 0] [0 0] [0 0] [0 0]]
#_[[2 4] [3 4] [3 3] [2 3] [1 3] [0 2] [-1 1] [0 0] [0 0] [0 0]]

#_(["." "." "." "0" "." "."]
      ["." "." "." "." "1" "."]
      ["." "." "4" "3" "2" "."]
      ["." "5" "." "." "." "."]
      ["6" "." "." "." "." "."])

#_(["." "." "0" "1" "." "."]
      ["." "." "." "2" "." "."]
      ["." "." "4" "3" "#" "."]
      ["." "5" "." "." "." "."]
      ["6" "." "." "." "." "."])

  ;;...     .H.      .H.
  ;;.TH     ...      .T.
  ;;...     ...      ...

  ;;[-1 0] [-1 1]    [0 0]


  ;;...     .H.      .H.
  ;;HT.     ...      .T.
  ;;...     ...      ...

  ;;[1 0] [1 1]     [0 0]

  ;;...     ...      ...
  ;;HT.     ...      .T.
  ;;...     .H.      .H.

  ;;[1 0] [1 -1]     [0 0]
