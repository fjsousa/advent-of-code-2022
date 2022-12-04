(ns advent.02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def raw-data (slurp (io/resource "02.txt")))

(def input (->> (str/split raw-data #"\n")
                (map #(str/split % #" "))))
(take 10 input)

(def win-book
  {"rock" "scissor"
   "paper" "rock"
   "scissor" "paper"})

(def loose-book
  {"scissor" "rock"
   "rock" "paper"
   "paper" "scissor"})

(def score-book
  {"rock" 1
   "paper" 2
   "scissor" 3})

(defn draw [threirs mine]
  (= threirs mine))

(defn iwin  [theirs mine]
  (= (get win-book mine) theirs))

(defn score-fn
  [theirs mine]
  (let [iwin? (iwin theirs mine)
        draw? (draw theirs mine)
        ilosse? (not (and iwin? draw?))
        hand-score (get score-book mine)
        win-score (if iwin? 6 0)
        draw-score (if draw? 3 0)
        result-score (+ win-score draw-score)]
    (+ hand-score result-score)))

(defn char->hand [c]
  (get {"A" "rock"
        "B" "paper"
        "C" "scissor"
        "X" "rock"
        "Y" "paper"
        "Z" "scissor"} c))

(defn score-round [[their-c mine-c]]
  (score-fn (char->hand their-c) (char->hand mine-c)))

(score-round ["C" "Z"])

(reduce + (map score-round input))

;; 2nd part

;;X you need to lose
;;Y you need to draw
;;Z you need to win


(defn mine-helper [theirs how-it-should-end]
  (let [their-hand (char->hand theirs)]

    (cond (= how-it-should-end "Y") their-hand
          (= how-it-should-end "X") (get win-book their-hand)
          (= how-it-should-end "Z") (get loose-book their-hand))))

(defn score-2nd-round [[theirs-c how-it-should-end]]
  (let [theirs-c theirs-c
        theirs (char->hand theirs-c)
        mine (mine-helper theirs-c how-it-should-end)]
    (score-fn theirs mine)))

(score-2nd-round ["C" "Z"])

(reduce + (map score-2nd-round input))
