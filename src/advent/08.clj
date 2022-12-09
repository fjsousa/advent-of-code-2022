(ns advent.08)


;;(x,y) = (0,0) is upper left corner
;;cols is number of collumns
;;rows is total number of rows

(def parsed-data


  (->> (clojure.string/split-lines (slurp (clojure.java.io/resource "08.txt")))
      (map-indexed (fn
                     [y line]
                     (vec (map-indexed
                           (fn [x h]
                             [x y (Integer/parseInt (str h))])
                           (-> line seq)))))
      vec))

(def cols (-> parsed-data first count))

(def rows (count parsed-data))

(def input-as-rows parsed-data)

#_[[[0 0 3] [1 0 0] [2 0 3] [3 0 7] [4 0 3]]
   [[0 1 2] [1 1 5] [2 1 5] [3 1 1] [4 1 2]]
   [[0 2 6] [1 2 5] [2 2 3] [3 2 3] [4 2 2]]
   [[0 3 3] [1 3 3] [2 3 5] [3 3 4] [4 3 9]]
   [[0 4 3] [1 4 5] [2 4 3] [3 4 9] [4 4 0]]]

(def empty-grid-transposed
  (vec (repeat rows (vec (repeat cols [])))))

(def input-as-columns

  (reduce (fn [output row]
            (reduce (fn [output [x y h]]
                      (update-in output [x y] (fn [old new] (vec (into old new))) [x y h]))
                    output row))
          empty-grid-transposed parsed-data))

#_[[[0 0 3] [0 1 2] [0 2 6] [0 3 3] [0 4 3]]
   [[1 0 0] [1 1 5] [1 2 5] [1 3 3] [1 4 5]]
   [[2 0 3] [2 1 5] [2 2 3] [2 3 5] [2 4 3]]
   [[3 0 7] [3 1 1] [3 2 3] [3 3 4] [3 4 9]]
   [[4 0 3] [4 1 2] [4 2 2] [4 3 9] [4 4 0]]]


;;left walk

(defn slope-fn [one two]
  (- (last one)
     (last two)))

(defn reduce-in-one-direction
  [output line]
  (let [max-line (->> line (map last) (apply max))
        first-entry-is-max? (-> line first last (= max-line))]
    ;;loop from second until max or until second to last
    (if first-entry-is-max?
      output
      (loop [new-output output
             [current-entry & rest-entries] (rest line)
             high-point (-> line first)
             ]
        (let [slope (slope-fn current-entry high-point)
              new-output (if (> slope 0)
                           (conj new-output current-entry)
                           new-output)
              high-point (if (> (-> current-entry last)
                                (-> high-point last))
                           current-entry
                           high-point)]
          (cond
            (= 1 (count rest-entries)) new-output
            (= max-line (last current-entry)) new-output
            :else (recur new-output rest-entries high-point)))))))

#_[[[0 0 3] [1 0 0] [2 0 3] [3 0 7] [4 0 3]]
   [[0 1 2] [1 1 5] [2 1 5] [3 1 1] [4 1 2]]
   [[0 2 6] [1 2 5] [2 2 3] [3 2 3] [4 2 2]]
   [[0 3 3] [1 3 3] [2 3 5] [3 3 4] [4 3 9]]
   [[0 4 3] [1 4 5] [2 4 3] [3 4 9] [4 4 0]]]

(defn reduce-matrix [input]
  (reduce
   (fn [output line]
     (-> output
         (reduce-in-one-direction line)
         (reduce-in-one-direction (reverse line)))) [] (rest (drop-last input))))


 ;;PART 1
(-> []
    (into (first input-as-rows))
    (into (last input-as-rows))
    (into (first  input-as-columns))
    (into (last  input-as-columns))
    (into (reduce-matrix input-as-rows))
    (into (reduce-matrix input-as-columns))
    distinct
    count)

;; PART 2

(defn score-in-one-direction [entries]
  (let [my-height (-> entries first last)]
    (loop [[entry & rest-entries] (rest entries)
           count 1]
      (let [slope (slope-fn entry [my-height])]
        (cond (empty? rest-entries) count
              (= 0 slope) count
              (> slope 0) count
              :else (recur rest-entries (inc count)))))))

(defn score-in-one-axis [idx line]
  (let [[first-half second-half] (split-at idx line)
        score-2 (if (= 1 (count second-half))
                  1
                  (score-in-one-direction second-half))
        score-1 (if (not-empty  first-half)
                  (score-in-one-direction (reverse (conj (vec first-half) (first second-half))))
                  1)]
    (* score-2 score-1)))

(apply clojure.core/max
       (for [x (range 0 cols)
             y (range 0 rows)]

         (let [row-data (nth input-as-rows y)
               col-data (nth input-as-columns x)
               new-score (* (score-in-one-axis x row-data)
                            (score-in-one-axis y col-data))]
           new-score)))
