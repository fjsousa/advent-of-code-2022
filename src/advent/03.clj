(ns advent.03)

(def score-fn
  {\a 1
   \b 2
   \c 3
   \d 4
   \e 5
   \f 6
   \g 7
   \h 8
   \i 9
   \j 10
   \k 11
   \l 12
   \m 13
   \n 14
   \o 15
   \p 16
   \q 17
   \r 18
   \s 19
   \t 20
   \u 21
   \v 22
   \w 23
   \x 24
   \y 25
   \z 26
   \A 27
   \B 28
   \C 29
   \D 30
   \E 31
   \F 32
   \G 33
   \H 34
   \I 35
   \J 36
   \K 37
   \L 38
   \M 39
   \N 40
   \O 41
   \P 42
   \Q 43
   \R 44
   \S 45
   \T 46
   \U 47
   \V 48
   \W 49
   \X 50
   \Y 51
   \Z 52
   })

(def s "vJrwpWtwJgWrhcsFMMfFFhFp")

(defn item-type->score [c]
  (score-fn c))

(item-type->score \p)


(defn initialize-array []
  (vec (take 52 (repeat false))))


;; s is split in two parts
;; for instance, part-1 is (\v \J \r \w \p \W \t \w \J \g \W \r)
;; this sequence is mapped into an array when each indx corresponds to the score:
#_[false
   false
   false
   false
   false
   false
   true
   false
   false
   false
   false
   false
   false
   false
   false
   true
   false
   true
   false
   true
   false
   true
   false
   true
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   true
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   false
   true
   false
   false]

;; now we just go through everything. Everything should be false, except in places where there's duplicates

(defn array-of-EVERYTHING [part]
  (reduce (fn  [acc i]
            (let [score (score-fn i)]
              (update-in acc [(dec score)] #(or % true)))) (initialize-array) part))

(defn list->score-of-misplaced-item
  [s]
  (let [item-type-list (seq s)
        part-length (/ (count item-type-list) 2)
        [part-1 part-2] (partition part-length item-type-list)
        accumulator-1 (array-of-EVERYTHING part-1)
        accumulator-2 (array-of-EVERYTHING part-2)]
    (loop [[item-part-1 & rest-part-1] accumulator-1
           [item-part-2 & rest-part-2] accumulator-2
           idx 1]
      (if (and item-part-1 item-part-2)
        idx
        (recur rest-part-1 rest-part-2 (inc idx))))))

(= 16 (list->score-of-misplaced-item "vJrwpWtwJgWrhcsFMMfFFhFp"))
(= 20 (list->score-of-misplaced-item "ttgJtRGJQctTZtZT"))

(->> (clojure.string/split (slurp (clojure.java.io/resource "03.txt")) #"\n")
     (map list->score-of-misplaced-item)
     (apply +))


;;PART 2

(defn part-2-mapper

 [[list-1 list-2 list-3]]
  (let [#_#_list-1 "vJrwpWtwJgWrhcsFMMfFFhFp"
        #_#_list-2 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        #_#_list-3 "PmmdzqPrVvPwwTWBwg"
        list-1 (seq list-1)
        list-2 (seq list-2)
        list-3 (seq list-3)
        accumulator-1 (array-of-EVERYTHING list-1)
        accumulator-2 (array-of-EVERYTHING list-2)
        accumulator-3 (array-of-EVERYTHING list-3)
        ]
    (loop [[item-list-1 & rest-list-1] accumulator-1
           [item-list-2 & rest-list-2] accumulator-2
           [item-list-3 & rest-list-3] accumulator-3
           idx 1]
      (if (and item-list-1 item-list-2 item-list-3)
        idx
        (recur rest-list-1 rest-list-2 rest-list-3 (inc idx))))))

(->> (clojure.string/split (slurp (clojure.java.io/resource "03.txt")) #"\n")
     (partition 3)
     (map part-2-mapper)
     (apply +))
