(ns advent.04)

;; a b
;;.234.....  2-4
;;.....678.  6-8
;;     y z

(->> (clojure.string/split (slurp (clojure.java.io/resource "04.txt")) #"\n|,|-")
     (map #(new Integer %))
     (partition 4)
     (filter (fn [[a b y z ]]
               (or (and (>= a y ) (<= b z))
                   (and (>= y a ) (<= z b)))))
     count)

;;part 2

(->> (clojure.string/split (slurp (clojure.java.io/resource "04.txt")) #"\n|,|-")
     (map #(new Integer %))
     (partition 4)
     (filter (fn [[a b y z ]]
               (or
                ;;a is within [y z]
                (and (>= a y ) (<= a z))
                ;;or
                ;;b is within [y z]
                (and (>= b y) (<= b z))
                ;;or
                ;;y is within [a b]
                (and (>= y a) (<= y b))
                ;;or
                ;;z is within [a b]
                (and (>= z a) (<= z b)))))
     count)
