(ns advent.05
  (:require [clojure.string :as str]))

;;PARSING

(def raw-input (-> (slurp (clojure.java.io/resource "05.txt"))
     (clojure.string/split #"\n" )))

(def aux
  (loop [[i & rest-input] raw-input
              result []]
         (if (not-empty i)
           (recur rest-input (conj result i))
           [result rest-input])))

(def layout (first aux))

(def instructions (last aux))

(def n-columns (->> layout last (re-seq #"\d+") (map #(new Integer %)) last))

(def stack-size (-> layout count dec))

(defn parse-letter [value-seq]
  (cond
    (= value-seq '(\space \space \space)) nil
    :else (second value-seq)))

(def parsed-input

  (reduce (fn [acc layout-line]
            (let [parsed-letters (map-indexed (fn [idx entry]
                                                [idx (parse-letter entry)])
                                              (partition 3 4 layout-line))]
              (reduce (fn [acc [idx letter]]
                        (if letter
                          (update-in acc [idx] #(conj % letter))
                          acc)) acc parsed-letters)))
          (vec (repeat n-columns []))
          (rest (reverse layout))))

(def parsed-instructions
  (map (fn [l] (->> l
                    (re-seq #"\d+")
                    (map #(new Integer %)))) instructions))

;;PART 1 and 2

(identity parsed-input)
;;[[\Z \N] [\M \C \D] [\P]]

(identity parsed-instructions)
;;((1 2 1) (3 1 3) (2 2 1) (1 1 2))

(defn order-crates [crane-type]
  (loop [[instruction & rest-instructions] parsed-instructions
         puzzle parsed-input]
    (if instruction
      (let [[qty from to] instruction
            qty qty
            from (dec from)
            to (dec to)
            items-to-move (vec (take-last qty (puzzle from)))
            new-puzzle (-> puzzle
                           (update-in [from] #(vec (drop-last qty %)))
                           (update-in [to] #(into % (if (= 9000 crane-type)
                                                      (reverse items-to-move)
                                                      items-to-move))))]
        (recur rest-instructions new-puzzle))
      (apply str (map last puzzle)))))

(order-crates 9000) ;;SBPQRSCDF
(order-crates 9001) ;;RGLVRCQSB
