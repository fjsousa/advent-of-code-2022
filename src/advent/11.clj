(ns advent.11
  (:require [clojure.string :as str]))

(defn str->pull-int
  "given any string, takes all ints and parses"[s]
  (mapv #(Integer/parseInt %)(re-seq #"\d+" s)))

(defn parse-operation [s]
  (let [[old op arg]
        (str/split
         (str/trim (last (str/split s #"=")))
         #" ")]
    [(keyword op)
     (if (= "old" arg)
       :old
       (Integer/parseInt arg))]))

(when (not (and (= [[:+ 3] (parse-operation "Operation: new = old + 3")])
                (= [[:* :old] (parse-operation "Operation: new = old * old")])))
  (throw (Exception. "parse operation fucked" )))

(def parsed-input
  (->> (str/split (slurp (clojure.java.io/resource "11-ex.txt")) #"\n|\n\n")
       (remove empty?)
       (partition 6)
       (mapv (fn [[monkey-idx-str
                   starting-items-str
                   operation-str
                   test-str
                   if-true-str
                   if-false-str]]
               {:monkey-n (first (str->pull-int monkey-idx-str))
                :items (map bigint (str->pull-int starting-items-str))
                :operation (parse-operation operation-str)
                :test-cond (-> test-str str->pull-int first)
                :test-+ (-> if-true-str str->pull-int first)
                :test-- (-> if-false-str str->pull-int first)
                :item-count 0}))))



(identity parsed-input)

(defn new-worry-level [operation old-item-level]
  (let [[op arg] operation
        arg (if (= arg :old)
              old-item-level
              arg)]
    (cond
      (= op :+) (+ old-item-level arg)
      (= op :*) (* old-item-level arg))))

(when (not (and (= 11 (new-worry-level [:+ 1] 10))
                (= 6241  (new-worry-level [:* :old] 79))
                (= 20 (new-worry-level [:* 2] 10))
                (= 100 (new-worry-level [:* :old] 10))))
  (throw (Exception. "new-worry-level fucked" )))

(defn cut-worry-by-a-third
  [worry-level]
  (int (Math/floor (/ worry-level 3))))

(cut-worry-by-a-third 6241)

(when (not (and (= 500 (cut-worry-by-a-third 1501))
                (= 620 (cut-worry-by-a-third 1862))))
  (throw (Exception. "can't dividde " )))

(defn is-level-divisible-by [divider level ]
  (= 0 (mod level divider)))


(defn round [vector-of-monks]
  (loop [current-monks vector-of-monks
         turn-n 0]
    (if (> (inc turn-n) (count vector-of-monks))
      current-monks
      (recur (let [{:keys [monkey-n
                           items
                           operation
                           test-cond
                           test-+
                           test--] :as turn} (nth current-monks turn-n)
                   current-monks (update-in current-monks [monkey-n :items] (fn [_ new] new) [] )
                   current-monks (update-in current-monks [monkey-n :item-count] (fn [old-count new-count] (+ old-count new-count)) (count items))
                   new-worries (->> items
                                    (mapv (partial new-worry-level operation))
                                    #_(mapv cut-worry-by-a-third))
                   apply-test-to-items (mapv (partial is-level-divisible-by test-cond) new-worries)
                   result-items (mapv (fn [test-result item-worry]
                                        [(if test-result
                                           test-+
                                           test--) item-worry]) apply-test-to-items new-worries)]

               (reduce (fn [monks [new-monkey item-worry]]
                         (update-in monks [new-monkey :items] conj item-worry)) current-monks result-items))
             (inc turn-n)))))

#_(time (->> (loop [round-n 0
             monks parsed-input]
        (if (= round-n 500)
          monks
          (recur (inc round-n) (round monks))
          ))
      (map :item-count)
      sort
y      (take-last 2)
      (apply *))) ;;110


;;PART 2 - not working

(defn is-level-divisible-by-2 [divider level ]
  [(= 0 (mod level divider)) level])

(def monks (atom parsed-input))
(def total-monks (count parsed-input))

(identity @monks)

(defn round-2 []
  (loop [turn-n 0]
    (if (> (inc turn-n) total-monks)
      :ok
      (do (let [{:keys [monkey-n
                        items
                        operation
                        test-cond
                        test-+
                        test--] :as turn} (nth @monks turn-n)
                _ (swap! monks update-in [monkey-n :items] (fn [_ new] new) [])
                _ (swap! monks update-in [monkey-n :item-count] (fn [old-count new-count] (+ old-count new-count)) (count items))
                xform (comp (map (partial new-worry-level operation))
                               (map (partial is-level-divisible-by-2 test-cond))
                               (map (fn [[test-result item-worry]]
                                        [(if test-result
                                           test-+
                                           test--) item-worry])))
                result-items (sequence xform items)]

            (doseq [[new-monkey item-worry] result-items]
              (swap! monks update-in [new-monkey :items] conj item-worry)))
          (recur (inc turn-n))))))

(time (->> (loop [round-n 0]
             (if (= round-n 100)
               @monks
               (do (round-2)
                   (recur (inc round-n)))
               ))
      (map :item-count)
      sort
      (take-last 2)
      (apply *)
      prn))

@monks
