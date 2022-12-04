(ns advent.01
  (:require [clojure.string :as str]))

(def raw-data (slurp (clojure.java.io/resource "01.txt")))

(def cal-blocks (str/split raw-data #"\n\n"))

(->> cal-blocks
     (map (fn [block]
            (apply + (map #(new Integer %) (str/split block #"\n"))) ))
     (apply max))

(->> cal-blocks
     (map (fn [block]
            (apply + (map #(new Integer %) (str/split block #"\n")))))
     sort
     reverse
     (take 3)
     (apply +))
