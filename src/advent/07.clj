(ns advent.07)

(def output-lines (->> (clojure.string/split-lines (slurp (clojure.java.io/resource "07.txt")))
                       (map (fn [s]
                              (clojure.string/split s #" ")))))

(defn cmd? [line]
  (= "$" (first line)))

(defn cd-dot-dot? [line]
  (= ["$" "cd" ".."] line))

(defn cd-into-dir? [line]
  (and (not (cd-dot-dot? line))
       (= ["$" "cd"] (take 2 line))))

(defn dir-info? [line]
  (= "dir" (first line)))

(defn ls? [line]
  (= ["$" "ls"] (take 2 line)))

(defn file-info? [line]
  (and (not (cmd? line)) (not (dir-info? line))))

(defn add-files-to-all-dirs-before
  [output path file-size]
  (loop [curr-path path
         final-output output]
    (if (empty? curr-path)
      final-output
      (recur (vec (drop-last curr-path)) (conj final-output [curr-path file-size])))))

(def disk-tree
  (loop [[output-line & rest-output-lines] output-lines
         output []
         path []
         processing-ls-output false]
    (if output-line
      (let [processing-ls-output (not (cmd? output-line))]
        (cond (cd-into-dir? output-line) (recur rest-output-lines output (conj path (last output-line)) processing-ls-output)
              (cd-dot-dot? output-line) (recur rest-output-lines output (vec (drop-last path)) processing-ls-output)
              (ls? output-line) (recur rest-output-lines output path true)
              (dir-info? output-line) (recur rest-output-lines output path processing-ls-output)
              (file-info? output-line) (recur rest-output-lines (add-files-to-all-dirs-before output path (first output-line)) path processing-ls-output)))
      output)))

;; disk-tree is a structure like this:
;; [[["/"] "14848514"]
;;  [["/"] "8504156"]
;;  [["/" "a"] "29116"]
;;  [["/"] "29116"]
;;  [["/" "a"] "2557"]
;;  [["/"] "2557"]
;;  [["/" "a"] "62596"]
;;  [["/"] "62596"]
;;  [["/" "a" "e"] "584"]
;;  [["/" "a"] "584"]
;;  [["/"] "584"]
;;  [["/" "d"] "4060174"]
;;  [["/"] "4060174"]
;;  [["/" "d"] "8033020"]
;;  [["/"] "8033020"]
;;  [["/" "d"] "5626152"]
;;  [["/"] "5626152"]
;;  [["/" "d"] "7214296"]
;;  [["/"] "7214296"]]


;;PART 1

(->> disk-tree
     (group-by first)
     (map (fn [[dir group]]
            (->> group (map last) (map #(Integer. %)) (reduce +))))
     (filter #(< % 100000))
     (reduce +);; 95437 (example data)
     )

;; part 2

(def grouped-disk-tree
  (->> disk-tree
       (group-by first)))

(def used-disk-space
  (->> (get grouped-disk-tree ["/"])
       (map last)
       (map #(Integer. %))
       (reduce +)))


(def free-space (- 70000000 used-disk-space))

(def space-to-free (- 30000000 free-space))

(->> grouped-disk-tree
     (map (fn [[dir group]]
            (->> group (map last) (map #(Integer. %)) (reduce +))))
     (filter #(> % space-to-free))
     sort
     first) ;;24933642 example data
