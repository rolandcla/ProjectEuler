;; Using names.txt (right click and 'Save Link/Target As...'),
;; a 46K text file containing over five-thousand first names,
;; begin by sorting it into alphabetical order.
;; Then working out the alphabetical value for each name,
;; multiply this value by its alphabetical position in the list to obtain a name score.

;; For example, when the list is sorted into alphabetical order, COLIN,
;; which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
;; So, COLIN would obtain a score of 938 × 53 = 49714.

;; What is the total of all the name scores in the file?

(require '[clojure.string :as str])

(def all-names
  (->> (slurp "../data_files/p022_names.txt")
       (#(str/split % #","))
       (map #(subs % 1 (dec (count %))))
       )
  )

(defn alpha-score
  [name]
  (->> (map #(- (int %) 64) name)
       (reduce +)
       ))

(->> all-names
     (sort)
     (map-indexed (fn [idx name] (*  (inc idx) (alpha-score name))))
     (reduce +)
     )

;;-> 871198282

