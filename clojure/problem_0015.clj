;; Starting in the top left corner of a 2×2 grid,
;; and only being able to move to the right and down,
;; there are exactly 6 routes to the bottom right corner.

;; How many such routes are there through a 20×20 grid?

(defn factorial [n]
  {:pre [(integer? n) (>= n 0)]}
  (reduce *' (range 1N (inc n))))


(/ (factorial 40) (factorial 20) (factorial 20))

;;-> 137846528820
