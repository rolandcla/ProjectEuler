;; A unit fraction contains 1 in the numerator.
;; The decimal representation of the unit fractions with denominators 2 to 10 are given:

;; 1/2	= 	0.5
;; 1/3	= 	0.(3)
;; 1/4	= 	0.25
;; 1/5	= 	0.2
;; 1/6	= 	0.1(6)
;; 1/7	= 	0.(142857)
;; 1/8	= 	0.125
;; 1/9	= 	0.(1)
;; 1/10	= 	0.1
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
;; It can be seen that 1/7 has a 6-digit recurring cycle.

;; Find the value of d < 1000 for which 1/d contains
;; the longest recurring cycle in its decimal fraction part.


(defn decimals
  [x]
  {:pre [(integer? x)(> x 1)]}

  (letfn [(recur-dec [n]
            (let [q (quot n x), r (rem n x)]
              (if (== 0 r)
                (if (== 0 q) [] [q])
                (lazy-seq (cons q (recur-dec (* 10 r))))
                )
              ))]
    (recur-dec 10)
    )
  )

(defn search-for-n-digits-cycle
  [decs n from]
  (let [len (+ 10 (* 3 n))
        ds (take len decs)]
    (loop [from from]
      (when (< from len)
        (let [p (partition n (drop from ds))]
          (if (and (> (count p) 1) (apply = p))
            [(take from ds) (take n (drop from ds))]
            (recur (inc from))
            )))))
  )

(defn search-for-shortest-cycle
  [ds]
  (loop [n 1]
    (when (< n 100)
      (or (search-for-n-digits-cycle ds n 0)
          (recur (inc n)))
      )
    )
    
  )

(defn solution []
  (->> (range 2 100)
       (map #(vector % (decimals %)))
       (map (fn [[ix ds]] [ix (search-for-shortest-cycle ds)]))
       ;;(map (fn [[ix ds]] [ix (take 20 ds)]))

       ))

