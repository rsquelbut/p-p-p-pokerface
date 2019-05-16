(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))


(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (>= (apply max (rank-frequencies hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (rank-frequencies hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (rank-frequencies hand)) 4))

(defn flush? [hand]
  (= (apply max (suit-frequencies hand)) 5))

(defn full-house? [hand]
  (= [2 3] (sort (rank-frequencies hand))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sort (rank-frequencies hand)))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [r (map rank hand)]
    (let [[-min -max] [(apply min r) (apply max r)]]
      (if (= 14 -max)
        (or
          (= (sort r) (range 10 15))
          (= (sort (replace {14 1} r)) (range 1 6)))
        (= (sort r) (range -min (+ 5 -min))))

      )
    )
  )

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[straight-flush? 8]
                   [four-of-a-kind? 7]
                   [full-house? 6]
                   [flush? 5]
                   [straight? 4]
                   [three-of-a-kind? 3]
                   [two-pairs? 2]
                   [pair? 1]
                   [high-card? 0]}]
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))
    ))
