(ns propeller.weighted_lexicase
  (:require [propeller.variation :as variation]))

(defn mutate-ordering
  [ordering]
  (reduce (fn [i1 i2]
            (if (< (rand) 0.25)
              (let [x (last i1)]
                (concat (butlast i1) (list i2 x)))
              (concat i1 (list i2))))
          (list (first ordering)) (rest ordering)))

(defn select-ordering
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (repeatedly tournament-size #(rand-nth pop))]
    (apply min-key :bias tournament-set)))

(defn evolve_orderings
  [pop argmap]
  (repeatedly (count pop)
              #(mutate-ordering (:ordering (select-ordering pop argmap)))))


(defn weighted-lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [case_order pop]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases case_order
         evaluated_cases 1]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      {:parent (rand-nth survivors)
        :bias evaluated_cases}
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (inc evaluated_cases))))))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [case_order pop argmap]
  ;(clojure.pprint/pprint "called")
  (let [plushy-with-bias (weighted-lexicase-selection case_order pop)]
    {:ordering case_order
     :plushy (-> (:plushy (:parent plushy-with-bias))
                 (variation/uniform-addition (:instructions argmap) (:umad-rate argmap))
                 (variation/uniform-deletion (:umad-rate argmap)))
     :bias (:bias plushy-with-bias)}))
