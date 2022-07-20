(ns propeller.weighted_lexicase
  (:require [propeller.variation :as variation]
            [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.tools.metrics :as metrics]
            [clojure.pprint]))

(defn compute-error-fizzbuzz
  ([argmap individual test-index]
   (let [program (genome/plushy->push (:plushy individual) argmap)
         input (:input1 (nth (:training-data argmap) test-index))
         correct-output (:output1 (nth (:training-data argmap) test-index))
         output (state/peek-stack
                  (interpreter/interpret-program
                    program
                    (assoc state/empty-state :input {:in1 input})
                    (:step-limit argmap))
                  :string)
         error (if (= output :no-stack-item)
                    10000
                   (metrics/levenshtein-distance correct-output output))]
     ;(println (:plushy individual))
     error)))

(defn compute-error-simplereg
  ([argmap individual test-index]
   (let [program (genome/plushy->push (:plushy individual) argmap)
         input (first (:input1 (nth (:training-data argmap) test-index)))
         correct-output (first (:output1 (nth (:training-data argmap) test-index)))
         output (state/peek-stack
                  (interpreter/interpret-program
                    program
                    (assoc state/empty-state :input {:in1 input})
                    (:step-limit argmap))
                  :integer)
         error (if (= output :no-stack-item)
                 1000000
                 (math/abs (- correct-output output)))]
     ;(println (:plushy individual))
     error)))


(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn mutate-ordering ;;;optimize?
  [ordering rate]
  (loop [order ordering
         i 0]
    (if (= i (count order))
      order
      (recur (if (< (rand) rate)
               (swap order i (int (Math/floor (rand (count order)))))
               order)
             (inc i)))))

(defn select-ordering
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (repeatedly tournament-size #(rand-nth pop))]
    ;(println (map :bias tournament-set))
    (apply min-key :bias tournament-set)))

(defn evolve_orderings
  [pop argmap]
  (map
  (fn [_] (let [ordering_with_bias (select-ordering pop argmap)]
            {:ordering (mutate-ordering (:ordering ordering_with_bias) (:ordering-m-rate argmap))
             :bias (:bias ordering_with_bias)}))
  (range (count pop))))




(defn weighted-lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [case_order pop errors population-size argmap]
  (loop [survivors (range population-size)
         cases case_order
         step 0]
    (if (or (empty? cases)
            (empty? (rest survivors)))
       {:parent (rand-nth survivors)
        :bias step}
       (let [min-err-for-case (apply min (map (fn [individual]
                                                 (if (contains? @errors (seq [individual (first cases)]))
                                                    (get @errors (seq [individual (first cases)]))
                                                    (let [error-value (compute-error-fizzbuzz argmap (nth pop individual) (first cases))]
                                                      ;(println (first cases))
                                                      (swap! errors #(assoc % (seq [individual (first cases)]) error-value))
                                                      error-value)))
                                               survivors))]
         ;(println min-err-for-case)
         (recur (filter #(= (get @errors (seq [% (first cases)])) min-err-for-case)
                        survivors)
                (rest cases)
                (inc step))))))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [case_order pop test-errors argmap]
  ;(clojure.pprint/pprint "called")
  ;(println "line 85")
  (let [plushy-with-bias (weighted-lexicase-selection case_order pop test-errors (:population-size argmap) argmap)]
    ;(println (count @test-errors))(flush)
    ;(println plushy-with-bias)
    {:ordering case_order
     :plushy (-> (:plushy (nth pop (:parent plushy-with-bias)))
                 (variation/uniform-addition (:instructions argmap) (:umad-rate argmap))
                 (variation/uniform-deletion (:umad-rate argmap)))
     :bias (:bias plushy-with-bias)}))

(defn generate-ordering
  [ranking]
  (loop [ordering []
         cases (vec ranking)]
    (if (= (count ordering) (count ranking))
        ordering
        (let [index (rand (rand (count cases)))]
          (recur (conj ordering (nth cases index))
                 (if (= index (dec (count cases)))
                   (subvec cases 0 index)
                   (vec (concat (subvec cases 0 index) (subvec cases (inc index) (count cases))))))))))
