(ns propeller.gp
  (:require [clojure.string]
            [clojure.pprint]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]
            [propeller.variation :as variation]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]
            [propeller.weighted_lexicase :as weighted_lexicase]))

(defn report
  "Reports information each generation."
  [pop generation argmap]
  (let [best (first pop)]
    (clojure.pprint/pprint {:generation            generation
                            ;:best-plushy           (:plushy best)
                            ;:best-program          (genome/plushy->push (:plushy best) argmap)
                            :best-total-error      (:total-error best)
                            ;:best-errors           (:errors best)
                            ;:best-behaviors        (:behaviors best)
                            :genotypic-diversity   (float (/ (count (distinct (map :plushy pop))) (count pop)))
                            :behavioral-diversity  (float (/ (count (distinct (map :behaviors pop))) (count pop)))
                            :average-genome-length (float (/ (reduce + (map count (map :plushy pop))) (count pop)))
                            ;:average-total-error   (float (/ (reduce + (map :total-error pop)) (count pop)))
                            })))

(defn gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size solution-error-threshold mapper]
    :or   {solution-error-threshold 0.0
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper #?(:clj pmap :cljs map)}
    :as   argmap}]
  ;;
  (prn {:starting-args (update (update argmap :error-function str) :instructions str)})
  (println)
  ;;
  (loop [generation 0
         population (pmap
                      (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                      (range population-size))
         orderings_population (pmap
                                (fn [_] {:ordering (shuffle (range (count (:training-data argmap))))
                                         :bias 0})
                                (range population-size))]

    ;(clojure.pprint/pprint  (pr-str orderings_population))
    ;(clojure.pprint/pprint  (str orderings_population))
    ;(clojure.pprint/pprint  (type orderings_population))
    ;(clojure.pprint/pprint  "hello")(flush)
    ;(println)
    (let [evaluated-pop (sort-by :total-error
                                 (pmap
                                   (partial error-function argmap (:training-data argmap))
                                   population))
          best-individual (first evaluated-pop)]
      (report evaluated-pop generation argmap)
      (cond
        ;; Success on training cases is verified on testing cases
        (<= (:total-error best-individual) solution-error-threshold)
        (do (prn {:success-generation generation})
            (prn {:total-test-error
                  (:total-error (error-function argmap (:testing-data argmap) best-individual))})
            (when (:simplification? argmap)
              (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) error-function argmap)]
                (prn {:total-test-error-simplified (:total-error (error-function argmap (:testing-data argmap) (hash-map :plushy simplified-plushy)))}))))
        ;;
        (>= generation max-generations)
        nil
        ;;
        :else
               (let [new-pop-with-bias (map #(propeller.weighted_lexicase/new-individual
                                                                          (:ordering %)
                                                                          evaluated-pop
                                                                          argmap)
                                                                      orderings_population)]
                 (clojure.pprint/pprint "Average number of tests considered:") (println)
                 (clojure.pprint/pprint (float (/ (apply + (map :bias new-pop-with-bias)) population-size)))
                 (println)
                 (recur (inc generation)
                       (map (fn [ind] {:plushy (:plushy ind)}) new-pop-with-bias)
                       (weighted_lexicase/evolve_orderings new-pop-with-bias argmap)))))))
