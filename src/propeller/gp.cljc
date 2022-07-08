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

(defn gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size solution-error-threshold mapper max-executions]
    :or   {solution-error-threshold 0.0
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper #?(:clj pmap :cljs map)}
    :as   argmap}]
  ;;
  ;(prn {:starting-args (update (update argmap :error-function str) :instructions str)})
  ;(println)
  ;;
  (loop [generation 0
         population (pmap
                      (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                      (range population-size))
         orderings_population (vec (pmap
                                (fn [_] {:ordering (shuffle (range (count (:training-data argmap))))
                                         :bias 0})
                                (range population-size)))
         program_executions []
         average-bias []
         orderings-diversity []]
    ;(clojure.pprint/pprint  (pr-str orderings_population))
    ;(clojure.pprint/pprint  (str orderings_population))
    ;(clojure.pprint/pprint  (type orderings_population))
    ;(clojure.pprint/pprint  "hello")(flush)
    ;(println)
      ;(report evaluated-pop generation argmap)
   ; (println program_executions)(flush)
   ; (println (apply + program_executions))
    (cond
        ;; Success on training cases is verified on testing cases
        ;(<= (:total-error best-individual) solution-error-threshold)
        ;(do
        ;    (prn {:success-generation generation
        ;          :program-executions program_executions
        ;          :total-test-error (:total-error (error-function argmap (:testing-data argmap) best-individual))}))
            ;(prn {:total-test-error
            ;      (:total-error (error-function argmap (:testing-data argmap) best-individual))})
            ;(when (:simplification? argmap)
            ;  (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) error-function argmap)]
            ;    (prn {:total-test-error-simplified (:total-error (error-function argmap (:testing-data argmap) (hash-map :plushy simplified-plushy)))}))))
        ;;
        (>= (apply + program_executions) max-executions)
          (let [evaluated-pop (sort-by :total-error
                                       (pmap
                                         (partial error-function argmap (:training-data argmap))
                                         population))
                best-individual (first evaluated-pop)]
            (if (<= (:total-error best-individual) solution-error-threshold)
              (prn {:success-generation generation
                        :program-executions program_executions
                        :total-test-error (:total-error (error-function argmap (:testing-data argmap) best-individual))
                        :average-bias average-bias
                        :orderings-diversity orderings-diversity})
              (prn {:failed-generation generation
                    :program-executions program_executions
                    :total-test-error (:total-error (error-function argmap (:testing-data argmap) best-individual))
                    :average-bias average-bias
                    :orderings-diversity orderings-diversity})))
        ;;
        :else
               (let [dynamic-errors (atom {})
                     new-pop-with-bias (doall (pmap #(propeller.weighted_lexicase/new-individual
                                                                      (if (:fixed-orderings argmap)
                                                                            (:ordering %)
                                                                            (shuffle (range (count (:training-data argmap)))))
                                                                          population
                                                                          dynamic-errors
                                                                          argmap)
                                                                      orderings_population))
                     executions (count @dynamic-errors)]
                 ;(println generation (count @dynamic-errors))
                 ;(println (count new-pop-with-bias))
                 (reset! dynamic-errors nil)
                 (recur (inc generation)
                        (map (fn [ind] {:plushy (:plushy ind)}) new-pop-with-bias)
                          (if (:fixed-orderings argmap)
                            (weighted_lexicase/evolve_orderings new-pop-with-bias argmap)
                            orderings_population)
                        (conj program_executions executions)
                        (conj average-bias (float (/ (apply + (map :bias new-pop-with-bias)) population-size)))
                        (conj orderings-diversity (float (/ (count (distinct orderings_population)) population-size))))))))
