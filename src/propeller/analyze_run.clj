(ns propeller.analyze_run)

(def num_runs 50)
(def folder "fuel_cost/no_coevolve")

(defn average [coll]
  (float (/ (reduce + coll) (count coll))))

(prn (let [runs (filter #(not (nil? %)) (map #(let [str (slurp (clojure.string/join "" ["/home/cbGST/runs/" folder "/run" % ".txt"]))]
                          (if (or (= str "") (not= "{" (clojure.core/str (first str))))
                            nil
                            (read-string str)))
                        (range num_runs)))
           successful_runs (filter #(not (= -1 (:success-generation %))) runs)]
       {:total (count runs)
        :successes (count successful_runs)
        :generalizations (count (filter #(= 0 (:total-test-error %)) successful_runs))
        :min-generation (:success-generation (apply min-key :success-generation successful_runs))
        :max-generation (:success-generation (apply max-key :success-generation successful_runs))
        :avg-generation (average (map :success-generation successful_runs))
        :avg-program-executions (average (map #(average (:program-executions %)) successful_runs))
        }
       ))