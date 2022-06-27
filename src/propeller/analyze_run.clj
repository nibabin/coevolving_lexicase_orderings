(ns propeller.analyze_run)

(def num_runs 50)

(let [runs (map #(slurp (clojure.string/join "" ["~/runs/with-coevolve/run" % ".txt"])) (range num_runs))]
  (prn runs))