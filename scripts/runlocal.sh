#!/bin/bash
for i in {0..49}; do lein run -m propeller.problems.simple-regression :variaton "{:umad 1.0}" :tournament-size 5 :ordering-m-rate 0.1 :population-size 75 :max-executions 75000 :fixed-orderings true > "runs/coevolve/run$i.txt" | echo $i; done
