#lang r5rs

(load "/usr/src/minlog/init.scm")

(add-pvar-name "A" (make-arity))
(set-goal (pf "A -> A"))
