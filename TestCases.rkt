#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

   

      (check-equal?
 (eval-exp (apply (lam "a" "b" (ifneq (var "b") (num 1) 
                         (with "b" (plus (var "b") (num -1)) (apply (var "a") (var "b")  ))
                         (num 3)
                         )) (num 2))
            ) (num 3) "test113")


     
      

   


   ))


(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade.txt" #:exists 'replace))
(pretty-write (- 100 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)