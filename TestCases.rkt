#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

  (check-equal? (eval-exp (plus (plus (num 1) (num 2)) (minus (num 3) (num 4)))) (num 2) "test85")
 (check-equal? (eval-exp (mult (mult (num 3) (num 2)) (mult (num 3) (num 4)))) (num 72) "test86")
   (check-exn exn:fail? (lambda () (eval-exp (mult (num 3) (munit)))) "test87")

   (check-equal? (eval-exp (num -5)) (num -5) "test88")
   (check-equal? (eval-exp (munit)) (munit) "test89")
    (check-equal? (eval-exp (closure '() (lam null "x" (var "x"))))
                 (closure '() (lam null "x" (var "x"))) "test90")


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