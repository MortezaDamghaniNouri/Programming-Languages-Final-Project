#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

   ; arithmetic functions test
   (check-equal? (eval-exp (value "Donald Knuth" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 1) "test132")
(check-equal? (eval-exp (value "John McCarthy" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 2) "test133")
(check-equal? (eval-exp (value "Barbara Liskov" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 3) "test134")
(check-equal? (eval-exp (value "Lotfi A. Zadeh" (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit))))) (munit) "test135")
(check-equal? (eval-exp (value "Maryam Mirzakhani" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (munit) "test136")
(check-equal? (eval-exp (value "Bertrand Russell" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (record (key "Zohar Manna" (num 4)) (munit))))))) (munit) "test137")
   
   


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