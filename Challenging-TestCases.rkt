#lang racket

;; syntax: https://docs.racket-lang.org/rackunit/api.html

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.
(require rackunit)

(define envList (list (cons "x" (num 2))
					  (cons "y" (num 3))
					  (cons "z" (num 4))
					  (cons "p" (num 5))
					  (cons "q" (num 6))
					  (cons "t" (num 7))
					  (cons "a" (num 8))
					  (cons "b" (num 9))
					  (cons "u" (num 10))
					  (cons "s" (num 11))
					  (cons "bt" (bool #t))
					  (cons "bf" (bool #f))
					  ))

(define tests
  (test-suite
   "Project Tests - Challenging Part"


				
					



	
				

  (check-equal? 	(closure-env (eval-exp-c (with* (list (cons "x" (num 1)) (cons "y" (num 2))) 
				(lam "f" "y" (plus (var "x") (var "y"))))))
				(list (cons "x" (num 1)))
				"eval-exp-c test #1")
					
		
				
   ))

(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade-Challenging.txt" #:exists 'replace))
(pretty-write (- 50 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)
