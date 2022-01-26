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

   ;1

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (plus (var "a") (var "u"))))) 
				(set "u") 
				"compute-free-vars test #1")
					
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (mult (var "t") (var "u")))))
				(set "t" "u") 
				"compute-free-vars test #2")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (neg (mult (var "t") (var "u"))))))
				(set "t" "u") 
				"compute-free-vars test #3")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (var "t"))))
				(set "t") 
				"compute-free-vars test #4")				
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (num 2))))
				(set) 
				"compute-free-vars test #6")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "bt" (bool #t))))
				(set) 
				"compute-free-vars test #6-1")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (iseq (plus (var "x") (var "y")) (neg (var "y"))))))
				(set "y" "x") 
				"compute-free-vars test #7")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ifnzero (div (var "x") (var "y")) (neg (var "p")) (minus (var "s")(var "t"))))))
				(set "y" "p" "s" "t") 
				"compute-free-vars test #8")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (neg (andalso (var "bt")(var "bt"))) (orelse (var "bf")(var "bf"))))))
				(set "y" "p" "bt" "bf") 
				"compute-free-vars test #9")
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "y" "x" (lam "z" "p" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z")))))))
				(set "s") 
				"compute-free-vars test #11")
				
			
				
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
