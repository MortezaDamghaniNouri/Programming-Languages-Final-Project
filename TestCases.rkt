#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

   ; arithmetic functions test
   (check-equal? (eval-exp (plus (num 2) (num 2))) (num 4) "test1")
   (check-equal? (eval-exp (minus (num -5) (num 2))) (num -7) "test2")
   (check-equal? (eval-exp (mult (num 5) (num 2))) (num 10) "test3")
   (check-equal? (eval-exp (div (num -5) (num -2))) (num 2) "test4")
   (check-exn exn:fail?
              (lambda () (eval-exp (div (num 5) (num 0)))
              "test5"))

   (check-equal? (eval-exp (plus (num -5) (neg (minus (num -2) (num 3)))))
                           (num 0) "test6")

    (check-equal? (eval-exp (neg (mult (num 5) (plus
                                                (div (num -2) (num 3))
                                                (minus (num 1) (num -1)
                                                       )))))
                           (num -10) "test7")
   (check-exn exn:fail?
              (lambda () (eval-exp (plus (num 5.2) (num 0.0)))
              "test8"))

   

   ; logical functions test
   (check-equal? (eval-exp (andalso (bool #t) (bool #t))) (bool #t) "test9")
   (check-equal? (eval-exp (orelse (bool #t) (bool #f))) (bool #t) "test10")
   (check-equal? (eval-exp (orelse (bool #t) (num 2))) (bool #t) "test12")
   (check-equal? (eval-exp (andalso (bool #f) (div (num 2) (num 0)))) (bool #f) "test13")
   (check-equal? (eval-exp (andalso (bool #t) (neg (bool #t)))) (bool #f) "test14")
   (check-equal? (eval-exp (orelse (iseq (num 2) (num 2)) (bool #f))) (bool #t) "test15")
   (check-equal? (eval-exp (neg (iseq (bool #t) (bool #f)))) (bool #t) "test16")
   (check-equal? (eval-exp (iseq (num 2) (bool #f))) (bool #f) "test17")
   (check-equal? (eval-exp (iseq (num 2) (num -2))) (bool #f) "test18")
   (check-equal? (eval-exp (neg (ismunit (apair (num 3) (munit))))) (bool #t) "test19")
   (check-exn exn:fail?
              (lambda () (eval-exp (plus (num 5) (bool #t)))
              "test20"))
(check-exn exn:fail?
              (lambda () (eval-exp (minus (bool #f) (bool #t)))
              "test21"))
(check-exn exn:fail?
              (lambda () (eval-exp (div (num 5) (bool #f)))
              "test22"))
(check-exn exn:fail?
              (lambda () (eval-exp (mult (bool #t) (num -2)))
              "test23"))
   (check-exn exn:fail?
              (lambda () (eval-exp (num "hi"))
              "test24"))
     (check-exn exn:fail?
              (lambda () (eval-exp (num (num 3)))
              "test25"))
   (check-exn exn:fail?
              (lambda () (eval-exp (bool (bool #t)))
              "test26"))
   (check-exn exn:fail?
              (lambda () (eval-exp (plus (munit) (num 2)))
              "test27"))
   (check-exn exn:fail?
              (lambda () (eval-exp (1st (munit)))
              "test28"))
   (check-exn exn:fail?
              (lambda () (eval-exp (2nd (num 3)))
              "test29"))



        
(check-equal? (eval-exp (neg (num 0))) (num 0) "test30")
(check-equal? (eval-exp (neg (neg (num -11)))) (num -11) "test31")
   
   ; condition
   (check-equal? (eval-exp (cnd (bool #t) (plus (num 1) (num 2)) (num "-1"))) (num 3) "test32")
   (check-equal? (eval-exp (cnd (iseq (bool #t) (bool #f)) (munit) (bool #f))) (bool #f) "test33")
   (check-equal? (eval-exp (cnd (bool #f) (num 2) (bool #t))) (bool #t) "test34")
(check-exn exn:fail?
              (lambda () (eval-exp (cnd (num 2) (num 2) (bool #t)))
              "test35"))

(check-exn exn:fail?
              (lambda () (eval-exp (cnd (munit) (num 2) (bool #t)))
              "test36"))

(check-equal? (eval-exp (cnd (andalso (neg (bool #t)) (bool 2))
                             (plus (num 1) (num 2))
                             (mult (num -1) (num -2))
                             )) (num 2) "test37")


   


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