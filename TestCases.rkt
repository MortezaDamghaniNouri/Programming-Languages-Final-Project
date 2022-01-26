#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)


(define tests
  (test-suite
   "Project Tests"

   
;; type inference tests:










;; type inference tests:
(check-equal? (infer-exp (plus (num 1) (num 2))) "int" "test142")
(check-equal? (infer-exp (plus (plus (num 4) (num 6)) (num 2))) "int" "test143")
(check-equal? (infer-exp (plus (plus (num 4) (num 6)) (plus (num 4) (num 6)))) "int" "test143")
(check-exn exn:fail? (lambda () (infer-exp (plus (plus (num 4) (bool #t)) (num 2)))) "test144")
(check-equal? (infer-exp (neg (num 1))) "int" "test145")
(check-equal? (infer-exp (neg (bool #t))) "bool" "test146")
(check-exn exn:fail? (lambda () (infer-exp (neg (munit)))) "test147")
(check-equal? (infer-exp (andalso (bool #t) (bool #f))) "bool" "test148")
(check-equal? (infer-exp (andalso (neg (bool #t)) (bool #f))) "bool" "test149")
(check-exn exn:fail? (lambda () (infer-exp (andalso (bool #t) (num 2)))) "test150")
(check-equal? (infer-exp (iseq (num 1) (num 2))) "bool" "test151")
(check-equal? (infer-exp (iseq (bool #t) (bool #f))) "bool" "test152")
(check-exn exn:fail? (lambda () (infer-exp (iseq (bool #t) (num 2)))) "test153")
(check-equal? (infer-exp (cnd (bool #t) (num 2) (num 3))) "int" "test154")
(check-equal? (infer-exp (cnd (bool #t) (bool #t) (bool #f))) "bool" "test155")
(check-equal? (infer-exp (cnd (andalso (bool #f) (bool #t)) (plus (num 5) (num 4)) (neg (num 3)))) "int" "test156")
(check-equal? (infer-exp (neg (neg (neg (num 1))))) "int" "test157")
(check-equal? (infer-exp (neg (neg (neg (bool #f))))) "bool" "test158")
(check-exn exn:fail? (lambda () (infer-exp (cnd (num 2) (num 2) (num 3)))) "test159")
(check-exn exn:fail? (lambda () (infer-exp (cnd (bool #t) (num 2) (bool #f)))) "test160")
(check-exn exn:fail? (lambda () (infer-exp (cnd (andalso (bool #f) (bool #t)) (plus (num 5) (num 4)) (neg (bool #t))))) "test161")
(check-exn exn:fail? (lambda () (infer-exp (cnd (plus (num 1) (num 2)) (plus (num 5) (num 4)) (neg (num 3))))) "test162")
(check-equal? (infer-exp (iseq (plus (num 1) (num 2)) (neg (num 2)))) "bool" "test163")
(check-equal? (infer-exp (iseq (neg (bool #t)) (neg (bool #f)))) "bool" "test164")
(check-exn exn:fail? (lambda () (infer-exp (iseq (plus (num 1) (num 2)) (neg (bool #f))))) "test165")
(check-equal? (infer-exp (with "x" (num 2) (plus (var "x") (num 3)))) "int" "test166")
(check-equal? (infer-exp (with "x" (neg (num 2)) (plus (num 2) (var "x")))) "int" "test167")
(check-equal? (infer-exp (with "x" (neg (bool #t)) (andalso (bool #f) (var "x")))) "bool" "test168")
(check-exn exn:fail? (lambda () (infer-exp (with "x" (neg (num 2)) (andalso (bool #f) (var "x"))))) "test169")
(check-exn exn:fail? (lambda () (infer-exp (with "x" (neg (bool #t)) (plus (num 2) (var "x"))))) "test170")
(check-equal? (infer-exp (munit)) "null" "test171")
(check-equal? (infer-exp (tlam "f" "x" "int" (plus (var "x") (num 3)))) (function "int" "int") "test172")
(check-equal? (infer-exp (tlam "f" "x" "int" (plus (num 2) (var "x")))) (function "int" "int") "test173")
(check-equal? (infer-exp (tlam "f" "x" "bool" (andalso (bool #f) (var "x")))) (function "bool" "bool") "test174")
(check-exn exn:fail? (lambda () (infer-exp (tlam "f" "x" "int" (andalso (bool #f) (var "x"))))) "test175")
(check-exn exn:fail? (lambda () (infer-exp (tlam "f" "x" "bool" (plus (num 2) (var "x"))))) "test176")
(check-equal? (infer-exp (tlam "f" "x" "bool" (plus (num 2) (cnd (var "x") (num 2) (num 3))))) (function "bool" "int") "test177")
(check-equal? (infer-exp (apply (tlam "f" "x" "int" (plus (var "x") (num 3))) (num 4))) "int" "test178")
(check-equal? (infer-exp (apply (tlam "f" "x" "int" (plus (var "x") (num 3))) (plus (plus (num 4) (num 6)) (num 2)))) "int" "test179")
(check-equal? (infer-exp (apply (tlam "f" "x" "bool" (plus (num 2) (cnd (var "x") (num 2) (num 3)))) (neg (bool #t)))) "int" "test180")
(check-equal? (infer-exp (apply (tlam "f" "x" "bool" (andalso (bool #f) (var "x"))) (neg (bool #t)))) "bool" "test181")
(check-exn exn:fail? (lambda () (infer-exp (apply (tlam "f" "x" "bool" (plus (num 2) (cnd (var "x") (num 2) (num 3)))) (neg (num 2))))) "test182")
(check-exn exn:fail? (lambda () (infer-exp (apply (tlam "f" "x" "bool" (andalso (bool #f) (var "x"))) (neg (num 2))))) "test183")
(check-equal? (infer-exp (apair (num 1) (munit))) (collection "int") "test184")
(check-equal? (infer-exp (apair (bool #t) (munit))) (collection "bool") "test185")
(check-exn exn:fail? (lambda () (infer-exp (apair (num 1) (num 2)))) "test186")
(check-exn exn:fail? (lambda () (infer-exp (apair (bool #t) (bool #f)))) "test187")
(check-equal? (infer-exp (apair (num 1) (apair (num 2) (munit)))) (collection "int") "test188")
(check-equal? (infer-exp (apair (bool #t) (apair (bool #t) (munit)))) (collection "bool") "test189")
(check-exn exn:fail? (lambda () (infer-exp (apair (num 1) (apair (bool #t) (munit))))) "test190")
(check-exn exn:fail? (lambda () (infer-exp (apair (bool #t) (apair (num 2) (munit))))) "test191")
(check-equal? (infer-exp (1st (apair (num 1) (apair (num 2) (munit))))) "int" "test192")
(check-equal? (infer-exp (1st (apair (bool #t) (apair (bool #t) (munit))))) "bool" "test193")
(check-exn exn:fail? (lambda () (infer-exp (1st (plus (num 2) (num 3))))) "test194")
(check-equal? (infer-exp (2nd (apair (num 1) (apair (num 2) (munit))))) (collection "int") "test195")
(check-equal? (infer-exp (2nd (apair (bool #t) (apair (bool #t) (munit))))) (collection "bool") "test196")
(check-equal? (infer-exp (apair (plus (num 2)(num 3)) (munit))) (collection "int") "test197")
(check-equal? (infer-exp (apair (andalso (bool #t)(bool #f)) (munit))) (collection "bool") "test198")
(check-equal? (infer-exp (2nd (2nd (2nd (2nd (2nd (apair (num 1) (munit)))))))) (collection "int") "test199")
(check-equal? (infer-exp (2nd (2nd (2nd (2nd (2nd (apair (bool #t) (munit)))))))) (collection "bool") "test200")
(check-exn exn:fail? (lambda () (infer-exp (2nd (plus (num 2) (num 3))))) "test201")
(check-equal? (infer-exp (ismunit (apair (num 1) (apair (num 2) (munit))))) "bool" "test202")
(check-equal? (infer-exp (ismunit (apair (bool #t) (apair (bool #f) (munit))))) "bool" "test203")
(check-equal? (infer-exp (ismunit (munit))) "bool" "test206")
(check-exn exn:fail? (lambda () (infer-exp (ismunit (plus (num 2) (num 3))))) "test207")
(check-exn exn:fail? (lambda () (infer-exp (ismunit (andalso (bool #t) (bool #f))))) "test208")






      


     
      

   


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