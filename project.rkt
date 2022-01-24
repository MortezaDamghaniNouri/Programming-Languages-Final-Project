;; PL Project - Fall 2021
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions

;; Added structs
(struct bool  (b)  #:transparent)
(struct minus  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent)
(struct div  (e1 e2)  #:transparent)
(struct neg  (e1)  #:transparent)
(struct andalso  (e1 e2)  #:transparent)
(struct orelse  (e1 e2)  #:transparent)
(struct cnd  (e1 e2 e3)  #:transparent)
(struct iseq  (e1 e2)  #:transparent)
(struct ifnzero  (e1 e2 e3)  #:transparent)
(struct ifleq  (e1 e2 e3 e4)  #:transparent)
(struct lam  (s1 s2 body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (e1 e2)       #:transparent) ;; function application
(struct with  (s e1 e2)  #:transparent)
(struct apair  (e1 e2)  #:transparent)
(struct 1st  (e1)  #:transparent)
(struct 2nd  (e1)  #:transparent)
(struct munit () #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e1) #:transparent)
(struct closure (env f) #:transparent)
(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions
(struct tlam  (nameopt formal arg-type body) #:transparent) ;; a typed argument, recursive(?) 1-argument function
(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r



;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1


(define (racketlist->numexlist xs) (
cond [(null? xs) munit] [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs) ) ) ] [#t (error "The input is not a racket list") ]

                                    ))

(define (numexlist->racketlist xs) (
cond [(munit? xs) null] [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs) ) ) ] [#t (error "The input is not a Numex list")]

                                    ))



;; Problem 2
;; This function checks the types of inputs of different types in Numex
(define (is_valid e)(
cond [(var? e) (cond ((string? (var-string e)) e) (#t (error "The input of the var type is not a string") )) ]
[(num? e) (cond [(integer? (num-int e) ) e ] [#t (error "The input of the num type is not an integer") ] ) ]
[(bool? e) (cond [(boolean? (bool-b e)) e] [#t (error "The input of the bool type is not a boolean") ] ) ]
[(munit? e) e]
[(apair? e) (apair (is_valid (apair-e1 e) ) (is_valid (apair-e2 e) ) ) ]
[(closure? e) e]
[#t (error "The input is not a valid Numex type") ]
                     ))


;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond
    [(null? env) (error "unbound variable during evaluation" str)]
    [(equal? (car (car env) ) str) (cdr (car env) ) ]
    [#t (envlookup (cdr env) str)]
                
 ) )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        [(num? e) (is_valid e) ]
        [(bool? e) (is_valid e)]
        [(munit? e) (is_valid e)]
        [(apair? e) (is_valid e)]
        [(closure? e) (is_valid e)]
        ;; Plus
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]



               
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1)
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        ;; Minus
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1)
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        ;; Mult
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1)
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        
        ;; Div
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (/ (num-int v1)
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        
        ;; Andalso
        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)]
               [v2 (eval-under-env (andalso-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (and (bool-b v1)
                       (bool-b v2)))
               (error "NUMEX andalso applied to non-booleans")))]

        ;; Orelse
        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)]
               [v2 (eval-under-env (orelse-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (or (bool-b v1)
                       (bool-b v2)))
               (error "NUMEX orelse applied to non-booleans")))]


        ;; Neg
        [(neg? e) 
         (cond [(bool? (eval-under-env (neg-e1 e) env) ) (bool (not (neg-e1 e) ) ) ]
               [(num? (eval-under-env (neg-e1 e) env) ) (num (- (neg-e1 e) (* 2 (neg-e1 e) ) ) ) ]
               [#t (error "NUMEX neg applied to something which is neither bool nor num")]
               )]

       ;; Cnd
        [(cnd? e)
         (cond [(and (bool? (eval-under-env (cnd-e1 e) env) ) (and (bool-b (cnd-e1 e)) #t) ) (eval-under-env (cnd-e2 e) env) ]
               [(and (bool? (eval-under-env (cnd-e1 e) env) ) (and (not (bool-b (cnd-e1 e))) #t) ) (eval-under-env (cnd-e3 e) env) ]
               [#t (error "Not a valid NUMEX cnd")]
               )]
        
        ;;Iseq
        [(iseq? e)(

          cond [(and (num? (eval-under-env (iseq-e1 e) env) ) (num? (eval-under-env (iseq-e2 e) env)) ) (bool (= (num-int (iseq-e1 e) ) (num-int (iseq-e2 e) ) ) ) ]
               [(and (num? (eval-under-env (iseq-e1 e) env) ) (bool? (eval-under-env (iseq-e2 e) env) ) ) (bool #f)]
               [(and (bool? (eval-under-env (iseq-e1 e) env) ) (num? (eval-under-env (iseq-e2 e) env) ) ) (bool #f)]
               [(and (bool? (eval-under-env (iseq-e1 e) env) ) (bool? (eval-under-env (iseq-e2 e) env) ) ) (bool (equal? (bool-b (iseq-e1 e) ) (bool-b (iseq-e2 e) ) ) )]
               [#t (error "The inputs of iseq are not bool or num") ]
                   )]




        
        


        
        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]

        ;; Plus
        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]
        
        ;; Minus
        [(minus? e) 
         (let ([t1 (infer-under-env (minus-e1 e) env)]
               [t2 (infer-under-env (minus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: subtraction applied to non-integer")))]


        ;; Mult
        [(mult? e) 
         (let ([t1 (infer-under-env (mult-e1 e) env)]
               [t2 (infer-under-env (mult-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: multiplication applied to non-integer")))]

        ;; Div
        [(div? e) 
         (let ([t1 (infer-under-env (div-e1 e) env)]
               [t2 (infer-under-env (div-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: division applied to non-integer")))]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        ;; Munit
        [(munit? e) "null"]

        ;; Andalso
        [(andalso? e)(
                      cond [(and (equal? "bool" (infer-under-env (andalso-e1 e) env) ) (equal? "bool" (infer-under-env (andalso-e2 e) env) ) ) "bool" ]
                           [#t (error "NUMEX TYPE ERROR: the inputs of andalso are not boolean")]
                     )]
        ;; Neg
        [(neg? e)(
                  cond [(equal? "int" (infer-under-env (neg-e1 e) env) ) "int" ]
                       [(equal? "bool" (infer-under-env (neg-e1 e) env) ) "bool" ]
                       [#t (error "NUMEX TYPE ERROR: the type of teh inputs of neg must be int or bool which is not")]
                  )]

        ;; Cnd
        [(cnd? e) (
                   cond [(and (equal? "bool" (infer-under-env (cnd-e1 e) env) ) (equal? (infer-under-env (cnd-e2 e) env) (infer-under-env (cnd-e3 e) env) ) ) (infer-under-env (cnd-e2 e) env) ]
                        [#t (error "NUMEX TYPE ERROR: the type of cnd can not be recognized")]

                   )]

        ;; Iseq
        [(iseq? e) (
                    cond [(equal? (infer-under-env (iseq-e1 e) env) (infer-under-env (iseq-e2 e) env) ) "bool" ]
                         [#t (error "NUMEX TYPE ERROR: the inputs of the iseq do not have the same type") ]

                    )]

        

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 5

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Problem 6

(define type-error-but-evaluates-ok "CHANGE")
(define type-ok-but-evaluates-error "CHANGE")

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
