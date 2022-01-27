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
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"
;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")







;; Problem 1


(define (racketlist->numexlist xs) (
cond [(null? xs) (munit)] [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs) ) ) ] [#t (error "The input is not a racket list") ]

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
[(apair? e) e]
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



;; This function is used in Value for finding the corresponding value of s
(define (does_exist s r) (
cond [(equal? s (key-s (record-k r) ) ) (key-e (record-k r) ) ]
     [(munit? (record-r r) ) (munit) ]
     [#t (does_exist s (record-r r) ) ]
            ))








;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        [(num? e) (is_valid e) ]
        [(bool? e) (is_valid e)]
        [(munit? e) (is_valid e)]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env ) (eval-under-env (apair-e2 e) env ))]
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
               (num (quotient (num-int v1)
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        
        ;; Andalso
        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (cond [(and (bool? v1) (equal? (bool-b v1) #f ) ) (bool #f) ]
                 [(and (bool? v1) (bool? (eval-under-env (andalso-e2 e) env ) ) ) (bool (and (bool-b v1) (bool-b (eval-under-env (andalso-e2 e) env) ) )) ]
                 [#t (error "NUMEX andalso applied to non-booleans" ) ]


             ))]

        ;; Orelse
        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (cond [(and (bool? v1 ) (equal? (bool-b v1) #t) ) (bool #t)]
                 [(and (bool? v1) (bool? (eval-under-env (orelse-e2 e) env ) ) ) (bool (or (bool-b v1) (bool-b (eval-under-env (orelse-e2 e) env) ) ) ) ]
                 [#t (error "NUMEX orelse applied to non-booleans" )]
                 ))]


        ;; Neg
        [(neg? e) 
         (cond [(bool? (eval-under-env (neg-e1 e) env) ) (bool (not (bool-b (eval-under-env (neg-e1 e) env) ) ) ) ]
               [(num? (eval-under-env (neg-e1 e) env) ) (num (- (num-int (eval-under-env (neg-e1 e) env) ) (* 2 (num-int (eval-under-env (neg-e1 e) env) ) ) ) ) ]
               [#t (error "NUMEX neg applied to something which is neither bool nor num")]
               )]

       ;; Cnd
        [(cnd? e)
         (cond [(and (bool? (eval-under-env (cnd-e1 e) env) ) (and (bool-b (eval-under-env (cnd-e1 e) env )) #t) ) (eval-under-env (cnd-e2 e) env) ]
               [(and (bool? (eval-under-env (cnd-e1 e) env) ) (and (not (bool-b (eval-under-env (cnd-e1 e) env))) #t) ) (eval-under-env (cnd-e3 e) env) ]
               [#t (error "Not a valid NUMEX cnd")]
               )]
        
        ;;Iseq
        [(iseq? e)(

          cond [(and (num? (eval-under-env (iseq-e1 e) env) ) (num? (eval-under-env (iseq-e2 e) env)) ) (bool (= (num-int (eval-under-env (iseq-e1 e) env ) ) (num-int (eval-under-env (iseq-e2 e) env ) ) ) ) ]
               [(and (num? (eval-under-env (iseq-e1 e) env) ) (bool? (eval-under-env (iseq-e2 e) env) ) ) (bool #f)]
               [(and (bool? (eval-under-env (iseq-e1 e) env) ) (num? (eval-under-env (iseq-e2 e) env) ) ) (bool #f)]
               [(and (bool? (eval-under-env (iseq-e1 e) env) ) (bool? (eval-under-env (iseq-e2 e) env) ) ) (bool (equal? (bool-b (eval-under-env (iseq-e1 e) env ) ) (bool-b (eval-under-env (iseq-e2 e) env) ) ) )]
               [#t (error "The inputs of iseq are not bool or num") ]
                   )]

        ;; Ismunit
        [(ismunit? e) (
                       cond [(munit? (eval-under-env (ismunit-e1 e) env) ) (bool #t) ]
                            [#t (bool #f) ]

                       )]


        ;; Ifnzero
        [(ifnzero? e) (
                       cond [(zero? (num-int (eval-under-env (ifnzero-e1 e) env) ) ) (eval-under-env (ifnzero-e3 e) env) ]
                            [#t (eval-under-env (ifnzero-e2 e) env)]

                       )]

        ;; ifleq
        [(ifleq? e) (
                     cond [(> (num-int(eval-under-env (ifleq-e1 e) env) ) (num-int(eval-under-env (ifleq-e2 e) env) ) ) (eval-under-env (ifleq-e4 e) env ) ]
                          [#t (eval-under-env (ifleq-e3 e) env) ]

             ) ]

        ;; With
        [(with? e) 
                    (eval-under-env (with-e2 e) (append (list (cons (eval-under-env (with-s e) env) (eval-under-env (with-e1 e) env) )) env))
             ]

        ;; Lam
        [(lam? e) 
        (if (and (or (string? (lam-s1 e)) (null? (lam-s1 e))) (string? (lam-s2 e)))
             (closure env e)
             (error "NUMEX function name and parameter name must be string")
        )]



        

        ;; Apply
        [(apply? e)
        (
         cond [(closure? (eval-under-env (apply-e1 e) env ) ) (
            let ([v (eval-under-env (apply-e2 e) env)])
            (let ([clsr_fun (closure-f (eval-under-env (apply-e1 e) env) )])
              (if (null? (lam-s1 clsr_fun))
                (eval-under-env (lam-body clsr_fun) (cons (cons (lam-s2 clsr_fun) v) (closure-env (eval-under-env (apply-e1 e) env ) )))
                (eval-under-env (lam-body clsr_fun) (cons (cons (lam-s1 clsr_fun) (eval-under-env (apply-e1 e) env) ) 
                    (cons (cons (lam-s2 clsr_fun) v) (closure-env (eval-under-env (apply-e1 e) env ) ))))
              )
            )


                                                               ) ]
              [#t (error "The first input of apply is not a closure") ]
      
        )]


        ;; 1st
        [(1st? e) (
                   cond [(apair? (eval-under-env (1st-e1 e) env ) ) (eval-under-env (apair-e1 (eval-under-env (1st-e1 e) env)) env ) ]
                        [#t (error "The input of 1st is not an apair") ]

             )]
        
        ;; 2nd
        [(2nd? e) (
                   cond [(apair? (eval-under-env (2nd-e1 e) env ) ) (eval-under-env (apair-e2 (eval-under-env (2nd-e1 e) env )) env ) ]
                        [#t (error "The input of 2nd is not an apair") ]

             )]

        ;;Letrec
        [(letrec? e) (
                      eval-under-env (letrec-e5 e) (append (list (cons (letrec-s1 e) (eval-under-env (letrec-e1 e) env ) ) (cons (letrec-s2 e) (eval-under-env (letrec-e2 e) (list (cons (letrec-s1 e) (eval-under-env (letrec-e1 e) env ) ) env) ) ) (cons (letrec-s3 e) (eval-under-env (letrec-e3 e) env ) ) (cons (letrec-s4 e) (eval-under-env (letrec-e4 e) env ) ) ) env)


             )]


        ;; Key
        [(key? e) (
                   cond [(string? (eval-under-env (key-s e) env) ) (key (eval-under-env (key-s e) env) (eval-under-env (key-e e) env)) ]
                        [#t (error "The inputs of the key are not proper") ]
             )]

        ;; Record
        [(record? e) (
                      cond [(and (key? (eval-under-env (record-k e) env) ) (record? (eval-under-env (record-r e) env )) ) (record (eval-under-env (record-k e) env ) (eval-under-env (record-r e) env) ) ]
                           [(and (key? (eval-under-env (record-k e) env) ) (munit? (eval-under-env (record-r e) env )) ) (record (eval-under-env (record-k e) env ) (eval-under-env (record-r e) env) ) ]
                           [#t (error "The inputs of the record are not proper") ]


             )]


        
        
        
        ;; Value
        [(value? e)(
                    cond [(and (string? (eval-under-env (value-s e) env ) ) (record? (eval-under-env (value-r e) env ) ) ) (does_exist (eval-under-env (value-s e) env ) (eval-under-env (value-r e) env  ) ) ]





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
        ;; Num
        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        ;; Bool
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


        ;; Function
        [(tlam? e) (

                    function (tlam-arg-type e) (infer-under-env (tlam-body e) (append (list (cons (tlam-formal e) (tlam-arg-type e) ) ) env ) ) 
                    )]
        
        ;; Apply
        [(apply? e) (
                     cond [(and (function? (infer-under-env (apply-e1 e) env ) ) (equal? (function-input-type (infer-under-env (apply-e1 e) env) ) (infer-under-env (apply-e2 e) env) ) ) (function-output-type (infer-under-env (apply-e1 e) env) ) ]
                          [#t (error "NUMEX TYPE ERROR: the inputs of the apply do not have the same function type") ]
                     )
         ]

        
        ;; Apair
        [(apair? e) (
                     cond [(or (equal? (collection (infer-under-env (apair-e1 e) env) ) (infer-under-env (apair-e2 e) env) ) (equal? "null" (infer-under-env (apair-e2 e) env) ) ) (collection (infer-under-env (apair-e1 e) env) ) ]
                          [#t (error "NUMEX TYPE ERROR: the types of the elements of the apair are not equal") ]
             )]
        
        ;; 1st
        [(1st? e) (
                   cond [(collection? (infer-under-env (1st-e1 e) env) ) (collection-type (infer-under-env (1st-e1 e) env) ) ]
                        [#t (error "NUMEX TYPE ERROR: the type of the input of 1st is not proper") ]
             )]

        ;; 2nd
        [(2nd? e) (
                   cond [(collection? (infer-under-env (2nd-e1 e) env) ) (infer-under-env (2nd-e1 e) env)  ]
                        [#t (error "NUMEX TYPE ERROR: the type of the input of 2nd is not proper") ]
             )]

        
        ;;Ismunit
        [(ismunit? e) (
                       cond [(or (collection? (infer-under-env (ismunit-e1 e) env) ) (equal? "null" (infer-under-env (ismunit-e1 e) env) ) ) "bool"]
                            [#t (error "NUMEX TYPE ERROR: the input of the ismunit does not have a proper type")]

             )
         ]

        ;; With
        [(with? e) (
                    infer-under-env (with-e2 e) (append (list (cons (with-s e) (infer-under-env (with-e1 e) env ) ) ) env)


                    )]
        
        



        

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4


(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))

(define (with* bs e2) 
  (cond
    [(null? bs) e2]
    [else (with (car (car bs) ) (cdr(car bs)) (with* (cdr bs) e2))]))


(define (ifneq e1 e2 e3 e4) (cnd (iseq e1 e2) e4 e3))










;; Problem 5

(define numex-filter 
(lam null "mapper" 
(lam "map" "lst" 
(cnd (ismunit (var "lst")) 
(munit)
(with "result" (apply (var "mapper") (1st (var "lst"))) 
(ifnzero (var "result")
(apair (var "result") (apply (var "map") (2nd (var "lst"))))
(apply (var "map") (2nd (var "lst")))))))))

(define numex-all-gt
  (with "filter" numex-filter
    (lam null "i"
    (lam null "list"
    (apply 
    (apply (var "filter") (lam null "number"
    (ifleq (var "number") (var "i")(num 0)(var "number"))))
    (var "list"))))))


;; Problem 6

(define (type-error-but-evaluates-ok) (eval-exp (apair (bool #f) (num 10) ) ) )
;; Another example for this part is: (eval-exp (andalso (bool #f) (num 12) ) )



(define (type-ok-but-evaluates-error) (infer-exp (2nd (2nd (apair (num 12) (munit) ) ) ) ))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent)

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (car (compute-free-vars-handler e))
)


(define (compute-free-vars-handler e)
  (cond [(var? e) 
          (cons e (set (var-string e)))]
        ;; Num
        [(num? e)
        (cons e (set))]

        ;; Plus
        [(plus? e)
        (let ([v1 (compute-free-vars-handler (plus-e1 e))]
        [v2 (compute-free-vars-handler (plus-e2 e))])
        (cons (plus (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]

        ;; Minus
        [(minus? e)
        (let ([v1 (compute-free-vars-handler (minus-e1 e))]
        [v2 (compute-free-vars-handler (minus-e2 e))])
        (cons (minus (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]

        ;; Mult
        [(mult? e)
        (let ([v1 (compute-free-vars-handler (mult-e1 e))]
        [v2 (compute-free-vars-handler (mult-e2 e))])
        (cons (mult (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]

        ;; Div
        [(div? e)
        (let ([v1 (compute-free-vars-handler (div-e1 e))]
        [v2 (compute-free-vars-handler (div-e2 e))])
        (cons (div (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]


        ;; Bool
        [(bool? e)
        (cons e (set))]

        ;; Andalso
        [(andalso? e)
        (let ([v1 (compute-free-vars-handler (andalso-e1 e))]
        [v2 (compute-free-vars-handler (andalso-e2 e))])
        (cons (andalso (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]

        ;; Orelse
        [(orelse? e)
        (let ([v1 (compute-free-vars-handler (orelse-e1 e))]
        [v2 (compute-free-vars-handler (orelse-e2 e))])
        (cons (orelse (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; Neg
        [(neg? e)
        (let ([v (compute-free-vars-handler (neg-e1 e))])
        (cons (neg (car v)) (cdr v)))]

        ;; Cnd
        [(cnd? e)
        (let ([v1 (compute-free-vars-handler (cnd-e1 e))]
        [v2 (compute-free-vars-handler (cnd-e2 e))]
        [v3 (compute-free-vars-handler (cnd-e3 e))])
        (cons (cnd (car v1) (car v2) (car v3)) (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))))]
      
        ;; Iseq
        [(iseq? e)
        (let ([v1 (compute-free-vars-handler (iseq-e1 e))]
        [v2 (compute-free-vars-handler (iseq-e2 e))])
        (cons (iseq (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; Ifnzero
        [(ifnzero? e)
        (let ([v1 (compute-free-vars-handler (ifnzero-e1 e))]
        [v2 (compute-free-vars-handler (ifnzero-e2 e))]
        [v3 (compute-free-vars-handler (ifnzero-e3 e))])
        (cons (ifnzero (car v1) (car v2) (car v3)) (set-union (cdr v1) (cdr v2) (cdr v3))))]
        
        ;; Ileq
        [(ifleq? e)
        (let ([v1 (compute-free-vars-handler (ifleq-e1 e))]
        [v2 (compute-free-vars-handler (ifleq-e2 e))]
        [v3 (compute-free-vars-handler (ifleq-e3 e))]
        [v4 (compute-free-vars-handler (ifleq-e4 e))])
        (cons (ifleq (car v1) (car v2) (car v3) (car v4)) (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
        
        ;; Lam
        [(lam? e)
        (let ([lam_body (compute-free-vars-handler (lam-body e))])
        (let ([free-var-set (set-remove (set-remove (cdr lam_body) (lam-s2 e)) (lam-s1 e))])
        (cons (fun-challenge (lam-s1 e) (lam-s2 e) (car lam_body) free-var-set) free-var-set)))]
      
        ;; Apply
        [(apply? e)
        (let ([va (compute-free-vars-handler (apply-e1 e))]
        [vf (compute-free-vars-handler (apply-e2 e))])
        (cons (apply (car vf) (car va)) (set-union (cdr vf) (cdr va))))]

        ;; With
        [(with? e)
        (let ([v1 (compute-free-vars-handler (with-e1 e))]
        [v2 (compute-free-vars-handler (with-e2 e))])
        (cons (with (with-s e) (car v1) (car v2)) (set-union (set-remove (cdr v2) (with-s e)) (cdr v1))))]
        
        ;; Apair
        [(apair? e)
        (let ([v1 (compute-free-vars-handler (apair-e1 e))]
        [v2 (compute-free-vars-handler (apair-e2 e))])
        (cons (apair (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; 1st 
        [(1st? e)
        (let ([v (compute-free-vars-handler (1st-e1 e))])
        (cons (1st (car v)) (cdr v)))]

        ;; 2nd
        [(2nd? e)
        (let ([v (compute-free-vars-handler (2nd-e1 e))])
        (cons (2nd (car v)) (cdr v)))]
       
        ;; Munit
        [(munit? e)
        (cons e (set))]

        ;; Ismunit
        [(ismunit? e)
        (let ([v (compute-free-vars-handler (ismunit-e1 e))])
        (cons (ismunit (car v)) (cdr v)))]

        ;; Closure
        [(closure? e)
        (cons e (set))]
        
        ;; Letrec
        [(letrec? e)
         (let ([v1 (compute-free-vars-handler (letrec-e1 e))]
               [v2 (compute-free-vars-handler (letrec-e2 e))]
               [v3 (compute-free-vars-handler (letrec-e3 e))])
        (cons (letrec (car v1) (car v2) (car v3)) (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))))]
       
        ;; Key
        [(key? e)
          (let ([v (compute-free-vars-handler (key-e e))])
          (cons (key (car v)) (cdr v)))]
        
        ;; Record
        [(record? e)
          (let ([v (compute-free-vars-handler (record-r e))])
          (cons (record (car v)) (cdr v)))]

        ;; Value
        [(value? e)
          (let ([v (compute-free-vars-handler (value-r e))])
          (cons (value (car v)) (cdr v)))]
        
        [#t (error ("The used syntax is not accepted in NUMEX"))]
))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond [(var? e)
          (envlookup env (var-string e))]
      
        [(num? e)
          (cond [(integer? (num-int e)) e]
                [else (error "NUMEX num applied to non racket integer")])]
      
        
        [(bool? e) (is_valid e)]
        [(munit? e) (is_valid e)]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env ) (eval-under-env-c (apair-e2 e) env ))]
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
               (num (quotient (num-int v1)
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]


        [(with? e)
          (let ([v (eval-under-env-c (with-e1 e) env)])
            (if (string? (with-s e))
              (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v) env))
              (error "NUMEX key applied to not-string variable")))]


         ;; lam
        [(lam? e)
          (if (and (or (string? (lam-s1 e))
                       (null? (lam-s1 e)))
                   (string? (lam-s2 e)))
            (closure env e)
            (error "NUMEX lam name and parameter name must be string"))]
        
        [(fun-challenge? e)
         (let ([nameopt (fun-challenge-nameopt e)]
               [formal (fun-challenge-formal e)]
               [freevars (fun-challenge-freevars e)])
         (if (and (or (string? nameopt) (null? nameopt)) (string? formal))
             (closure (commons env freevars)  e)
             (error "NUMEX function name and parameter name must be string")))]

        
        
))

(define (commons env set)
  (if (equal? env null) null
      (if (set-member? set (car (car env)))
          (cons (car env) (commons (cdr env) set))
          (commons (cdr env) set)))
)


;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null)
)
