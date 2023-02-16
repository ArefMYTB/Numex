#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)

(struct bool (b) #:transparent)  ;; boolean

(struct plus  (e1 e2)  #:transparent)  ;; add two expressions

(struct minus  (e1 e2)  #:transparent)  ;; subtract two expressions
(struct mult  (e1 e2)  #:transparent)  ;; multiply 
(struct div  (e1 e2)  #:transparent)  ;; divide
(struct neg (e1)      #:transparent)  ;; negation
(struct andalso  (e1 e2)  #:transparent)  ;; logical conjunction
(struct orelse  (e1 e2)  #:transparent)  ;; logical disjunction
(struct cnd  (e1 e2 e3)  #:transparent)  ;; condition
(struct iseq  (e1 e2)  #:transparent)  ;; comparison
(struct ifnzero  (e1 e2 e3)  #:transparent)  ;; if not zero condition
(struct ifleq  (e1 e2 e3 e4)  #:transparent)  ;; less or equal condition

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct tlam  (nameopt formal arg-type body) #:transparent) ;; a typed argument, recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with  (s e1 e2)  #:transparent)  ;; let
(struct apair  (e1 e2)  #:transparent)  ;; pair
(struct 1st  (e1)  #:transparent)  ;; first part of a pair
(struct 2nd  (e1)  #:transparent)  ;; second part of a pair

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k rm) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

(define (racketlist->numexlist xs) (
                                    cond [(null? xs) (munit)] [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs) ) ) ] [#t (error "The input is not a racket list") ]
                                    ))

(define (numexlist->racketlist xs) (
                                    cond [(munit? xs) null] [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs) ) ) ] [#t (error "The input is not a Numex list")]
                                    ))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(list? env) (cond ((equal? str (car (car env))) (cdr (car env)))
                           (#t (envlookup (cdr env) str)) )]
        [#t (error "Error in envlookup: invalid argument type")]
		)
 )


;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        
        [(num? e)
         (cond[(integer? (num-int e)) e]
              [else (error "NUMEX num applied to non-racket-integer")])]
        
        [(bool? e)
         (cond[(boolean? (bool-b e)) e]
              [else (error "NUMEX bool applied to non-racket-boolean")])]
        
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
               (cond[(and (num? v1)(num? v2)) (num (- (num-int v1) (num-int v2)))]
                    [else (error "NUMEX subtraction applied to non-number")]))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
               (cond[(and (num? v1)(num? v2)) (num (* (num-int v1) (num-int v2)))]
                    [else (error "NUMEX multiplication applied to non-number")]))]
        
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (equal? (num-int v2) 0)
                   (error "NUMEX division applied to Zero" v2)
                   (num (quotient (num-int v1) 
                       (num-int v2))))
               (error "NUMEX division applied to non-number")))]

        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond  ((bool? v1) (bool (not (bool-b v1))))
                  ((num? v1) (num (* -1 (num-int v1))))  
               (#t (error "NUMEX orelse applied to non-boolean"))))]
        
        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (cond[(bool? v1)(cond[(equal? (bool-b v1) #f) v1]
                                [else (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                      (cond[(bool? v2) v2]
                                           [else (error "NUMEX conjunction applied to non-number")]))])]
                [else (error "NUMEX conjunction applied to non-number")]))]

        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (cond[(bool? v1)(cond[(equal? (bool-b v1) #t) v1]
                                [else (let ([v2 (eval-under-env (orelse-e2 e) env)])
                                      (cond[(bool? v2) v2]
                                           [else (error "NUMEX conjunction applied to non-number")]))])]
                [else (error "NUMEX conjunction applied to non-number")]))]
        
        [(cnd? e) 
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (bool? v1)
               (cond[(equal? (bool-b v1) #t)(let ([v2 (eval-under-env (cnd-e2 e) env)]) v2)]
                    [(equal? (bool-b v1) #f)(let ([v3 (eval-under-env (cnd-e3 e) env)]) v3)])
               (error "NUMEX condition applied to non-boolean")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (if (and (or (bool? v1)
                    (num? v1))
                   (or (bool? v2)
                    (num? v2)) )
               (cond ((equal? v1 v2) (bool #t))
                    (#t (bool #f)))
               (cond ((not (and (or (bool? v1) (num? v1))
                    (or (bool? v2) (num? v2) ))) (error "NUMEX iseq applied to neither boolean or num"))
                     ((not (or (and (bool? v1) (bool? v2))
                    (and (num? v1) (num? v2) ))) (error "NUMEX iseq applied to different types")))
               ))]
        
        [(ifnzero? e) 
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
               (cond[(not (equal? (num-int v1) 0))(let ([v2 (eval-under-env (ifnzero-e2 e) env)]) v2)]
                    [(equal? (num-int v1) 0)(let ([v3 (eval-under-env (ifnzero-e3 e) env)]) v3)])
               (error "NUMEX ifnotzero condition applied to non-number")))]

        [(ifleq? e) 
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (cond[(> (num-int v1) (num-int v2))(let ([v4 (eval-under-env (ifleq-e4 e) env)]) v4)]
                    [(not (> (num-int v1) (num-int v2)))(let ([v3 (eval-under-env (ifleq-e3 e) env)]) v3)])
           (error "NUMEX ifleq condition applied to non-numbers")))]
        
        [(lam? e)
        (if (and (or (string? (lam-nameopt e)) (null? (lam-nameopt e))) (string? (lam-formal e)))
            (closure env e)
        (error "NUMEX function name and parameter name must be string"))]

        [(apply? e) 
         (let ([v1 (eval-under-env (apply-funexp e) env)])
           (if (closure? v1)
               (cond ((equal? null (lam-nameopt (closure-f v1)))
                      (eval-under-env (lam-body (closure-f v1)) (cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env))  (closure-env v1))) )
                     (#t (eval-under-env (lam-body (closure-f v1)) (cons (cons (lam-nameopt (closure-f v1)) v1)(cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env))  (closure-env v1))))))
               (cond ((lam? v1) (eval-under-env (apply v1 (apply-actual e)) env))
               (#t (error "NUMEX ~v not a function" (apply-funexp e)) ))))]
        
        [(with? e)
         (let ([v1 (eval-under-env (with-e1 e) env)])
              (cond[(string? (with-s e)) (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))]
              [#t (error "NUMEX with applied to non-string")]))]

        [(apair? e) 
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        
        [(1st? e) 
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "NUMEX 1st applied to non-apair")))]

        [(2nd? e) 
         (let ([v1 (eval-under-env (2nd-e1 e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "NUMEX 2nd applied to non-apair")))]

        [(munit? e) (munit)]

        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (cond[(munit? v1)(bool #t)]
                [else (bool #f)]))]

        [(closure? e) e]

        [(key? e)
         (let ([ex (eval-under-env (key-e e) env)])
               (cond[(string? (key-s e)) e]
                    [else (error "key is not a string")]))]
        
        [(record? e)
         (let ([k (eval-under-env (record-k e) env)]
               [rm (eval-under-env (record-rm e) env)])
               (cond[(key? k) (cond[(or (munit? (eval-exp rm)) (record? rm)) (record k rm)]
                                   [else (error "value of record invalid")])]
                [else (error "key invalid")]))]

        [(value? e)
         (let ([rec (eval-under-env (value-r e) env)])
               (cond[(and (string? (value-s e)) (record? rec))
                 (define (find-key goal-str rec)
                 (let ([key-str (key-s (record-k rec))]
                       [key-val (key-e (record-k rec))]
                       [next-rec (record-rm rec)]) 
                       (cond[(equal? goal-str key-str) key-val]
                            [(munit? (eval-exp next-rec)) (munit)]
                            [else (find-key goal-str next-rec)])))(find-key (value-s e) rec)]
                    [else (error "NUMEX value applied to non-string")]))]

        [(letrec? e)
            (cond [(and (string? (letrec-s1 e))
                     (string? (letrec-s2 e))
                     (string? (letrec-s3 e))
                     (string? (letrec-s4 e)))
              (eval-under-env (letrec-e5 e) (append (list (cons (letrec-s1 e) (letrec-e1 e))
                                                          (cons (letrec-s2 e) (letrec-e2 e))
                                                          (cons (letrec-s3 e) (letrec-e3 e))
                                                          (cons (letrec-s4 e) (letrec-e4 e))) env))]
              [#t (error "NUMEX letrec applied to non-string s")])]
                
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

        [(plus? e)
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: aithmetic operation applied to non-integer")))]

        [(munit? e) "null"]
        
        [(ismunit? e)
         (let ([t (infer-under-env (ismunit-e e) env)])
           (if (or (collection? t) (equal? "null" t))
               "bool"
               (error "e1 is not a collection nor null")))]

        
        [(bool? e)
         (if (boolean? (bool-b e))
             "bool"
             (error "BOOL not a boolean")
             )]

        [(minus? e)
         (let ([t1 (infer-under-env (minus-e1 e) env)]
               [t2 (infer-under-env (minus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: aithmetic operation applied to non-integer")))]

        [(mult? e)
         (let ([t1 (infer-under-env (mult-e1 e) env)]
               [t2 (infer-under-env (mult-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: aithmetic operation applied to non-integer")))]

        [(div? e)
         (let ([t1 (infer-under-env (div-e1 e) env)]
               [t2 (infer-under-env (div-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: aithmetic operation applied to non-integer")))]

        
        [(andalso? e)
          (let ([t1 (infer-under-env (andalso-e1 e) env)]
               [t2 (infer-under-env (andalso-e2 e) env)])
           (if (and (equal? "bool" t1)
                    (equal? "bool" t2))
               "bool"
               (error "NUMEX TYPE ERROR: logical operation applied to non-integer")))]

        [(orelse? e)
          (let ([t1 (infer-under-env (orelse-e1 e) env)]
               [t2 (infer-under-env (orelse-e2 e) env)])
           (if (and (equal? "bool" t1)
                    (equal? "bool" t2))
               "bool"
               (error "NUMEX TYPE ERROR: logical operation applied to non-integer")))]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        
        [(neg? e)
         (if (munit? (neg-e1 e))
             (error "e is munit")
             (infer-under-env (neg-e1 e) env)
             )]

        [(cnd? e)
         (let
           ([t1 (infer-under-env (cnd-e1 e) env)]
           [t2 (infer-under-env (cnd-e2 e) env)]
           [t3 (infer-under-env (cnd-e3 e) env)])
           (if (equal? "bool" t1)
               (if (equal? t2 t3)
                   t2
                   (error "e2 and e3 should be the same type"))
               (error "e1 must be bool")))]

        [(iseq? e)
         (let
           ([t1 (infer-under-env (iseq-e1 e) env)]
           [t2 (infer-under-env (iseq-e2 e) env)])
           (if (equal? t1 t2)
               "bool"
               (error "e1 and e2 should be the same type")
               )
           )]

        
        [(with? e)
              (if (string? (with-s e))
                           (let ([t1 (infer-under-env (with-e1 e) env)])
                             (infer-under-env (with-e2 e) (cons (cons (with-s e) t1) env)))
                           (error "s should be string"))]


        [(tlam? e)
         (function (tlam-arg-type e) (infer-under-env (tlam-body e) (cons (cons (tlam-formal e) (tlam-arg-type e)) env)))]
        

        [(apply? e)
         (let ([t1 (infer-under-env (apply-funexp e) env)]
               [t2 (infer-under-env (apply-actual e) env)])
         (if (function? t1)
                        (if (equal? t2 (function-input-type t1))
                            (function-output-type t1)
                            (error "arugment should be the same type as input type"))
                        (error "first argument should be a function")))]

        [(apair? e) 
         (let ([t1 (infer-under-env (apair-e1 e) env)]
               [t2 (infer-under-env (apair-e2 e) env)])
           (if (or (equal? t2 (collection t1)) (equal? t2 "null"))
                    (collection t1)
               (error "NUMEX TYPE ERROR: apair's second input should be null or a collection with the correct type")))]
                   
        [(1st? e)
         (let ([t (infer-under-env (1st-e1 e) env)])
           (if (collection? t)
           (collection-type t)
           (error "e1 should be a collection type")))]

        [(2nd? e)
         (let ([t (infer-under-env (2nd-e1 e) env)])
           (if (collection? t)
           t
           (error "e1 should be a collection type")))]

        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))

(define (with* bs e2)
  (cond [(null? bs) e2]
        [(with (caar bs) (cdar bs) (with* (cdr bs) e2))]))

(define (ifneq e1 e2 e3 e4)
  (with "_x" e1
        (with "_y" e2
              (cnd (iseq (var "_x") (var "_y")) e4 e3
               ))))

;; Problem 5

(define numex-filter (lam null "f"
                           (
                            lam "map" "list"
                                (ifmunit (var "list")
                                         (munit)
                                         (ifnzero (apply (var "f") (1st (var "list")))
                                                      (apair (apply (var "f") (1st (var "list"))) (apply (var "map") (2nd (var "list"))))
                                                      (apply (var "map") (2nd (var "list"))))))))

(define numex-all-gt
  (with "filter" numex-filter
        (lam null "i"
              (lam null "list"
                   (apply
                    (apply
                     (var "filter")
                     (lam "gt" "num"
                          (ifleq (var "num") (var "i") (num 0) (var "num")))) (var "list"))))))

;; Problem 6

(define type-error-but-evaluates-ok (cnd (bool #t) (num 2) (bool #t)))
(define type-ok-but-evaluates-error (div (num 1) (num 0)))

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