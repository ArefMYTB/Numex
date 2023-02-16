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


   (check-equal? (eval-exp (ifnzero (num 4) (num 1) (num 0))) (num 1) "test38")
   (check-equal? (eval-exp (ifnzero (mult (num 0) (num 4)) (num 1) (bool #t))) (bool #t) "test39")
  (check-exn exn:fail?
              (lambda () (eval-exp (ifnzero (bool #t) (num 2) (bool #t)))
              "test40"))

     (check-exn exn:fail?
              (lambda () (eval-exp (var munit)))
              "test41")

     (check-exn exn:fail?
              (lambda () (eval-exp (var (num 2))))
              "test42")

     (check-exn exn:fail?
              (lambda () (eval-exp (div (munit) (bool #f)))
              "test43"))
     
   (check-exn exn:fail?
              (lambda () (eval-exp (neg (munit))))
              "test44")

   
     
   (check-equal? (numexlist->racketlist
                  (eval-exp (apply (apply numex-all-gt (num 5))
                                  (racketlist->numexlist 
                                   (list (num 10) (num 4) (num 5) (num 15))))))
                 (list (num 10) (num 15))
                 "test45")

   (test-equal? "test46"
    (list (num 3) (num 4) (bool #t))
    (numexlist->racketlist (apair (num 3) (apair (num 4) (apair (bool #t) (munit))))))


(test-equal? "test47"
    (apair (list (num 42) (var "x")) (apair (list (bool #t) (var "y")) (munit)))
    (racketlist->numexlist (list (list (num 42) (var "x")) (list (bool #t) (var "y"))))
  )

  (test-equal? "test48"
   (apair (var "foo") (apair (num 17) (munit)))
   (racketlist->numexlist (list (var "foo") (num 17))))


         
   (check-equal? (racketlist->numexlist '(1 #t 3 #f))
                  (apair 1 (apair #t (apair 3 (apair #f (munit))))) "test49")
   (check-equal? (racketlist->numexlist '()) (munit) "test50")
   
   (check-equal? (numexlist->racketlist (apair #t (apair 2 (apair #f (apair 4 (munit))))))
                  '(#t 2 #f 4) "test51")
   (check-equal? (numexlist->racketlist (munit)) '() "test52")
   

  
   (check-equal? (eval-exp (ifleq (num 4) (num 5) (num 1) (bool #t))) (num 1) "test53")
   (check-equal? (eval-exp (ifleq (num 4) (num 4) (bool #f) (munit))) (bool #f) "test54")
   (check-equal? (eval-exp (ifleq (num 4) (num -4) (num 1) (num 0))) (num 0) "test55")
  
(check-exn exn:fail?
              (lambda () (eval-exp (ifleq (bool #t) (num -4) (num 1) (num 0))))
              "test56")
(check-exn exn:fail?
              (lambda () (eval-exp (ifleq (apair (munit) (munit)) (num -4) (num 1) (num 0))))
              "test57")

(test-equal? "test58"
               (num 2)
               (eval-exp (with "f1"
                               (lam "f1" "a" (with "x" (var "a") (lam "f2" "z" (plus (var "x") (num 1)))))
                               (with "f3" (lam "f3" "f" (with "x" (num 1729) (apply (var "f") (munit)))) 
                                     (apply (var "f3") (apply (var "f1") (num 1)))))))

(test-equal? "test59"
               (num 43)
               (eval-exp (apply (lam "incr" "x" (plus (var "x") (num 1))) (num 42))))


(test-equal? "test60"
               (bool #t)
               (eval-exp (apply (lam "neg" "x" (neg (var "x"))) (bool #f))))


(test-equal? "test61"
               (num 10)
               (eval-exp (apply (lam "mul" "x" (neg (mult (var "x") (num 2)))) (num -5))))

(test-equal? "test62"
               (bool #f)
               (eval-exp (ifleq (plus (num 3) (num 2)) (mult (num 2) (num 5))
                                (andalso (bool #t) (bool #f)) (neg (plus "wrong" "bad")))))

(test-equal? "test63"
               (num 2)
               (eval-exp (ifnzero (plus (num 2) (num -2))
                                  (neg (plus "wrong" "bad"))
                                  (mult (num 2) (num 1)))))

   (check-equal? (eval-exp (with "s" (num 4)
                                 (div (num 12) (var "s")))) (num 3) "test64")

  (test-equal? "test65"
     (apair (num 21) (bool #t))
     (eval-exp (apair (1st (apair (mult (num 7) (num 3)) (num 2)))
                 (2nd (apair (num 3) (bool #t)))
                 )))

  (test-equal? "test66"
     (num 6)
     (eval-exp (with "fnc"
       (lam "f1" "x" 
            (ifneq (ismunit (var "x")) (bool #f) 
                       (num 0) 
                       (plus (1st (var "x")) (apply (var "f1") (2nd (var "x"))))))
       (apply (var "fnc") (apair (num 1) (apair (num 2) (apair (num 3) (munit))))))))

   
   (check-equal? (eval-exp (1st (apair (num 3) (bool #t)))) (num 3) "test67")
   (check-equal? (eval-exp (2nd (apair (num 3) (bool #t)))) (bool #t) "test68")
   (check-equal? (eval-exp (ismunit (munit))) (bool #t) "test69")
   (check-equal? (eval-exp (apply (lam "a" "b" (ifleq (var "b") (num 5) (plus (var "b") (num 3))
                                                      (apply (var "a") (mult (num 2) (num 3)))))
                                  (num 2))) (num 5) "test70")
   
(check-equal? (eval-exp (apply (lam "fact" "n" 
           (cnd (iseq (var "n") (num 0)) 
                   (num 1) 
                   (mult (var "n") (apply (var "fact") (minus (var "n") (num 1))
           )))) (num 5)))
               (num 120) "test71")

   (check-equal? (eval-exp (with* (list (cons "f" (num 2)) (cons "y" (var "f"))) (plus (var "f") (plus (var "y") (num 3))))) (num 7) "test72")
   (check-equal? (eval-exp (ifmunit (munit) (plus (num 1) (num 2)) (plus (num 3)(num 4)))) (num 3) "test73")
   (check-equal? (eval-exp (ifmunit (bool #f)  (bool 2) (plus (num 1) (num 2)))) (num 3) "test74")
   (check-exn exn:fail?
              (lambda () (eval-exp (neg (munit))))
              "test75")
   (check-equal? (eval-exp (ifneq (num 1) (num 2) (num 3) (num 4) )) (num 3) "test76")
   (check-equal? (eval-exp (ifneq (num 1) (num 1) (num 3) (bool #t))) (bool #t) "test77")
   
    (check-equal? (num 1)
     (eval-exp (with* (cons (cons "x" (num 1)) null) (var "x")))
     "test78")

    (check-equal? 
    (bool #t)
     (eval-exp (with* (list (cons "f" (num 2)) (cons "y" (bool #t))) (ifneq (var "f") (num 2) (bool #f) (var "y"))))
     "test79")


    
     (check-exn exn:fail?
              (lambda () (eval-exp (num #t))
              "test80"))

        (check-exn exn:fail?
              (lambda () (eval-exp (bool "hi"))
              "test81"))

         (check-exn exn:fail?
              (lambda () (eval-exp (bool 22))
              "test82"))

         (check-exn exn:fail?
              (lambda () (eval-exp (bool (num 3)))
              "test83"))

         (check-exn exn:fail?
              (lambda () (eval-exp (num (bool #t)))
              "test84"))

   (check-equal? (eval-exp (plus (plus (num 1) (num 2)) (minus (num 3) (num 4)))) (num 2) "test85")
 (check-equal? (eval-exp (mult (mult (num 3) (num 2)) (mult (num 3) (num 4)))) (num 72) "test86")
   (check-exn exn:fail? (lambda () (eval-exp (mult (num 3) (munit)))) "test87")

   (check-equal? (eval-exp (num -5)) (num -5) "test88")
   (check-equal? (eval-exp (munit)) (munit) "test89")
    (check-equal? (eval-exp (closure '() (lam null "x" (var "x"))))
                 (closure '() (lam null "x" (var "x"))) "test90")


    (check-equal? (eval-exp (with "x" (andalso (bool #t) (bool #t)) (orelse (bool #f) (var "x")))) (bool #t) "test 91")
   (check-equal? (eval-exp (with "x" (num 1) (var "x"))) (num 1) "test92")
   (check-exn exn:fail? (lambda () (eval-exp (var "x"))) "test93")
   
    (check-equal? (eval-exp (lam null "x" (var "x")))
                 (closure '() (lam null "x" (var "x"))) "test94")
   (check-equal? (eval-exp (with "x" (num 1) (lam null "a" (var "x"))))
                 (closure (list (cons "x" (num 1))) (lam null "a" (var "x"))) "test95")


      (check-exn exn:fail? (lambda () (eval-exp (ifleq "1" (num 2) (num 3) (num 4)))) "test96")
      (check-exn exn:fail? (lambda () (eval-exp (ifnzero "1" (num 2) (num 3) ))) "test97")

      (check-equal? (eval-exp (apair (num 1) (num 1))) (apair (num 1) (num 1)) "test98")
   (check-equal? (eval-exp (with "x" (num 1) (apair (var "x") (var "x"))))
                 (apair (num 1) (num 1)) "test99")


   (check-equal? (eval-exp (1st (apair (num 1) (bool #t)))) (num 1) "test100")
   (check-equal? (eval-exp (with "x" (apair (num 1) (bool #t)) (1st (var "x")))) (num 1) "test101")
   (check-exn exn:fail? (lambda () (eval-exp (1st (plus (num 1) (num 2))))) "test102")


   (check-equal? (eval-exp (2nd (apair (num 1) (bool #t)))) (bool #t) "test103")
   (check-equal? (eval-exp (with "x" (apair (num 1) (bool #t)) (2nd (var "x")))) (bool #t) "test104")
   (check-exn exn:fail? (lambda () (eval-exp (2nd (plus (num 1) (bool #t))))) "test105")

   (check-equal? (eval-exp (ismunit (munit))) (bool #t) "test106")
   (check-equal? (eval-exp (with "x" (munit) (ismunit (var "x")))) (bool #t) "test107")
   (check-equal? (eval-exp (ismunit (num 0))) (bool #f) "test108")
   (check-equal? (eval-exp (with "x" (num 0) (ismunit (var "x")))) (bool #f) "test109")


   (check-equal? (eval-exp (with "double" (lam "double" "x" (plus (var "x") (var "x")))
                                  (apply (var "double") (num 10))))
                 (num 20) "test110")

      (check-equal?
    (eval-exp
     (with "range"
           (lam "range" "lo"
                (lam null "hi"
                     (ifleq  (var "hi") (var "lo") (munit)
                                (apair (var "lo") (apply (apply (var "range") (plus (num 1) (var "lo"))) (var "hi"))))))
           (apply (apply (var "range") (num 5)) (num 8))))
    (apair (num 5) (apair (num 6) (apair (num 7)  (munit)))) "test111")


      (check-equal?
 (eval-exp (apply (lam "a" "b" (ifleq (num 5) (var "b") (plus (var "b") (num 3))
                         (apply (var "a") (mult (num 2) (num 3))  ))) (num 2))
             )(num 9) "test112")


      (check-equal?
 (eval-exp (apply (lam "a" "b" (ifneq (var "b") (num 1) 
                         (with "b" (plus (var "b") (num -1)) (apply (var "a") (var "b")  ))
                         (num 3)
                         )) (num 2))
            ) (num 3) "test113")
;;

      (check-equal?
 (eval-exp (apply (apply (apply (lam "a" "b" (lam "x" "y" (lam "w" "r" (neg (mult (plus (var "b") (var "y")) (var "r"))))))
                       (num 2))
                       (num 3))
                       (num 5))
            ) (num -25) "test114")

      
(check-equal?
 (eval-exp (apply (lam "a" "b" (ifnzero (var "b") 
                                      (with* (list (cons "b" (plus (var "b") (num -1)))) (plus (num 1) (apply (var "a") (var "b"))))
                                      (num 3)
                                      )) (num 2))
             )(num 5) "test115")
;;

   (check-exn exn:fail? (lambda () (eval-exp (apply (num 1) (num 2)))) "test116")

      (check-exn exn:fail? (lambda () (eval-exp (list (num 1) (num 2)))) "test117")


(check-equal? (eval-exp (with* (list (cons "x" (num 1)) (cons "y" (num 2))) (plus (var "x")(var "y"))))
                 (num 3) "test118")
   (check-equal? (eval-exp (with* (list (cons "x" (bool #t))) (var "x")))
                 (bool #t) "test119")
   (check-equal? (eval-exp (with* (list (cons "x" (num 1)) (cons "x" (num 2))) (var "x")))
                 (num 2) "test120")

(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (plus (num 1) (var "x"))))
                   (apair (num 1) (apair (num 2) (munit)))))
                 (apair (num 2) (apair (num 3) (munit))) "test121")

(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (minus (num 1) (var "x"))))
                   (apair (num 1) (apair (num 2) (munit)))))
                 (apair (num -1) (munit)) "test122")

(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (mult (num 0) (var "x"))))
                   (apair (num 1) (apair (num 2) (munit)))))
                 (munit) "test123")

(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (num 0)))
                   (apair (bool #t) (apair (num 2) (munit)))))
                 (munit) "test124")
(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (minus (num 1) (var "x"))))
                    (munit)))
                 (munit) "test125")


(check-equal? (eval-exp
                  (apply (apply numex-filter (lam null "x" (div (num 1) (var "x"))))
                   (apair (num 2) (apair (num 1) (apair (num -2) (munit))))))
                 (apair (num 1) (munit)) "test126")

(check-equal? (eval-exp
                  (apply (apply numex-all-gt (num 1) )
                   (apair (num 2) (apair (num 1) (apair (num -2) (munit))))))
                 (apair (num 2) (munit)) "test127")

(check-equal? (eval-exp
                  (apply (apply numex-all-gt (num 2) )
                   (apair (num 2) (apair (neg (num 2)) (apair (num -2) (munit))))))
                 (munit) "test128")

(check-equal? (eval-exp
                  (apply (apply numex-all-gt (num -2) )
                   (munit)))
                 (munit) "test129")

(check-exn exn:fail? (lambda () (eval-exp
                  (apply (apply numex-all-gt (bool #t) )
                   (apair (num 2) (apair (num 2) (apair (num -2) (munit)))))))
                  "test130")

(check-exn exn:fail? (lambda () (eval-exp
                  (apply (apply numex-all-gt (num 2) )
                   (apair (num 2) (apair (bool #t) (apair (num -2) (munit)))))))
                  "test131")


(check-equal? (eval-exp (value "Donald Knuth" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 1) "test132")
(check-equal? (eval-exp (value "John McCarthy" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 2) "test133")
(check-equal? (eval-exp (value "Barbara Liskov" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (num 3) "test134")
(check-equal? (eval-exp (value "Lotfi A. Zadeh" (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit))))) (munit) "test135")
(check-equal? (eval-exp (value "Maryam Mirzakhani" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (munit)))))) (munit) "test136")
(check-equal? (eval-exp (value "Bertrand Russell" (record (key "Donald Knuth" (num 1)) (record (key "John McCarthy" (num 2)) (record (key "Barbara Liskov" (num 3)) (record (key "Zohar Manna" (num 4)) (munit))))))) (munit) "test137")

(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-odd") (num 11)))) (bool #t) "test138")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-odd") (num 10)))) (bool #f) "test139")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-even") (num 11)))) (bool #f) "test140")
(check-equal? (eval-exp (letrec "is-even" (lam null "n" (orelse (iseq (var "n") (num 0)) (apply (var "is-odd") (minus (var "n") (num 1))))) "is-odd" (lam null "n" (andalso (neg (iseq (var "n") (num 0))) (apply (var "is-even") (minus (var "n") (num 1))))) "temporary-1" (num 1) "temporary-2" (num 2) (apply (var "is-even") (num 10)))) (bool #t) "test141")

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