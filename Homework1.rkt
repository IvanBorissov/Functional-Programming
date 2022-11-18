#lang racket
  
(define (sum-digits a)
    (if (= a 0)
        0
        (+ (remainder a 10) (sum-digits (quotient a 10)))))

(define (special-num? k a)
    (and (= 0 (remainder a k)) (= 0 (remainder (sum-digits a) k))))

(define (count-specials k a b)
    (cond   [(> a b) 0]
            [(special-num? k a) (+ 1 (count-specials k (+ a 1) b))]
            [else (count-specials k (+ a 1) b)] ))

(count-specials 3 3 9) 
(count-specials 5 10 100) 
(count-specials 8 100 200) 
(count-specials 15 1000 2000)

(display "-------------------------\n")

;------------------------------------------------------------------------------------------------------------------------------------

(define (count-digits n)
  (if (= n 0)
      0
      (+ 1 (count-digits( quotient n 10)))))

(define (find-max n1 n2)
  (if (> n1 n2)
      n1
      n2))

(define (pow n)
  (if (= n 0)
      1
      (* 10 (pow (- n 1)))))

(define (create-num n base)
 (+ (+ (* (quotient n (pow base)) (pow base)) (* (remainder n (pow (- base 1))) 10)) (remainder (quotient n (pow (- base 1))) 10)))

(define (max-rot n)
  (define (helper base num maxnum)
   (if (= base 1)
       maxnum
       (helper (- base 1) (create-num num base) (find-max (create-num num base) maxnum))))
  (helper (count-digits n) n n))

(max-rot 56789)
(max-rot 12490)
(max-rot 38458215)
(max-rot 195881031)
(max-rot 896219342)
(max-rot 69418307)
(max-rot 257117280)

;-----------------------------------------------------------------------------------------------------------------------------------

;(define (create-num1 n base)
 ; (* (quotient n (pow base)) (pow base)))

;(define (create-num2 n base)
 ; (* (remainder n (pow (- base 1))) 10))

;(define (create-num3 n base)
 ; (remainder (quotient n (pow (- base 1))) 10))

;(define (create-num n base)
 ;(+ (+ (* (quotient n (pow base)) (pow base)) (* (remainder n (pow (- base 1))) 10)) (remainder (quotient n (pow (- base 1))) 10)))

;(sum-digits 12345)
;(special-num? 2 3254)
;(special-num? 2 32547)



