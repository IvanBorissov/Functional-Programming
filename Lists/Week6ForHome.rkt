#lang racket

;-------------------------------------------------
;Task 1
;Define a procedure that accepts a list of numbers and a list of predicates
;and returns only the elements that satisfy all of the predicates.

(define (valid-element? a xs)
  (if (empty? xs)
      #t
  (and ((first xs) a) (valid-element? a (rest xs)))))

(define (where xs predxs)
  (define (helper xs answer)
    (cond [(empty? xs) (flatten answer)]
          [(valid-element? (first xs) predxs) (helper (rest xs)(cons answer (first xs)))]
          [else (helper (rest xs) answer)]))
  (helper xs '()))

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))

;-------------------------------------------------

;-------------------------------------------------
;Task 2
;Define a procedure that reverses a list using foldr.

(define (my-reverse-foldr xs) #t)
  

(equal? (my-reverse-foldr '(1 2 3 4 5)) '(5 4 3 2 1))
;-------------------------------------------------

;-------------------------------------------------
;Task 3
;Define a procedure that takes a list of numbers and returns a list of pairs in the form (xi . ni)
;where xi spans the elements of xs and ni is the number of elements in xs that are greater than xi.

(define (count-biggerthan x xs)
  (define (helper x xs cnt)
    (cond [(empty? xs) cnt]
          [(< x (first xs)) (helper x (rest xs) (+ cnt 1))]
          [else (helper x (rest xs) cnt)]))
  (helper x xs 0))
  
(define (num-bigger-elements xs)
  (define (helper tempxs answer)
    (cond [(empty? tempxs) (reverse answer)]
          [else (helper (rest tempxs) (cons (cons (first tempxs) (count-biggerthan (first tempxs) xs)) answer))]))
  (helper xs '()))

(num-bigger-elements '(5 6 3 4))
(num-bigger-elements '(1 1 1))

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
;-------------------------------------------------

;-------------------------------------------------
;Task 4
;Define a procedure that returns a list of pairs comprising the cartesian product of two sets.
;Note: There is a built-in procedure (cartesian-product xs ys [zs ...]). This exercise is the only time you should not use it.

(define (cartesianate x xs)
  (define (helper xs answer)
    (cond [(empty? xs) (reverse answer)]
          [else (helper (rest xs) (cons (cons x (first xs)) answer))]))
  (helper xs '()))

(define (my-cartesian-product xs ys)
  (define (helper xs answer)
    (cond [(empty? xs) (reverse answer)]
          [else (helper (rest xs) (cons (cartesianate (first xs) ys) answer))]))
  (helper xs '()))

(my-cartesian-product '(1 2) '(3 4))
(my-cartesian-product '(1 2 3 4 5) '(6 7 8))

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))

;-------------------------------------------------

;-------------------------------------------------
;Task 5
;Задача 2 Контролно 1 24.11.2021

(define (recalc-list xs)
  (define (helper xs answer)
    (cond [(empty? xs) (sort answer <)]
          [(empty? (first xs)) '()]
          [else (helper (rest xs) (cons (length (first xs)) answer))]))
  (helper xs '()))

(define (get-missing-length xss)
  (define newxs (recalc-list xss))
  (define (helper xs cnt)
    (cond [(empty? xs) (print "empty list")]
          [(< cnt (first xs)) cnt]
          [else (helper (rest xs) (+ cnt 1))]))
  (helper newxs (if (empty? newxs)
                               0
                               (first newxs))))

(get-missing-length '())
(get-missing-length '((1 2) (4 5 1 1) (1) () (5 6 7 8 9))); → 3
(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))); → 3
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a","a") ("a") ("a", "a", "a", "a", "a", "a"))); → 5

;-------------------------------------------------

;-------------------------------------------------
;Task 6
;Да се дефинира функция (persistence n), която приема естествено число n и
;връща точкова двойка от вида ‘(ys . x). Първият елемент на резултата е списък,
;чийто първи елемент е равен на произведението на цифрите на n, а всеки следващ;
;е равен на произведението на цифрите на предходния до получаването на
;едноцифрено произведение, на което е равен последният елемент на ys. Вторият
;елемент на резултата (т.е. x) е равен на дължината на списъка ys.

(define (magic x)
  (if(= x 0)
     1
     (* (remainder x 10) (magic (quotient x 10)))))

(define (persistence x)
  (define (helper x answer cnt)
    (cond [(< x 10) (cons(reverse(flatten answer)) cnt)]
          [else (helper (magic x) (cons (magic x) answer) (+ cnt 1))]))
  (helper (magic x) (list(magic x)) 1))

(magic 39)
(magic 27)
(magic 4)

(persistence 39) ; → '((27 14 4) . 3) ; 3*9=27, 2*7=14, 1*4=4
(persistence 126)  ;→ '((12 2) . 2) ; 1*2*6=12, 1*2=2
(persistence 4) ;→ '((4) . 1)
(persistence 999) ;→ '((729 126 12 2) . 4)

;-------------------------------------------------

;-------------------------------------------------
;Task 7
;Trailing zeros Контролно 1 24.11.2022

(define (kth-max-min xs)
  (λ (x) (if (> x (length (filter (λ (y) (< y 0)) xs)))
             (error "There is no such number")
             (list-ref (sort (filter (λ (y) (< y 0)) xs) >) (sub1 x))
             )
    ) 
  )
;-------------------------------------------------
