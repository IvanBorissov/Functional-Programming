#lang racket

;First exam 2nd var
;-------------------------------------------------
;Task 1

(define (toxs a)
  (if (= a 0)
      '()
      (cons (remainder a 10) (toxs (quotient a 10)))))

(define (palindrome? a)
  (define curr (toxs a))
  (define rev (reverse(toxs a)))
  (equal? curr rev))

(define (next-pali n)
  (define (helper a)
    (cond [(palindrome? a) a]
          [else (helper (+ a 1))]))
  (helper (+ n 1)))

(next-pali 808)
(next-pali 2133)
(next-pali 5645)
(next-pali 2863)
(next-pali 93)
(next-pali 4205)
(next-pali 2215)
;-------------------------------------------------

;-------------------------------------------------
;Task 2
(define (alt-split xs)
  (define (helper xs ans1 ans2 cnt)
    (cond [(empty? xs) (cons (reverse ans2) (list(reverse ans1)))]
          [(= 1 (remainder cnt 2)) (helper (rest xs) (cons (first xs) ans1) ans2 (+ cnt 1))]
          [else (helper (rest xs) ans1 (cons (first xs) ans2) (+ cnt 1))]))
  (helper xs '() '() 0))

(alt-split '(1 2 3 4 5))
(alt-split '(1 2 3 4 5 6 7 8 9 10 11))
(alt-split '(5 6 1 2 3 3 3 4 8 5 4 1))
;-------------------------------------------------

;-------------------------------------------------
;Task 3
(define (mandist a b)
 (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))

(define (get-dist p)
  (define pta '(0 . 0))
  (define (helper i j)
    (cond [(and (= i 101) (= j 101)) '(-1 . -1)]
          [(= j 101) (helper (+ i 1) 0)]
          [(and (equal? (mandist pta (cons i j)) (/ (mandist pta p) 2)) (equal? (mandist p (cons i j)) (/ (mandist pta p) 2))) (cons i j)]
          [else (helper i (+ j 1))]))
  (helper 0 0))

(get-dist '(49 . 3))
(get-dist '(2 . 50))
(get-dist '(13 . 0))
(get-dist '(0 . 41))
(get-dist '(42 . 0))
(get-dist '(0 . 36))
(get-dist '(13 . 37))
(get-dist '(42 . 16))
(get-dist '(42 . 13))
(get-dist '(0 . 0))
;-------------------------------------------------

;-------------------------------------------------
;Task4
