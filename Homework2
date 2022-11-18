#lang racket

;-------------------------------------------------
;Task 1 Homework 2

(define (id x)
  x)

(define (pair-compose fs xs)
  (λ (y)
  (define (helper fs xs answer)
    (cond [(empty? (rest xs)) (+ answer ((first fs) (first xs) (id y)))] ;граничният случай при който ни е останала само една функция
          [else (helper (rest(rest fs)) (rest(rest xs)) (+ answer ((first fs) (first xs) ((first(rest fs)) (first(rest xs)) y))))]))
    (helper fs xs 0)))
  
(define fs (list *
 (λ (x y) (* x x x y))
 (λ (x y) (+ x 1 y))
 (λ (x y) (- x (+ 1 y)))
 (λ (x y) (* x y 2))))

(define xs '(1 2 3 4 5))

((pair-compose fs xs) 5); → 92
; ((* 1).(* 8) 5) + ((+ 4).(- 3) 5) + ((* 10).id 5) = 40 + 2 + 50 = 92

;-------------------------------------------------

;-------------------------------------------------
;Task 2 Homework2

(define (woodcutters xs)
  (define (helper prev xs answer) ;prev = xi ИЛИ xi + hi, в зависимост дали сме го бутнали напред или назад
    (cond [(empty? (rest xs))
           (+ answer 1)] ;последното дърво винаги го бутаме напред
          
          [(> (- (car(first xs)) (cdr(first xs))) prev)
           (helper (car(first xs)) (rest xs) (+ answer 1))] ;ако можем да го бутнем назад, го бутаме и най-левият ни кординат става xi
          
          [(< (+ (car(first xs)) (cdr(first xs))) (car(first(rest xs))))
           (helper (+(car(first xs)) (cdr(first xs))) (rest xs) (+ answer 1))] ;ако можем да го бутнем напред, го бутаме и най-левият кординат става xi + hi
          
          [else (helper (car(first xs)) (rest xs) answer)])); няма свободен интервал, в който да го бутнем => продължаваме да гледаме напред
  
  (helper (car (first xs)) (rest xs) 1)) ;първото дърво го бутаме назад

(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (19 . 1))) ; → 3
(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1))) ; → 4
(woodcutters '((10 . 4) (15 . 1) (19 . 3) (20 . 1))) ; → 4

(woodcutters '((1 . 7) (3 . 11) (6 . 12) (7 . 6) (8 . 5) (9 . 11)
 (15 . 3) (16 . 10) (22 . 2) (23 . 3) (25 . 7) (27 . 3) (34 . 5)
 (35 . 10) (37 . 3) (39 . 4) (40 . 5) (41 . 1) (44 . 1) (47 . 7)
 (48 . 11) (50 . 6) (52 . 5) (57 . 2) (58 . 7) (60 . 4) (62 . 1)
 (67 . 3) (68 . 12) (69 . 8) (70 . 1) (71 . 5) (72 . 5)
 (73 . 6) (74 . 4) ) ) ; → 10

;-------------------------------------------------
