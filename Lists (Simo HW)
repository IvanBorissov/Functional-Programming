#lang racket
(require math/number-theory)

;----------------------------------------------------------------------
;Task 2
;Define a procedure longest-ascending-sub xs that returns the longest sublist (sequence of consecutive elements) that is sorted in ascending order from xs.

(define (longest-ascending-sub xs)
  (define (helper xs curr_list answer)
    (cond [(and (empty? xs) (>= (length curr_list) (length answer))) (flatten curr_list)]
          [(and (empty? xs) (< (length curr_list) (length answer))) (flatten answer)]
          [(<= (last curr_list) (first xs)) (helper (rest xs) (flatten(list curr_list (first xs))) answer)]
          [(and (> (last curr_list) (first xs)) (>= (length curr_list) (length answer))) (helper (rest xs) (flatten(list(first xs))) curr_list)]
          [(and (> (last curr_list) (first xs)) (< (length curr_list) (length answer))) (helper (rest xs) (flatten(list(first xs))) answer)]))
  (helper (rest xs) (list(car xs)) '()))

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))

(define xs '(1 2 3 4 5))
(define xs2  (list(car xs)))
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 3
;Define a procedure (set-union xs ys) that takes two sets of numbers and returns their union. It (the union) must be sorted in ascending order!

(define (set-union list1 list2)
  (define (helper xs ys ans)
    (cond [(empty? xs) (remove-duplicates(flatten(cons ans ys)))]
          [(empty? ys) (remove-duplicates(flatten(cons ans xs)))]
          [(< (first xs) (first ys)) (helper (rest xs) ys (cons ans (first xs)))]
          [else (helper xs (rest ys) (cons ans (first ys)))]))
  (helper list1 list2 '() ))  

(set-union '(1 3 5 7) '(5 7 13))
(set-union '(5 7 13) '(1 3 5 7))

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 4
;Define procedures that accept a list of digits and return the number that is build by traversing the list from right to left.
;Create two versions - one that utilizes folding, and another that does recursion.

(define (rev-lin-iter xs )
    (if (empty? xs)
        0
       (+ (first xs) (* (rev-lin-iter (rest xs)) 10))))

(rev-lin-iter '(1 2 3))
(rev-lin-iter '(1 2 3 4 5 6 7 8 9))

(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)

;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 5
;Define a procedure that reverses a list using foldl.

(define (my-reverse-foldl xs)
  (foldl cons '() xs))

(my-reverse-foldl '(1 2 3 4 5))
(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 6
;According to the fundamental theorem of arithmentics every natural number greater than 2 can be expressed as a product of prime numbers.
;Define a procedure that returns the sorted list of prime factors of a natural number.

(define (find-prime n)
  (define (helper n del)
    (cond [ (and (= 0 (remainder n del)) (prime? del)) del]
          [else (helper n (- del 1))]))
  (helper n n))

(define (factorize n)
  (if (= n 1)
      '()
      (flatten(cons(factorize (/ n (find-prime n))) (find-prime n)))))

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 7
;Define a procedure that accepts a list of numbers and returns an unary procedure of a natural number - k,
;such that the result from a call to it (the new procedure) is the kth largest negative number in the list.


(define (kth-max-min xs)
  (define (helper cnt k xs)
    (cond [(empty? xs) (print "error: No such number!")]
          [(= cnt k) (first xs)]
          [else (helper (+ cnt 1) k (rest xs))]))
  (λ (k) (helper 1 k (sort (remove-duplicates(filter negative? xs)) > ))))

(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!

(define (kth-max-min-Mitko xs)
  (λ (x) (if (> x (length (filter (λ (y) (< y 0)) xs)))
             (error "There is no such number")
             (list-ref (sort (filter (λ (y) (< y 0)) xs) >) (sub1 x))
             )
    ) 
  )

;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 8
;Take 2 strings s1 and s2 including only letters from a to z.
;Return a new sorted string, the longest possible, containing distinct letters - each taken only once - coming from s1 or s2.


(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")

;--------------------------------------------------------------------------
;Task 9
;By using num-to-xs and xs-to-num define a procedure that sorts a number in descending order.


(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task10
;Define a procedure (insert-at x i xs) that inserts an element at a given index in a list.

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))
(insert-at 7 4 '(1 2 3)) ; error: Invalid index!
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
;Task 11
;Define a procedure that concatenates two lists


; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly iterative process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
;--------------------------------------------------------------------------
