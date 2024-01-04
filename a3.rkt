#lang racket

;; QUESTION 3
;determines if an integer is even
;zero is even
(define (iseven? x)
  (cond
    [(equal? x 0) #t]
    [(equal? (remainder x 2) 0) #t]
    [else #f]))


;determines if an integer is odd
(define (isodd? x)
  (cond
    [(equal? x 0) #f]
    [(equal? (remainder x 2) 1) #t]
    [else #f]))


;; QUESTION 4
;selects elements of the list based on a function passed in

(define (select func lst)
  (filter func lst))


;; QUESTION 5
;finds the greatest common denominator of two positive integers
(define (gcd a b)
  (cond
    [(equal? b 0) a]
    [else (gcd b (remainder a b))]))
