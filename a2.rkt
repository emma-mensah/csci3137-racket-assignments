#lang racket
; Assignment 2
; Emma Mensah

; QUESTION 1 -- Sum a list of integers
(define (lst-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+(first lst) (lst-sum(rest lst)))]))


; QUESTION 2 -- Find the mean of integers
; Made use of lst-sum function above
; Converted fraction result to decimal using exact->inexact
(define (mean lst)
  (cond
    [(empty? lst) 0]
    [else (exact->inexact(/ (lst-sum lst) (length lst)))]))


; QUESTION 3 -- Determine if a list is flat
; Made use of pair? function
; Assumption - empty list has no sublists thus is flat
(define (flat? lst)
  (cond
    [(empty? lst) #t]
    [(pair? (first lst)) #f]
    [else (flat? (rest lst))]))


; QUESTION 4 -- check if a value(v) is in a list
; used flat? to determine if list is flat, if #t recurse on the rest of list
; if #f check if the first element is a list, if #t recurse on the first and rest of the list
; else recurse on the rest of list
(define (in-list? v lst)
  [cond
    [(empty? lst) #f]
    [(equal? v (first lst)) #t]
    [(flat? lst) (in-list? v (rest lst))]
    [(list? (first lst)) (or (in-list? v (first lst)) (in-list? v (rest lst)))]
    [else (in-list? v (rest lst))]])


; QUESTION 5 -- determine if a list is in ascending order
; created a helper function to return second value in a list for comparison
(define (sorted-helper lst)
  (cond
    [(empty? lst) empty]
    [else (first (rest lst))]))

(define (sorted? lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst))]
    [(<= (first lst) (sorted-helper lst)) (sorted? (rest lst))]
    [else #f]))


; QUESTION 6 -- sort a list of integers in ascending order
;; Used quick sort algorithm 
(define (sort lst)
  (cond
    [(empty? lst) empty]
    [(equal? (empty? (rest lst)) #t) lst]
    (else (append (sort (filter (lambda (i) (< i (first lst))) lst))
                  (list (first lst)) (sort (filter (lambda (i) (> i (first lst))) lst))))))


; QUESTION 7 -- Set difference
; Returns elements in list 1 that are not present in list 2
(define (difference lst1 lst2)
  (cond
    [(empty? lst1) empty]
    [(empty? lst2) lst1]
    [else (filter (lambda (i) (not (member i lst2))) lst1)]))

;EOF - EM