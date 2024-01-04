#lang racket
; Assignment 5
; Emma Mensah

; Question 1 -- Short-circuit evaluation
; AND Operator
;In this scenario, 'and' will short-circuit if the first case returns a false even if the second case is true.
;  Example, if x = 1, the first case is false but the second case holds true, however 'and' will short circuit at the first case.
(define (short-circuit x)
  (cond
    [(and (> x 2) (< x 10)) #f]
    (else #t)))

;example
(short-circuit 1)
(short-circuit 6)


;Question 2 -- Dynamic vs static typing
;In the function below that sums up all the items in a list, it is possible to add integer and decimal numbers if they
;existed in the given list. However, in a statically typed language like Java, this would throw an error as the type of the
;list has to be declared. Thus trying to sum a float and int in the same list would cause an error.
(define (lst-sum lst)
  (cond
    [(empty? lst) 0]
    [else (+(first lst) (lst-sum(rest lst)))]))

;example
(lst-sum '(2 4 1.1 3)) ;succesfully sums ints and decimals

;The second function that cannot be written in a statically typed language like Java is a join list function that simply joins two lists.
;Racket allows the combination of two lists even if the elements in both lists are of different types. This however would not work
;in Java because the data type of the list must be declared before hand and thus would only combine lists that have the same data type.
(define (join-lst lst-a lst-b)
  (cond
    [(empty? lst-a) lst-b]
    (else (cons (first lst-a) (join-lst (rest lst-a) lst-b)))))

;example
(join-lst '(a b c) '(1 2 3)) ;succesfully combines a list with both characters and numbers unlike Java, that works with lists of same type.


;EOF - EM
