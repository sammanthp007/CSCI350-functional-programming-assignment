;;; This program is written by Samman Bikram Thapa (samman.thapa@bison.howard.
;;; edu) as part of an assignment for CSCI 350: Structures of Programming
;;; Language at Howard University.

(define (nmember atm lis)
  (cond
    ((NULL? lis) #F) ;; empty list
    ((EQ? atm (CAR lis)) #T)
    (ELSE (nmember atm (CDR lis)))
    )
  )

;;; 1. (25 pts) Write a function (reverse-general L). L is a list. The result
;;; of the function is the reversed version of L. Every single sub-list in L
;;; should be reversed as well. For example, the result of
;;; (reverse-general ‘(a b (c (d e)) f) should be (f ((e d) c) b a).
(define (reverse-general L)
  (cond
    ((NULL? L) '())
    (else (append (reverse-general (CDR L)) (List (car L))))
    )
  )
