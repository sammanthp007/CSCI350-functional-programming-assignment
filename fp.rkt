;;; This program is written by Samman Bikram Thapa (samman.thapa@bison.howard.
;;; edu) as part of an assignment for CSCI 350: Structures of Programming
;;; Language at Howard University.


;;; 1. (25 pts) Write a function (reverse-general L). L is a list. The result
;;; of the function is the reversed version of L. Every single sub-list in L
;;; should be reversed as well. For example, the result of
;;; (reverse-general ‘(a b (c (d e)) f) should be (f ((e d) c) b a).
(define (reverse-general L)
  (cond
    ((NULL? L) '())
    (else (append (reverse-general (CDR L)) ((lambda(x)
                                               (if (LIST? x)
                                                 (LIST (reverse-general x))
                                                 (list x)
                                                 )
                                               ) (CAR L))
                  ))
    )
  )

;(reverse-general '(a b c (d e f) g h))


;;; (25 pts) Write a function (sum-up-numbers-simple L). L is a list, which
;;; may contain as elements numbers and non-numbers. The result of the function
;;; is the sum of the numbers not in nested lists in L. If there are no such
;;; numbers, the result is zero. For example, the result of
;;; (sum-up-numbers-simple ‘(a b 1 2 c 3 d)) should be 6.

(define (sum-up-numbers-simple L)
  (cond
    ; if L is empty list
    ((null? L) '0)
    ; if first element is list
    ((list? (car L)) (sum-up-numbers-simple(cdr L)))
    ; if first element is not a number
    ((not (number? (car L))) (sum-up-numbers-simple(cdr L)))
    ; calculate recursively
    (else (+ (car L) (sum-up-numbers-simple(cdr L)))
          ))
  )

; (sum-up-numbers-simple '())
; (sum-up-numbers-simple '(100 200))
; (sum-up-numbers-simple '(a b c) )
; (sum-up-numbers-simple '(100 a))
; (sum-up-numbers-simple '(a 100))
; (sum-up-numbers-simple '(a 100 b 200 c 300 d))
; (sum-up-numbers-simple '(()))
; (sum-up-numbers-simple '((100)))
; (sum-up-numbers-simple '(100 (200)))
; (sum-up-numbers-simple '(a 100 b (200) c 300 d))
; 
; (sum-up-numbers-simple '(100 (200) (a) a b a c 200 (2)))
; (sum-up-numbers-simple '((((3)))))

;;; (25 pts) Write a function (sum-up-numbers-general L). L is a list, which 
;;; may contain as elements numbers and non-numbers. The result of the function
;;; is the sum of all the numbers (including those in nested lists) in L. If 
;;; there are no such numbers, the result is zero. For example, the result of 
;;; (sum-up-numbers-general ‘(a b 1 (2 c (3)) d)) should be 6.

(define (sum-up-numbers-general L)
  (cond
    ; if L is empty list
    ((null? L) '0)
    ; if first element is list
    ((list? (car L)) (+ (sum-up-numbers-general (car L)) (sum-up-numbers-general(cdr L))))
    ; if first element is not a number
    ((not (number? (car L))) (sum-up-numbers-general(cdr L)))
    ; calculate recursively
    (else (+ (car L) (sum-up-numbers-general(cdr L)))
          ))
  )

(sum-up-numbers-general '(100 (200) (a) a b a c 200 (2)))
(sum-up-numbers-general '((((3)))))
