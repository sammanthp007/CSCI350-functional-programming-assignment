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

;(sum-up-numbers-general '() )
;(sum-up-numbers-general '(100) )
;(sum-up-numbers-general '(100 200)  )
;(sum-up-numbers-general '(a) )
;(sum-up-numbers-general '(a 100 b 200 c 300 d) )
;(sum-up-numbers-general '(()))
;(sum-up-numbers-general '((100)))
;(sum-up-numbers-general '(100 (200)))
;(sum-up-numbers-general '(a 100 b (200) c 300 d))
;(sum-up-numbers-general '(a 100 ((b ((200) c)) 300 d)))

;;; 4. (25 pts) Write a function (min-above-min L1 L2). L1 and L2 are both 
;;; simple lists, which do not contain nested lists. Both lists may have 
;;; non-numeric elements. The result of the function is the minimum of the 
;;; numbers in L1 that are larger than the smallest number in L2. If there is 
;;; no number in L2, all the numbers in L1 should be used to calculate the 
;;; minimum. If there is no number in L1 larger than the smallest number in L2, 
;;; the result is false (#F). For example, the result of 
;;; (min-above-min ‘(2 a 1 3) ‘(b 5 3 1)) should be 2.

(define (greater_than lis val output)
  (cond
    ; if car lis = null then do nothing
    ((null? (cdr lis)) (if (and (number? (car lis)) (> (car lis) val))
                           (cons (car lis) output)
                           output))
    ; if car lis > val then append to output
    ((and (number? (car lis)) (> (car lis) val)) (greater_than (cdr lis) val (cons (car lis) output)))
    ; if car lis <= val then do nothing
    (else (greater_than (cdr lis) val output))
  )
)

;;; returns the minimum number as an atom of the list
(define (min_atm_of lis)
  ; if the first atm is a number then 
  (cond
    ; if lis is empty return empty 
    ((null? lis) #f)
    ; if (car lis) is not a number return min_atm_of(cdr lis) 
    ((not (number? (car lis))) (min_atm_of(cdr lis)))
    ; if (car lis) is a number and if (min_atm_of_cdr lis) is empty list
    ((and (number? (car lis)) (not (min_atm_of(cdr lis)))) (car lis))
    ; if (car lis) is a number and car lis is less than everything else
    ((< (car lis) (min_atm_of(cdr lis))) (car lis))
    ; else return min from the remaining list
    (else (min_atm_of(cdr lis)))
  )
)


(define (min-above-min L1 L2)
  (cond
    ; If there is no number in L2, all the numbers in L1 should be used to calculate the minimum
    ((null? L2) (min_atm_of L1))
    ((null? L1) #f)
    (else (cond
            ; if 
            ((null? (min_atm_of ( greater_than L1 (min_atm_of L2) '()))) #f)
            (else (min_atm_of ( greater_than L1 (min_atm_of L2) '())))
            )
     )
  )
)

(min-above-min '() '(a 100 b 200 c 300 d))
(min-above-min '(100) '())
(min-above-min '(a 200 b 100 c 300 d)  '())
(min-above-min '(a) '())
(min-above-min '(a) '(a 200 b 300 c 100 d))
(min-above-min '(a b c) '(a 200 b 300 c 100 d))
(min-above-min '(a 200) '(a 200 b 300 c 100 d))
(min-above-min '(a 100) '(a 200 b 300 c 100 d))
(min-above-min '(100 200 300) '(300 100 200))
(min-above-min '(a 300 b 100 c 200 d) '(a 200 b 300 c 100 d))