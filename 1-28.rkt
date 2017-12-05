#lang planet neil/sicp
(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
        (Rabin (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
(define (Rabin n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1))  
             (not (= n (- m 1)))  
             (= x 1))  
        0
        x)))

(define (next? a n)
  (= (expmod a n n) a))

(define (judge? n times)
  (cond ((= times 0) true)
        ((next? times n) (judge? n (- times 1)))
        (else false)))

(define (carmichael n)
  (judge? n (- n 1)))