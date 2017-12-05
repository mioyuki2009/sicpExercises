#lang planet neil/sicp
(define (smallest-divisor n)
  (find-divisor n 2))

#|(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
|#
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square x) (* x x))

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (find-biggerthan-prime n)
  (if (fast-prime? n 10) (time-prime-test n)
      (find-biggerthan-prime (+ 1 n))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;;(Rabin (square (expmod base (/ exp 2) m)) m))
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (Rabin n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1))  
             (not (= n (- m 1)))  
             (= x 1))  
        0
        x
        )))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (next? a n)
  (= (expmod a n n) a))

(define (judge? n times)
  (cond ((= times 0) true)
        ((next? times n) (judge? n (- times 1)))
        (else false)))

(define (carmichael n)
  (judge? n (- n 1)))