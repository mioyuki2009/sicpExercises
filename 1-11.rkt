#lang planet neil/sicp


(define (fnd n)
  (cond ((< n 3) n)
        (else (+ (+ (fnd (- n 1))
                 (* 2 (fnd (- n 2)))
                 )
                (* 3 (fnd (- n 3))
                 )
                )
              )
        )
  )

(define (fndd n)
  (cond ((> 3 n) n)
        (else (getans 0 1 2 3 n)
              )
        )
  )
(define (getans a b c n max)
  (define m (+ c (+ (* a 3) (* 2 b))))
  (if (= n max)
      m
      (getans b c m (+ 1 n) max))
  )
