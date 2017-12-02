#lang planet neil/sicp
(define (showpascal n)
  (define (showi i)
    (displayrow i)
    (newline)
    (if (< i n)
        (showi (+ 1 i)))
    )
  (showi 1)
  )


(define (pascals row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascals (- row 1) (- col 1))
               (pascals (- row 1) col)
               )
              )
        )
  )



(define (displayrow n)
  (define (displayrowi i)
    (display (pascals n i))
    (display " ")
    (if (< i n)
        (displayrowi (+ 1 i))
        
        )
    )
  (displayrowi 1)
 )