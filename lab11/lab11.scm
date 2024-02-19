(define (over-or-under num1 num2) 
  (cond
    ((< num1 num2) -1)
    ((= num1 num2) 0)
    (else 1)
  )
)

(define (make-adder num)
  (define (add x)
    (+ x num)
  )
  add
)

(define (composed f g)
  (lambda (x) (f (g x)))
)

(define (repeat f n)
  (define (helper x)
    (if (not (= n 0))
      (f ((repeat f (- n 1)) x ))
      x
    )
  )
  helper
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) 
  (let ((x (min a b)) (y (max a b)))
    (if (= x 0)
      y
      (gcd x (modulo y x))
    )  
  )
)
