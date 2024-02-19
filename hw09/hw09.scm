(define (ascending? asc-lst) 
    (if (or (null? asc-lst) (null? (cdr asc-lst)))
        #t
        (if (<= (car asc-lst) (car (cdr asc-lst)))
            (ascending? (cdr asc-lst))
            #f
        )
    )
)


(define (my-filter pred s) 
    (cond
        ((null? s) '())
        ((pred (car s)) 
            (cons (car s) (my-filter pred (cdr s)))
        )
        (else
            (my-filter pred (cdr s))
        )
    )
)

(define (interleave lst1 lst2) 
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else
            (cons (car lst1) (interleave lst2 (cdr lst1)))
        )
    )
)

(define (no-repeats lst) 
    (define (in_lst lst s)
        (if (null? lst)
            #f
            (if (= s (car lst))
                #t
                (in_lst (cdr lst) s)
            )
        )
    )
    (define (helper lst acc)
        (cond 
            ((null? lst)
                acc
            )           
            ((in_lst acc (car lst))
                (helper (cdr lst) acc)
            )
            (else
                (helper (cdr lst) (cons (car lst) acc))
            )
        )
    )
    (define (rev lst res)
        (if (null? lst)
            res
            (rev (cdr lst) (cons (car lst) res))
        )
    )
    (rev (helper lst '()) '())
)
