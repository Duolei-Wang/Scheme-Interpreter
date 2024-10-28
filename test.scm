
(define-syntax if
  (syntax-rules ()
    ((if condition then else) (check? condition then else))
  )
)


(if (eq? #t #t) 
  1 
  2))