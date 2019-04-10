(define mult
  (lambda (a)
    (lambda (b)
      (* a b))))

(define curry3
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (list a b c)
       )
      )
   )
)
