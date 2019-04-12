(define curry3
  (lambda (f)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          (f b c d ))))))


(define uncurry3
  (lambda (f)
    (lambda (x y z)
      (((f x) y) z))))


(define addthree_nums
  (lambda(x y z)
  (+ (+ x y) z)))

(define addone
  (lambda(x)
    (+ x 1)))



(define my-filter
  (lambda (f lst)
    (cond
      ((null? lst) '())
      ((equal? (f (car lst)) #f) (my-filter f (cdr lst)) )
      (else (append (list (car lst)) (my-filter f (cdr lst)) )))))

(define union
  (lambda(lst1 lst2)
    (append (my-filter (union-helper lst1) lst2) lst1)))


(define intersect
  (lambda (lst1 lst2)
    (my-filter (lambda(x) (member x lst2)) lst1)))

(define union-helper
  (lambda(lst1)
    (lambda(val)
      (member-helper val lst1))))

(define exists
  (lambda(f lst)
    (if (null? (my-filter f lst))
        #f
        #t)))
    



    
(define uncurry
  (lambda (curried)
    (lambda args
      args
      
      )))


    
;define a helper that keeps going through parameters
;call itself recursively
;if null return function






(define member-helper
  (lambda (val lst)
    (cond
      ((equal? (member val lst ) #f ) #t)
      (else #f))))
       
  


