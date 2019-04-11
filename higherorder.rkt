;; 4/11/19
;; CS 251
;; Currying and higher order functions


;; Currying

(define curry3
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          (list a b c d ))))))

(define uncurry3
  (lambda (f)
    (define a (car (((f null) null) null)))
    (lambda ( x y z)
      (list a x y z))))

;; Higher order functions

(define my-filter
  (lambda (f lst)
    (cond
      ((null? lst) '())
      ((equal? (f (car lst)) #f) (my-filter f (cdr lst)) )
      (else (append (list (car lst)) (my-filter f (cdr lst)) )))))

