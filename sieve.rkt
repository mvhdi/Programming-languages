(define seq
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda ()
              (seq (+ start 1) stop))))))


(define inf-seq
  (lambda (start)
        (cons start
            (lambda ()
              (inf-seq (+ start 1)))))) 

(define first-n
  (lambda (lazy-list n)
    (cond
     ((equal? lazy-list #f) '())
     ((> 1 n) '())
     ((= 1 n) (list (car lazy-list)))
     (else (append (list (car lazy-list)) (first-n (call-lazy-list lazy-list) (- n 1)))))))

(define call-lazy-list
  (lambda (lazy-list)
    ((cdr lazy-list))))

(define nth
  (lambda (lazy-list n)
   (cond
    ((< n 1) '())
    ((< (length (reverse (first-n lazy-list n)) ) n) #f)
    (else (car (reverse (first-n lazy-list n))) ))))


(define return-all
  (lambda (lazy-list)
    (cond
      ((equal? lazy-list #f) '())
      (else (append (list (car lazy-list)) (return-all (call-lazy-list lazy-list))) )
      
    )))

(define is-multiple
  (lambda (val1)
    (lambda (val2)
    (cond
      ((integer? (/ val2 val1)) #t)
      (else #f)
     ))))


(define filter-multiples
  (lambda (lazy-list n)
    (filter  (is-multiple n ) (return-all lazy-list) )))



(define sieve
  (lambda (lazy-list)
   ( lazy-list-from-lst (filter-multiples  (call-lazy-list lazy-list) (car lazy-list)  ) )
    ))

(define lazy-list-from-lst
  (lambda (lst)
    (if (null? lst)
        #f
        (cons (car lst)
            (lambda ()
              (lazy-list-from-lst (cdr lst)))))))

(define primes
  (lambda ()
       (sieve (inf-seq 2))
    ))



;; tests 
(define x (seq 5 20) )
(first-n x 7)

(first-n x 100)

(first-n x 0)

(nth x 10)
(nth x 50)
(nth x 0)
(return-all x)



(filter-multiples x 2)
(filter-multiples x 3)

