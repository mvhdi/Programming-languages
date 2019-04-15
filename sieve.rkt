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




(define is-multiple
  (lambda (val1)
    (lambda (val2)
    (cond
      ((integer? (/ val1 val2)) #t)
      (else #f)))))




(define primes
  (lambda ()
    (primes-helper 2 '() )))


(define primes-helper
  (lambda (start lst)
        (cons (car (prime-finder start lst))
            (lambda ()
              (primes-helper (+ (car (prime-finder start lst)) 1) (prime-finder start lst))))))

(define prime-finder
  (lambda (start lst)
    (cond
      ((check-prime start lst) (append (list start) lst))
      (else (prime-finder (+ start 1) lst) ))))


(define check-prime
  (lambda (val lst)
    (cond
      ( (null? (filter (is-multiple val ) lst ) )    #t)
      (else #f))))


;; tests 
(define x (seq 5 20) )
(first-n x 7)

(first-n x 100)

(first-n x 0)

(nth x 10)
(nth x 50)
(nth x 0)

(prime-finder 4 '(3 2))
(check-prime 4 '(3 2))

(first-n (primes) 10)
(nth (primes) 20)