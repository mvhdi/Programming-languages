;; 4/14/19
;; CS 251
;; Sieve of Eratosthenes

;; This function takes two integers and returns an integer lazy list containing the sequence of values first, first+1, ... , last.
(define seq
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda ()
              (seq (+ start 1) stop))))))

;; This function takes an integer and returns an integer lazy list containing the infinite sequence of values first,first+1, ...
(define inf-seq
  (lambda (start)
        (cons start
            (lambda ()
              (inf-seq (+ start 1))))))

;; his function takes a lazy list and an integer and returns an ordinary Scheme list containing the first n values in the lazy-list.
(define first-n
  (lambda (lazy-list n)
    (cond
     ((equal? lazy-list #f) '())
     ((> 1 n) '())
     ((= 1 n) (list (car lazy-list)))
     (else (append (list (car lazy-list)) (first-n (call-lazy-list lazy-list) (- n 1)))))))

;; This helper function calls the function in lazy list
(define call-lazy-list
  (lambda (lazy-list)
    ((cdr lazy-list))))

;; This function takes a lazy list and an integer and returns the n-th value in the lazy-list (counting from 1).
(define nth
  (lambda (lazy-list n)
   (cond
    ((< n 1) '())
    ((< (length (reverse (first-n lazy-list n))) n) #f)
    (else (car (reverse (first-n lazy-list n)))))))

;; This helper function takes two values and checks if the first value can be evenly diveded by the second value. 
(define is-multiple
  (lambda (val1)
    (lambda (val2)
    (cond
      ((integer? (/ val1 val2)) #t)
      (else #f)))))

;; This function returns a new lazy list that has n and all integer multiples of n removed from lazy-list.
(define filter-multiples
  (lambda (lazy-list n)
   (lazy-list-from-lst (filter  (is-multiple-2 n ) (return-all lazy-list)))))

;; This helper function is like is-mulitple but swaps true and false
(define is-multiple-2
  (lambda (val1)
    (lambda (val2)
    (cond
      ((integer? (/ val2 val1)) #f)
      (else #t)))))

;; This helper  function turns a lazy-list into a list
(define return-all
  (lambda (lazy-list)
    (cond
      ((equal? lazy-list #f) '())
      (else (append (list (car lazy-list)) (return-all (call-lazy-list lazy-list)))))))

;; This helper function turns a list into a lazy-list
(define lazy-list-from-lst
  (lambda (lst)
    (if (null? lst)
        #f
        (cons (car lst)
            (lambda ()
              (lazy-list-from-lst (cdr lst)))))))

;; This function computes a lazy list containing all prime numbers
(define primes
  (lambda ()
    (primes-helper 2 '() )))

;; This helper function creates a lazy list of prime numbers
(define primes-helper
  (lambda (start lst)
        (cons (car (prime-finder start lst))
            (lambda ()
              (primes-helper (+ (car (prime-finder start lst)) 1) (prime-finder start lst))))))

;; This helper function finds the next prime number
(define prime-finder
  (lambda (start lst)
    (cond
      ((check-prime start lst) (append (list start) lst))
      (else (prime-finder (+ start 1) lst)))))

;; This function checks if a value is prime by checking if it is divisiable by any prime integer less than it.
(define check-prime
  (lambda (val lst)
    (cond
      ((null? (filter (is-multiple val) lst)) #t)
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
(filter-multiples x 2)
(filter-multiples x 3)
(first-n (filter-multiples x 2) 5)