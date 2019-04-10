;; 4/9/19
;; CS 251
;; Lazy Lists

(define gen-list
  (lambda (start stop)
    (cond
      ((> start stop) '() )
      ((= start stop) (list stop))
      (else (append (list start) (gen-list (+ 1 start) stop))))))

(define pair-sum?
  (lambda (lst val)
    (cond
      ((null? lst) #f)
      ((= 1 (length lst)) #f)
      (else (or (= val (+ (car lst) (car(cdr lst)))) (pair-sum? (cdr lst) val))))))


(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda ()
              (gen-lazy-list (+ start 1) stop))))))


(define pair-sum-lazy?
  (lambda (lazy-list val)
    (cond
      ((equal? lazy-list #f) #f)
      ((> (car lazy-list ) val) #f)
      (else (pair-sum-lazy-helper (car lazy-list) val (call-lazy-list lazy-list )))))) 



(define pair-sum-lazy-helper
  (lambda (start target lst)
    (cond
      ((equal? lst #f) #f)
      (else (or
               (and (>= (- target (car lst)) start ) (= (- (car lst) 1 ) (- target (car lst)) ) (< (- target (car lst)) (car lst)))
               (pair-sum-lazy-helper start target (call-lazy-list lst)))))))


(define call-lazy-list
  (lambda (lazy-list)
    ((cdr lazy-list))))


;; Bonus

(define make-lazy
  (lambda (lst)
    (gen-lazy-list (apply min lst) (apply max lst))))


(define any-sum-lazy?
  (lambda (lazy-list val)
    (cond
      ((equal? lazy-list #f) #f)
      ((> (car lazy-list ) val) #f)
      (else (any-sum-lazy-helper (car lazy-list) val (call-lazy-list lazy-list ))))))

(define any-sum-lazy-helper
  (lambda (start target lst)
    (cond
      ((equal? lst #f) #f)
      (else (or
               (and (>= (- target (car lst)) start ) (< (- target (car lst)) (car lst)))
               (any-sum-lazy-helper start target (call-lazy-list lst)))))))




