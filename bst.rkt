;; 4/8/19
;; CS 251
;; Binary Search Trees Assignment
      
( define entry
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car currentList ) ))))


( define left 
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car (cdr  currentList ) ) ))))


( define right 
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car(cdr (cdr currentList ) ) ) ))))

;; helper function for functions entry, left, right. test the structure of the tree.
(define test
  (lambda (bst)
    (cond
      ((equal? bst '())  #f)
      ((not (= 3 (length  bst)) ) #f)
     ( (and (number? (car bst)) (list? (car (cdr  bst ) ))  (list? (car(cdr (cdr bst))) )  ) #t  )
     (else #f))))


(define make-bst
  (lambda (elt lft rght)
    (define tree (list elt left right ))
    (cond
      (( and (test lft) (test rght) (number? elt) ) (list elt lft rght  ) )
      (else  #f))))
    
 
( define preorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the node, the values to the left, and the values to the right
      (else  (append (list (entry currentList))   ( preorder (left currentList ))    ( preorder (right currentList )) )))))


( define inorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the values to the left, the node, and the values to the right
      (else  (append  ( inorder (left currentList ))    (list (entry currentList))    ( inorder (right currentList )) )))))


( define postorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the values to the left, the values to the right, and the node
      (else  (append  ( postorder (left currentList ))   ( postorder (right currentList ))    (list (entry currentList)) )))))


(define (insert v currentList)
  (cond
    ;; if it hits a leaf, insert bst with the value as its only node
    ((equal? currentList '())   (list v '() '()) )
    ((= v (entry currentList))   (list (car currentList)   (car (cdr currentList))    (car (cdr (cdr currentList)))))
    ;;if bigger return a  list of the node, left side of the tree, and call insert on right side
    ((> v (entry currentList))   (list (car currentList)   (car (cdr currentList))   (insert v (car (cdr (cdr currentList))))))
    ;; if smaller return a list of the node, call insert of left side, and the right side of the tree,
    (else (list (car currentList)    (insert v (car (cdr currentList)))   (car (cdr (cdr currentList)))))
  ))

;; BONUS POINTS

(define bst-from-list
  (lambda (lst)
    (bst-from-list-helper (reverse lst) )))

(define bst-from-list-helper
  (lambda (lst)
    (cond
      ( (= 1 (length lst)) '() (list (car lst) '() '() ) )
      (else (insert (car lst)  (bst-from-list-helper (cdr lst)) )))))

;; checks if the number of integers in the list correctly match the number of parentheses,
;; if so it then checks if  the values in a list from an inorder traversal are in accending order. If so the list is an bst.

(define proper-tree? 
  (lambda (bst)
    (cond
     ( (correct-format-bst bst) (acending-order? (inorder-test bst) ) )
     (else #f))))

;; helper function checks if values in list are in accending order
(define (acending-order? bst)
      (cond
        ((null? bst) #t)
        ((eq? (length bst) 1) #t)
        ((> (car (cdr bst)) (car bst)) (acending-order? (cdr bst)))
        (else #f)))

;; returns list of values in bst from an inorder traversel without checking format of bst
( define inorder-test
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the values to the left, the node, and the values to the right
      (else  (append  ( inorder (car (cdr  currentList ) ))    (list (entry currentList))    ( inorder (car(cdr (cdr currentList ) ) ) )) ))))


;; checks if the number of parentheses are correct for the number of intergers in the list
(define correct-format-bst
  (lambda (bst)
    (cond
      ((equal? bst '()) #f)
      ((= (/ (- (count-nulls bst) 1) 2) (count-integers bst)) #t)
      (else #f))))

;; counts the number of parentheses in the bst
(define count-nulls
  ( lambda (bst)
     (cond
       ((null? bst) 1)
       ((integer? bst) 0)
       ( (+ (count-nulls (car bst)) (count-nulls (cdr bst)))))))

;; counts the number of integers in the list
 (define count-integers
  ( lambda (bst)
     (length (remove-par bst ))))

;; turns the bst into a list of integers
(define (remove-par bst)
  (cond ((null? bst) '())
        ((pair? bst) (append (remove-par  (car bst)) (remove-par  (cdr bst))))
        (else (list bst))))

;; -------------- TESTS --------------
( define x  '( 8 (3 (1 () () ) (6 (4 () () ) (7 () ()) ) ) (10 () (14 (13 () ()) () ) ) ) )
(define bad-tree '(8 () ))

(entry x)
(left x)
(right x)

(entry bad-tree)
(left bad-tree)
(right bad-tree)

(make-bst 5  '( 3 () () ) '( 7 () () )  )
(make-bst 5  '( 3 () () ) '(7 () )  )

(preorder x) 
(inorder x)
(postorder x)

x

(insert 43 x)
(bst-from-list '(8 3 1 6 4 7 10 14 13 43))
(bst-from-list '(8 3 1 6 4 7 10 14 13))

(define y '(5 () () ) )
(test y )
(proper-tree? '(10 (11 () ()) ()))
(proper-tree? '(10 (5 () ()) (3 () ())))
(proper-tree? '(10 (5  ()) (3 () ())))
(proper-tree? x)

