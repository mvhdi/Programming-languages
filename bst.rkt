(define test
  (lambda (bst)
    (cond
      ((equal? bst '())  #f)
      

     ;; ((equal?( (equal? (number? (car bst)) '(#t)) '(#t))) #t) (equal? (list?  (car (cdr  bst ))) '(#t) ) (equal? (list? (car(cdr (cdr bst))) ) '(#t) ) '(#t)) (#t) )
      (       (not (= 3 (length  bst)) )  #f     )
      
     ;; (  (and  (> (car bst)  (car(car(cdr (cdr bst ) ) ) )  )    (< (car bst) (car (car (cdr  bst ) )) )    )      #f) possible checking if bst if correctly formated

    
     ( (and (number? (car bst)) (list? (car (cdr  bst ) ))  (list? (car(cdr (cdr bst))) )  ) #t  )
     ;;((list? bst) #t)
     (else #f)
     )
    )
  )
      
       
      

( define entry
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car currentList ) )
     )
   )
 )


( define left 
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car (cdr  currentList ) ) )
    )   
  )
)


( define right 
  (lambda (currentList  )
    (cond
      ( (not (test currentList ) ) #f)
      (else (car(cdr (cdr currentList ) ) ) )
    )
  )
)


(define make-bst
  (lambda (elt left right)
    (define tree (list elt left right ))
    (cond
      ((test (list elt left right ) ) (list elt left right  ))
      (else  #f)
    )
  )
)
    









 
( define preorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the node, the values to the left, and the values to the right
      (else  (append (list (entry currentList))   ( preorder (left currentList ))    ( preorder (right currentList )) ))
     )  
  )
)


( define inorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the values to the left, the node, and the values to the right
      (else  (append  ( inorder (left currentList ))    (list (entry currentList))    ( inorder (right currentList )) ))
     )  
  )
)


( define postorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      ;; for each node: it adds the values to the left, the values to the right, and the node
      (else  (append  ( postorder (left currentList ))   ( postorder (right currentList ))    (list (entry currentList)) ))
     )  
  )
)


(define (insert v currentList)
  (cond
    
    ;; if it hits a leaf, insert bst with the value as its only node
    ((equal? currentList '())   (list v '() '()) )

    ((= v (entry currentList))   (list (car currentList)   (car (cdr currentList))    (car (cdr (cdr currentList)))))
    
    ;;if bigger return a  list of the node, left side of the tree, and call insert on right side
    ((> v (entry currentList))   (list (car currentList)   (car (cdr currentList))   (insert v (car (cdr (cdr currentList))))))

    ;; if smaller return a list of the node, call insert of left side, and the right side of the tree,
    (else (list (car currentList)    (insert v (car (cdr currentList)))   (car (cdr (cdr currentList)))))
  )  
)



(define bst-from-list
  (lambda(lst)
    ;(define curentList '())
    (if (null? lst) currentList)
    (insert car lst)
    (bst-from-list (cdr lst))
   )
 )




( define x
   '(
  5
  (3 (12 () ()) (4 () (22 () () )) )
  (43 () () )
  
 ))

(entry x)
(left x)
(right x)
(preorder x) 
(inorder x)
(postorder x)
x
(insert 43 x)

;; 5
;; (3 (12 () ()) (4 () (22 () ())))
;; (43 () ())
;; (5 3 12 4 22 43)
;; (12 3 4 22 5 43)
;; (12 22 4 3 43 5)
;; (5 (3 (12 () ()) (4 () (22 () ()))) (43 () ()))
;; (5 (3 (12 () ()) (4 () (22 () ()))) (43 (6 () ()) ()))



(define y '(5 () () ) )
(test y )
(make-bst 5  '( 3 () () ) '( 7 () () )  )

(bst-from-list '(1 2 3 4 5))
