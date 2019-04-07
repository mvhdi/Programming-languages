

( define entry
  (lambda (currentList  )
   (car currentList )   
  )
)

( define left 
  (lambda (currentList  )
   
   (car (cdr  currentList ) )
  )
)

( define right 
  (lambda (currentList  )
  
   (car(cdr (cdr currentList ) ) )
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

    ;;if bigger return a  list of the node, left side of the tree, and call insert on right side
    ((> v (entry currentList))   (list (car currentList)   (car (cdr currentList))   (insert v (car (cdr (cdr currentList))))))

    ;; if smaller return a list of the node, call insert of left side, and the right side of the tree,
    (else (list (car currentList)    (insert v (car (cdr currentList)))   (car (cdr (cdr currentList)))))
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
(insert 6 x)

;; 5
;; (3 (12 () ()) (4 () (22 () ())))
;; (43 () ())
;; (5 3 12 4 22 43)
;; (12 3 4 22 5 43)
;; (12 22 4 3 43 5)
;; (5 (3 (12 () ()) (4 () (22 () ()))) (43 () ()))
;; (5 (3 (12 () ()) (4 () (22 () ()))) (43 (6 () ()) ()))