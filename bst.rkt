

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
      (else  (append (list (entry currentList)) ( preorder (left currentList )) ( preorder (right currentList )) ))
     )  
  )
)


( define inorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      (else  (append  ( inorder (left currentList )) (list (entry currentList)) ( inorder (right currentList )) ))
     )  
  )
)


( define postorder
  (lambda (currentList  )
    (cond
      ((null? currentList) '() )
      (else  (append  ( postorder (left currentList )) ( postorder (right currentList ))  (list (entry currentList)) ))
     )  
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

 
;; 5
;; (3 (12 () ()) (4 () (22 () ())))
;; (43 () ())
;; (5 3 12 4 22 43)
;; (12 3 4 22 5 43)
;; (12 22 4 3 43 5)
