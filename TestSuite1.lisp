;; It is expected that your code has been saved
;; to the file HowDoYouDoThatInLisp.lsp in the root dir of the C: drive. 
;; If the file is saved elsewhere, change the
;; LOAD statement in the main test driver below.


;; test cube volume
(defun test1 ()
  (and 
   (equal (cube-volume 0 0 0) 0) ;zero case
   (equal (cube-volume 1 1 1) 1) ;one case
   (equal (cube-volume 2 2 2) 8) ;correct eval case
   (equal (cube-volume 'a 'b 'c) nil) ;three nonnumber case
   (equal (cube-volume '(a b) '(b c) '(d e)) nil) ;three list of nonnumber case
   (equal (cube-volume '(4) '(4) '(2)) nil) ;three list of number case
))



;; test euclid
(defun test2 ()
  (and
   (or (equal (euclid '(0 0) '(3 4)) 5) ; working case
       (equal (euclid '(0 0) '(3 4)) 5.0)) ;working case --float
   (or (equal (euclid '(0 0) '(0 0)) 0) ;zero case
       (equal (euclid '(0 0) '(0 0)) 0.0)) ;zero case -- float
   (equal (euclid '(a b) '(c d)) nil) ;nonnumber case
   (equal (euclid '(0 0 3) '(0 0 2)) nil) ;too many parameters
   (equal (euclid '2 '4) nil) ;too few parameters
))
  


;; test path-distance
(defun test3 ()
  (and
   (or (equal (path-distance '((0 0) (0 0))) 0) ; zero case 
       (equal (path-distance '((0 0) (0 0))) 0.0)) ;zero case -- float
   (or (equal (path-distance '((0 0) (3 4))) 5)  ;working case, two inputs
       (equal (path-distance '((0 0) (3 4))) 5.0)) ;working case, two inputs -- float
   (equal (path-distance '((0 0) (3 4) (6 8))) 10) ;working case, multiple inputs
   (equal (path-distance '((0 0) (b 4))) nil) ;nonnumber entry in list
   (equal (path-distance '((3 4))) 0) ;single enttry case.
   (equal (path-distance '(3 4)) nil) ;list of atoms case.
   (equal (path-distance 'b) nil) ;single nonnumber case.
   (equal (path-distance 3) nil) ;single number case 
))



;; test duple
(defun test4 ()
  (and
   (equal (duple 1 'hello) '(hello)) ;working case single atom
   (equal (duple 3 'hello) '(hello hello hello)) ;working case multple atoms
   (equal (duple 0 'hello) nil) ;n = 0 case
   (equal (duple 2 '(this (is) a ((list)))) '((THIS (IS) A ((LIST))) (THIS (IS) A ((LIST))))) ;list with sublists
   (equal (duple 2 ()) '(nil nil)) ; null list case -- duplicates nil. Bad thing?
))



;; test my-member
(defun test5 ()
  (and
   (equal (my-member 'a '(a b c)) '(a b c)) ;in list
   (equal (my-member 'a '(c b a b c)) '(a b c)) ;in list
   (equal (my-member nil '(a b c)) nil) ;nil not in set
   (equal (my-member 'a ()) nil) ; compare against null list
   (equal (my-member 'c '(a b d e f)) nil) ;not in set
   (equal (my-member 'a '((a) b c d)) nil) ;in sublist
   (equal (my-member '(b d) '((f) (b d) a (((cg))))) '((b d) a (((cg))))) ;member is list not at beginning
))



;; test get-nth
(defun test6 ()
  (and
   (equal (get-nth 0 '(b a e)) 'b) ;item in range
   (equal (get-nth 2 '(w x y z)) 'y) ;item in range
   (equal (get-nth 5 '(a b c d)) nil) ;item not in range
   (equal (get-nth 2 '(a b (c d) e)) '(c d)) ;item in range is list
   (equal (get-nth -2 '(a b c)) nil) ;pass negative number

))


;; test set-nth
(defun test7 ()
  (let ((lst '(a b c)))
    (and
     (equal (set-nth 0 lst 1) '(1 b c)) ;value in range
     (equal (set-nth 2 lst 2) '(a b 2)) ;value in range
     (equal (set-nth 4 lst 3) '(a b c)) ;value out of range
     (equal (set-nth -8 lst 3) '(a b c)) ;value out of range negative
     (equal (set-nth 1 lst lst) '(a (a b c) c)) ;value in range replaced with lst
     
)))


;; test get-matrix-value
(defun test8 ()
  (let
      ((matrix '((a b c) (d e f) (g h i) (j k l))))
    (and
     (equal (get-matrix-value matrix 0 0) 'a) ;the following five functions just grab a value from every row.
     (equal (get-matrix-value matrix 0 1) 'b)
     (equal (get-matrix-value matrix 1 1) 'e)
     (equal (get-matrix-value matrix 2 1) 'h)
     (equal (get-matrix-value matrix 3 2) 'l)
     (equal (get-matrix-value matrix 4 4) nil) ;both row and col out of range
     (equal (get-matrix-value matrix 4 2) nil) ;row out of range
     (equal (get-matrix-value matrix 3 6) nil) ;col out of range
)))


;; test set-matrix-value
(defun test9 ()
  (let
      ((matrix '((a b c) (d e f) (g h i) (j k l))))
    (and
     (equal (set-matrix-value matrix 0 0 99) '((99 b c) (d e f) (g h i) (j k l))) ;the following four lines just set a value in each row.
     (equal (set-matrix-value matrix 1 0 99) '((a b c) (99 e f) (g h i) (j k l)))
     (equal (set-matrix-value matrix 2 0 99) '((a b c) (d e f) (99 h i) (j k l)))
     (equal (set-matrix-value matrix 3 0 99) '((a b c) (d e f) (g h i) (99 k l)))
     (equal (set-matrix-value matrix 0 0 '(a b c)) '(((a b c) b c) (d e f) (g h i) (j k l))) ;set with a list
     (equal (set-matrix-value matrix 5 5 '(a b c)) '((a b c) (d e f) (g h i) (j k l))) ; row and col out of range
     (equal (set-matrix-value matrix 5 0 '(a b c)) '((a b c) (d e f) (g h i) (j k l))) ;row out of range
     (equal (set-matrix-value matrix 0 5 '(a b c)) '((a b c) (d e f) (g h i) (j k l))) ;col out of range 
       
)))

     



;;;;;;;;;;;;;;;;;;;
;;
;; main test script
;;
;;;;;;;;;;;;;;;;;;;


(defun test-all ()

  (format t "~& ")
  (format t "~&TESTING")
  (format t "~& ")

  ;; load code from HowDoYouDoThatInLisp.lsp in the root dir of the C: drive.
  ;; change this if the code is located elsewhere.
  ;; or comment this line out and load the file manually.
  (load "path/to/HowDoYouDoThatInLisp.lisp")

  (format t "~& ")

  (if (test1)
      (format t "~&1 success ")
      (format t "~&1 failure "))

  (if (test2)
      (format t "~&2 success ")
      (format t "~&2 failure "))

  (if (test3)
      (format t "~&3 success ")
      (format t "~&3 failure "))

  (if (test4)
      (format t "~&4 success ")
      (format t "~&4 failure "))

  (if (test5)
      (format t "~&5 success ")
      (format t "~&5 failure "))

  (if (test6)
      (format t "~&6 success ")
      (format t "~&6 failure "))

  (if (test7)
      (format t "~&7 success ")
      (format t "~&7 failure "))

  (if (test8)
      (format t "~&8 success ")
      (format t "~&8 failure "))

  (if (test9)
      (format t "~&9 success ")
      (format t "~&9 failure "))

  "Done testing"
)

