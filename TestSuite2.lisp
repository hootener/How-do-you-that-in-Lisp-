;; It is expected that your code has been saved
;; to the file HowDoYouDoThatInLisp.lsp in the root dir of the C: drive. 
;; If the file is saved elsewhere, change the
;; LOAD statement in the main test driver below.

;; 1test down
(defun test1 ()
  (and 
   (equal (down '(a b c d e)) '((a) (b) (c) (d) (e))) ;correct case
   (equal (down '(a (b) (c (d)) e)) '((a) ((b)) ((c (d))) (e))) ;sublist case
   (equal (down nil) nil) ;nil case
   (equal (down '(a b c nil)) '((a) (b) (c) (nil))) ;nil in list case
   (equal (down 'a) '(a)) ;single atom case
))

;; 2test up
(defun test2 ()
  (and 
   (equal (up '(a b c d e)) '(a b c d e)) ;no siblist case
   (equal (up '(a (b) (c (d)) e)) '(a b c (d) e)) ;sublist case
   (equal (up nil) nil) ;nil case
   (equal (up '(a (((b))) (d) nil)) '(a ((b)) d nil)) ;nil in list case
   (equal (up 'a) 'a) ;single atom case
))

;;3test flatten
(defun test3 ()
  (and 
   (equal (flatten '(a b c d e)) '(a b c d e)) ;no sublist case
   (equal (flatten '(a (b) (c (d)) e)) '(a b c d e)) ;sublist case
   (equal (flatten nil) nil) ;nil case
   (equal (flatten '(a (((b))) (d) ((nil)))) '(a b d)) ;nil in list case -- nil is removed
   (equal (flatten 'a) 'a) ;single atom case
   (equal (flatten '(a)) '(a)) ;single element list case
   (equal (flatten '((a))) '(a)) ;single element nested sublist case
))

;;4test my-merge -- note I'm assuming lists of numbers will always be passed in.
(defun test4 ()
  (and
   (equal (my-merge '(1 2 3 4 5) '(2 3 4 5 6)) '(1 2 3 4 5 6)) ;;expected case
   (equal (my-merge nil '(1 2 3 4)) nil) ;;one nil list passed in
   (equal (my-merge nil nil) nil) ;;two nil lists passed in
   (equal (my-merge 1 '(1 2 3 4)) nil) ;;a list and atom are pased in.
   (equal (my-merge '(3 4 6 2 1) '(5 7 2 4 1)) '(1 2 3 4 5 6 7)) ;;unsorted lists passed in.
))


;;5test leftmost
(defun test5 ()
  (and
   (equal (leftmost '(((((a)))) b c d e)) 'a) ;;expected case
   (equal (leftmost '(a b c d e f)) 'a) ;;no sublist case
   (equal (leftmost '(nil a)) nil) ;;nil as first element
   (equal (leftmost 'a) 'a) ;;atom case
))


;;6test remove first
(defun test6 ()
  (and
   (equal (remove-first 'a '(b c d a f a)) '(b c d f a)) ;;expected case
   (equal (remove-first 'b '(a (b c) b d)) '(a (b c) d)) ;;nested sublist
   (equal (remove-first '(c d) '(a b (c d))) '(a b)) ;;sublist removal
   (equal (remove-first 'z '(a b c d)) '(a b c d)) ;;item not in list case
   (equal (remove-first 'a 'c) nil) ;;second parameter is not list case
))

;;7test remove last -- note that testing is a little lax here since remove-last calls the thoroughly tested
;;remove-first
(defun test7 ()
  (and
   (equal (remove-last 'a '(a b c d)) '(b c d)) ;;expected case, last occurance is first
   (equal (remove-last 'c '(a b c d c)) '(a b c d)) ;;another expected case.
   (equal (remove-last 'z '(a b c d)) '(a b c d)) ;;element not in list case
   (equal (remove-last 'a 'c) nil) ;;second parameter isn't list
))

;;8test list-index
(defun test8 ()
  (and
   (equal (list-index 'c '(a b c d)) 2) ;;single atom expected case
   (equal (list-index 'f '(a b c d e)) -1) ;; item not in list
   (equal (list-index '(a b) '(a b c (a b))) 3) ;;list expected case
   (equal (list-index '(a c) '(a b c)) -1) ;;list not in list
   (equal (list-index 'a 'f) nil) ;;second parameter isn't list
))

;;9test count-parens
(defun test9 ()
  (and
   (equal (count-parens '( )) 2) ;;empty list
   (equal (count-parens nil) 2) ;;same as above
   (equal (count-parens 'a) 0) ;;atom case
   (equal (count-parens '((a b) c)) 4) ;;expected case
   (equal (count-parens '(((a ( ) b) c) ( ) ((d) e))) 14) ;;another expected case

))

;;10test pair add
(defun test10 ()
  (and
   (equal (pairadd '(0 1 2 3) '(0 1 2 3)) '(0 2 4 6)) ;expected case
   (equal (pairadd '(0 1 2 3) '(0 1 3)) '(0 2 5)) ;unbalanced lists
   (equal (pairadd '(0 1 2 3) nil) nil) ;nil parameter case
   (equal (pairadd '(0 1 2 3) 1) nil) ;list and atom case
   (equal (pairadd '(0 1 2 3) '(4 5 6 nil 7)) '(4 6 8)) ;nil in list case. Adds elements up to the nil
))

;;11test filter -- note since the user can pass in a predicate here, there are practically limitless test
;;conditions that could cause this function to fail. I've just tested a few.
(defun test11 ()
 (and
  (equal (filter 'plusp '(-2 -1 0 1 2)) '(1 2)) ;;positive numbers
  (equal (filter (function atom) '(a (b c) ((d)) e)) '(a e)) ;;return atoms
  (equal (filter 'numberp '(a 1 b 2 c 3 d 4)) '(1 2 3 4)) ;;return numbers
  (equal (filter nil '(a b c d)) nil) ;;no predicate case
  (equal (filter 'numberp nil) nil) ;;no input list case
))

;;12test my-mapcar
(defun test12 ()
  (and
   (equal (my-mapcar  '1+  '(0 1 2 3)) '(1 2 3 4)) ;;expected case
  (equal (my-mapcar  'evenp  '(0 1 2 3 4)) '(T nil T nil T)) ;;test with predicate
   (equal (my-mapcar nil '(0 1 2)) nil) ;;nil function parameter
   (equal (my-mapcar  '1+  1) nil) ;;atom instead of list case
   (equal (my-mapcar  '1+  nil) nil) ;;null list case
))

;;13test maxint -- doesn't test null conditions since hw specifies list is nonempty. Also assumes entry is 
;;a  list of numbers instead of a list of chars or something weird.
(defun test13 ()
  (and
   (equal (maxint  '(0 1 2 3 2 -1 0)) 3) ;;expected case
   (equal (maxint  '(2)) 2) ;;single entry case
   (equal (maxint '(-2 -4 -6)) -2) ;;negative case
))

;;14test maxintr uses same tests as maxint case
(defun test14 ()
  (and
   (equal (maxintr  '(0 1 2 3 2 -1 0)) 3) ;;expected case
   (equal (maxintr  '(2)) 2) ;;single entry case
   (equal (maxintr '(-2 -4 -6)) -2) ;;negative case
))





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

  (if (test10)
      (format t "~&10 success ")
      (format t "~&10 failure "))

  (if (test11)
      (format t "~&11 success ")
      (format t "~&11 failure "))

  (if (test12)
      (format t "~&12 success ")
      (format t "~&12 failure "))

  (if (test13)
      (format t "~&13 success ")
      (format t "~&13 failure "))

  (if (test14)
      (format t "~&14 success ")
      (format t "~&14 failure "))

  "Done testing"
)
