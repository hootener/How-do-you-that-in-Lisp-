;;; Beginner

;; Maths
; Checks if item a or b is null before adding. If either is null, nil is returned.
(defun my-null-add (a b)
  (cond
   ((or (null a) (null b)) nil)
   (T (+ a b)
   )
  )
)

; Squares a number
(defun my-sqr (x)
  (cond
   ((null x) nil)
   ((listp x) nil)
   ((not (numberp x)) nil)
   (T (* x x))
   )
)

; Calculate a Factorial - use recursion
(defun fact (n)
  (cond
   ((not (numberp n)) nil)
   ((zerop n) 1)
   ((= n 1) 1)  ; optional base case
   (T (* n (fact (- n 1))))))


; Calculate a Factorial - use iteration
(defun facti (n)
  (if (not (numberp n)) nil
    (do ( (i n (- i 1))   ; or (1- i)
	  (prod 1 (* prod i))
	)
	((zerop i) prod)
)))

;; Lists
; Reverse a list -- recursive verion
(defun rreverser (lst)
  (if (null lst)
      lst
    (append (rreverser (cdr lst)) (list (car lst)))))

; Reverse a list -- iterative version
(defun reversei (lst)
  (do
      ((lst2 lst (cdr lst2))
       (lst3 () (cons (car lst2) lst3)))
      ((null lst2) lst3)
    ;; empty body
))

; Reverses a list --deep recursive
(defun reverser (lst)
  (cond
   ((null lst) nil)
   ((listp (car lst)) (append (reverser (cdr lst)) (list (reverser (car lst)))))
   (T (append (reverser (cdr lst)) (list (car lst))))
  )
)

; Calculate the length of a list -- recursive version
(defun lengthr (lst)
  (if (null lst)
      0
    (1+ (lengthr (cdr lst)))))

; Calculate the length of a list -- iterative version
(defun lengthi (lst)
  (do
      ((lst2 lst (cdr lst2))
       (count 0 (1+ count)))
      ((null lst2) count)))


; Determines if a member is part of a list
(defun my-member (item lst)
  (cond 
   ((null item) nil)
   ((null lst) nil)
   (T
    (if (equal item (car lst)) lst (my-member item (cdr lst)))
    )
   )
)

; Gets the nth item in the list. Returns nil if list has no nth item
(defun get-nth(n lst)
  (cond
   ((or (null n) (null lst)) nil) ;returns nil if either parameter is null
   ((< n 0) nil)
   (T (if (equal n 0) (car lst) (get-nth (- n 1) (cdr lst)))
   )
  )
)

; Duplicates a suplied item n times.
(defun duple (n item)
  (cond
   ((null n) nil)
   ((equal n 0) nil) ;return nil if n is equal to zero
   (T (cons item (duple (- n 1) item))
   )
  )
)

;set the nth item of the list with value. Return the unedited list if the list does not contain an nth item.
(defun set-nth (n lst value)
  (cond
   ((or (null n) (< n 0)) lst)
   ((null lst) nil)
   ((equal n 0) (cons value (cdr lst)))
   (T (cons (car lst) (set-nth (- n 1) (cdr lst) value))
   )
  )
)

;;; Less Beginner

;; Quoting

; Determines whether x is within some tolerance of y. Uses the absolute value of tolerance
; Tolerance is quoted
(setq tolerance 0.000005)
(defun my-tolerance (x y tolerance)
  (if (and (<= x (+ y (abs tolerance))) (>= x (- y (abs tolerance)))) T nil)
)
 
;; Maths

;calculates the volume of a cube if it is safe to do so (i.e., all parameters are numbers)
(defun cube-volume (x y z)
  (if (and (and (numberp x) (numberp y)) (numberp z)) (* x y z) nil)
)

;returns the euclidean distance between two points. uses cond to check for the proper input.
;returns nil if the output is improper.
(defun euclid (lst1 lst2)
  (cond
   ((or (null lst1) (null lst2)) nil) ;return nil if either list is null
   ((or (atom lst1) (atom lst2)) nil) ;return nil if either list is an atom
   ((or (null (cdr lst1)) (null (cdr lst2))) nil);return nil if either list contains less than two inputs
   ((or (not (null (car(cdr(cdr lst1))))) (not (null (car(cdr(cdr lst2)))))) nil) ;return nil if either list contains more than two inputs
   ((or (or (not (numberp (car lst1))) (not (numberp (cadr lst1)))) (or (not (numberp (car lst2))) (not (numberp (cadr lst2))))) nil);return nil if any of the four entires aren't numbers
   (T (sqrt (+ (my-sqr (- (car lst1) (car lst2))) (my-sqr (- (cadr lst1) (cadr lst2)))))) ;we're all good if we make it here. Calculate the euclidean distance
))

; Calculates the path distance for a list of (x,y) pairs. Returns nil if any of the pairs are improper. Returns 0 if the list is empty
(defun path-distance (lst)
  (cond
   ((null lst) 0)
   ((atom lst) nil)
   (T (if (or (null (car lst)) (null (cadr lst))) 0 (my-null-add (euclid (car lst) (cadr lst)) (path-distance (cdr lst))))
   )
  ) 
)

;; Matrices

; Takes a two dimensional matrix and returns the col-th item from the row-th list. Return nil if row or column are out of range for matrix.
(defun get-matrix-value (matrix row col)
  (cond
   ((or (atom matrix) (not (numberp row)) (not (numberp col))))
   ((or (null matrix) (null row) (null col)) nil)
   (T (get-nth col (get-nth row matrix))
   )
  )
)

;returns a matrix that is the same as the passed matrix parameter except that the col-th item of the row has been replaced with value.
;if the row or column are out of range, return the original matrix.
(defun set-matrix-value (matrix row col value)
  (cond
   ((or (null matrix) (null row) (null col)) nil) ;basic check to make sure all the passed parameters are legit.
   ((null (get-matrix-value matrix row col)) matrix) ;will return matrix if the row or column doesn't exist.
   ((equal row 0) (cons (set-nth col (get-nth row matrix) value) (cdr matrix))) 
   (T (cons (car matrix) (set-matrix-value (cdr matrix) (- row 1) col value))
   )
  )
)

;; Ribcage Style Lookup Example

(setq my-rib '(((a b c d) 1 2 3 4) ((q w e) 5 6 7) ((a s d) 11 12 13)))
;; or
;;(setq my-rib '(((a b c d).(1 2 3 4)) ((q w e).(5 6 7)) ((a s d).(11 12 13))))
(defun ribcage-lookup (sym ribcage)
  (do
      ((ribs ribcage (cdr ribs)))
      ((null ribs) nil)
    (do
	((left (caar ribs) (cdr left))
	 (right (cdar ribs) (cdr right)))
	((or (null left) (null right)) nil)
      (if (my-equal sym (car left))
	  (return-from ribcage-lookup (car right))))))

;; define our own equal predicate
(defun my-equal (a b)
  (cond 
   ((eql a b) T)
   ((or (atom a) (atom b)) nil)
   (T (and (my-equal (car a) (car b))
	   (my-equal (cdr a) (cdr b))))))


;;; Intermediate

;; Lists

;down wraps each top level entry in a list within parentheses.
(defun down (lst)
  (cond
   ((null lst) nil)
   ((atom lst) (list lst))
   (T (append (cons (list(car lst)) (down (cdr lst))))
   )
  )
)

;up removes the parentheses from a top level item unless it is already an atom.
(defun up (lst)
  (cond
   ((null lst) nil)
   ((atom lst) lst)
   ((and (not (null (car lst))) (atom (car lst))) (cons (car lst) (up (cdr lst))))
   (T (append (car lst) (up (cdr lst))))
   )
 )

;flatten makes all entries in a list (including nesting atoms in sub lists) top level items.
;If a nil entry is in the list, it is removed.
(defun flatten (lst)
  (cond
   ((null lst) nil)
   ((atom lst) lst)
   ((null (car lst)) (flatten (cdr lst)))
   ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
   (T (append (flatten (car lst)) (flatten (cdr lst)))
   )
  )
)

;my-merge merges two sorted lists of numbers. I went ahead and used the sort function here
;so technically the individual lists do not have to be sorted. This and remove-duplicates
;allowed me to do all the work in one line. 
;if either lon is an atom nil is returned
;assumes numbers will be passed in in each list, so there is no checking for sublists,
;characters or anything else that might be weird.
(defun my-merge (lon1 lon2)
  (cond
   ((or (null lon1) (null lon2)) nil)
   ((or (atom lon1) (atom lon2)) nil)
   (T (remove-duplicates (sort (append lon1 lon2) #'<))
   )
  )
)

;leftmost returns the leftmost atomic element in a list. 
(defun leftmost (lst)
  (cond
   ((null lst) nil)
   ((atom lst) lst)
   ((atom (car lst)) (car lst))
   ((listp (car lst)) (leftmost (car lst)))
   (T (leftmost (cdr lst))
   )
  )
 )

;rightmost returns the rightmost atomic element in a list.
(defun rightmost(lst)
  (cond
   ((null lst) nil)
   (T (leftmost (reverse lst)))))

;remove first removes the first occurance of an item in a list.
(defun remove-first (item lst)
  (cond
   ((or (null item) (null lst)) nil)
   ((atom lst) nil)
   ((equal item (car lst)) (cdr lst))
   (T (cons (car lst) (remove-first item (cdr lst)))
   )
  )
)

;remove-last removes the last occurance of an item in a list.
(defun remove-last (item lst)
  (cond
   ((or (null item) (null lst)) nil)
   ((atom lst) nil)
   (T (reverse (remove-first item (reverse lst)))
   )
  )
)

;index-count is a helper for list-index. Returns the index of the first occurance of a top level item in a list
;if the item is not in the list, nil is returned
(defun index-count (item lst)
  (cond
   ((or (null item) (null lst)) nil)
   ((atom lst) nil)
   ((equal item (car lst)) 0)
   (T (my-null-add 1 (index-count item (cdr lst)))
   )
  )
)

;list-index calls index-count to determine item's position in the list. If list-index
;returns nil, -1 is returned from this function.
(defun list-index (item lst)
  (cond
   ((atom lst) nil)
   ((null (index-count item lst)) -1)
   (T (index-count item lst)
   )
  )
)

;count-parens counts all of the parenthesis in a list
(defun count-parens(lst)
  (cond
   ((null lst) 2) ;;since every list will eventually be null, the addition is done here.
   ((atom lst) 0)
   ((listp (car lst)) (+ (count-parens (car lst)) (count-parens (cdr lst))))
   (T (count-parens (cdr lst)))
  )
)

;pair-add adds corresponding elements from two lists of numbers. Addition stops when either
;list is empty. 
;Also, if there is a nil in either lon1 or lon2, this function will return the summed
;members up to that point.

(defun pairadd (lon1 lon2)
  (cond
   ((or (atom lon1) (atom lon2)) nil)
   ((or (null (car lon1)) (null (car lon2))) nil)
   (T (cons (+ (car lon1) (car lon2)) (pairadd (cdr lon1) (cdr lon2)))
   )
  )
)

;my-remv-dups removes duplicate items from a list, but leaves the last occurance of the item.
(defun my-remv-dups (lst)
  (cond
   ((null lst) nil)
   ((my-member (car lst) (cdr lst)) (my-remv-dups (cdr lst)))
   (T (cons (car lst) (my-remv-dups (cdr lst))))
  )
)


;; Function passing

;filter returns all the members in a list that satisfy a supplied predicate
(defun filter (pred lst)
  (cond
   ((null pred) nil)
   ((null lst) nil)
   ((atom lst) nil)
   ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
   (T (filter pred (cdr lst))
   )
  )
)

;my-mapcar performs some function on every element in a list
(defun my-mapcar (func lst)
  (cond
   ((null func) nil)
   ((null lst) nil)
   ((atom lst) nil)
   ((null (car lst)) nil)
   (T (cons (funcall func (car lst)) (my-mapcar func (cdr lst)))
   )
  )
)

;maxint determines the largest number from a list of numbers. 
(defun maxint (lon)                      
  (cond 
   ((= (length lon) 1) (car lon)) 
   ((> (car lon) (cadr lon)) (maxint (cons (car lon) (cddr lon))))           
   (T (maxint (cdr lon))
   )
  )
)
        
;maxintr does the same as maxint but uses reduce and the bult in max funciton
(defun maxintr (lon)
  (cond
   ((null lon) nil)
   ((atom lon) nil)
   (T (reduce 'max lon))))
   
;determines if a member is part of a list
(defun my-member (item lst)
  (cond 
   ((null item) nil)
   ((null lst) nil)
   (T
    (if (equal item (car lst)) lst (my-member item (cdr lst)))
    )
   )
)  

