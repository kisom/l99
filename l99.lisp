;;;; L-99: Ninety-Nine Lisp Problems
;;;; Based on a Prolog problem list by werner.hett@hti.bfh.ch
;;;;     Source: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;;;;
;;;; Author: K. Isom

;;; Problem 1
;;; (*) Find the last box of a list.
;;;     Example:
;;;     * (my-last '(a b c d))
;;;     (D)
(defun p01 (lst)
  "Find the last box of a list. Solved: 2015-08-20."
  (cond
    ((null lst) nil)
    ((null (rest lst)) (car lst))
    (t (p01 (rest lst)))))
      

;;; Problem 2
;;; (*) Find the last but one box of a list.
;;;     Example:
;;;     * (my-but-last '(a b c d))
;;;     (C D)


;;; Problem 3
;;; (*) Find the K'th element of a list.
;;;    The first element in the list is number 1.
;;;    Example:
;;;    * (element-at '(a b c d e) 3)
;;;    C
(defun p03 (lst k)
  (let ((k (- k 1)))
    (cond
      ((null lst) nil)
      ((zerop k)  (if (null lst)
		      nil
		      (first lst)))
      (t (p03 (rest lst) k)))))

;;; Problem 4
;;; (*) Find the number of elements of a list.
(defun p04 (lst)
  "Find the number of elements of a list."
  (labels ((internal-length (lst n)
	     (if (null lst)
		 n
		 (internal-length (rest lst) (+ n 1)))))
    (internal-length lst 0)))

;;; Problem 5
;;; (*) Reverse a list.
(defun p05 (lst)
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (rest lst)
		      (cons (first lst) acc)))))
    (rev lst nil)))

;;; Problem 6
;;; (*) Find out whether a list is a palindrome.
;;;    A palindrome can be read forward or backward; e.g. (x a m a x).

;;; Problem 7
;;; (**) Flatten a nested list structure.
;;;    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;;; 
;;;    Example:
;;;    * (my-flatten '(a (b (c d) e)))
;;;    (A B C D E)
;;; 
;;;    Hint: Use the predefined functions list and append.

;;; Problem 8
;;; (**) Eliminate consecutive duplicates of list elements.
;;;    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;;; 
;;;    Example:
;;;    * (compress '(a a a a b c c a a d e e e e))
;;;    (A B C A D E)

;;; Problem 9
;;; (**) Pack consecutive duplicates of list elements into sublists.
;;;    If a list contains repeated elements they should be placed in separate sublists.
;;; 
;;;    Example:
;;;    * (pack '(a a a a b c c a a d e e e e))
;;;    ((A A A A) (B) (C C) (A A) (D) (E E E E))

;;; Problem 10
;;; (*) Run-length encoding of a list.
;;;    Use the result of problem P09 to implement the so-called
;;;    run-length encoding data compression method. Consecutive
;;;    duplicates of elements are encoded as lists (N E) where N is
;;;    the number of duplicates of the element E.
;;; 
;;;    Example:
;;;    * (encode '(a a a a b c c a a d e e e e))
;;;    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

;;; Problem 11
;;; (*) Modified run-length encoding.
;;;    Modify the result of problem P10 in such a way that if an
;;;    element has no duplicates it is simply copied into the result
;;;    list. Only elements with duplicates are transferred as (N E)
;;;    lists.
;;; 
;;;    Example:
;;;    * (encode-modified '(a a a a b c c a a d e e e e))
;;;    ((4 A) B (2 C) (2 A) D (4 E))

;;; Problem 12
;;; (**) Decode a run-length encoded list.
;;;    Given a run-length code list generated as specified in problem
;;;    P11. Construct its uncompressed version.

;;; Problem 13
;;; (**) Run-length encoding of a list (direct solution).
;;;    Implement the so-called run-length encoding data compression
;;;    method directly. I.e. don't explicitly create the sublists
;;;    containing the duplicates, as in problem P09, but only count
;;;    them. As in problem P11, simplify the result list by replacing
;;;    the singleton lists (1 X) by X.
;;; 
;;;    Example:
;;;    * (encode-direct '(a a a a b c c a a d e e e e))
;;;    ((4 A) B (2 C) (2 A) D (4 E))

;;; Problem 14
;;; (*) Duplicate the elements of a list.
;;;    Example:
;;;    * (dupli '(a b c c d))
;;;    (A A B B C C C C D D)

;;; Problem 15
;;; (**) Replicate the elements of a list a given number of times.
;;;    Example:
;;;    * (repli '(a b c) 3)
;;;    (A A A B B B C C C)

;;; Problem 16
;;; (**) Drop every N'th element from a list.
;;;    Example:
;;;    * (drop '(a b c d e f g h i k) 3)
;;;    (A B D E G H K)

;;; Problem 17
;;; (*) Split a list into two parts; the length of the first part is
;;;    given.
;;;    Do not use any predefined predicates.
;;; 
;;;    Example:
;;;    * (split '(a b c d e f g h i k) 3)
;;;    ( (A B C) (D E F G H I K))

;;; Problem 18
;;; (**) Extract a slice from a list.
;;;    Given two indices, I and K, the slice is the list containing
;;;    the elements between the I'th and K'th element of the original
;;;    list (both limits included). Start counting the elements with
;;;    1.
;;; 
;;;    Example:
;;;    * (slice '(a b c d e f g h i k) 3 7)
;;;    (C D E F G)

;;; Problem 19
;;; (**) Rotate a list N places to the left.
;;;    Examples:
;;;    * (rotate '(a b c d e f g h) 3)
;;;    (D E F G H A B C)
;;; 
;;;    * (rotate '(a b c d e f g h) -2)
;;;    (G H A B C D E F)
;;; 
;;;    Hint: Use the predefined functions length and append, as well
;;;    as the result of problem P17.

;;; Problem 20
;;; (*) Remove the K'th element from a list.
;;;    Example:
;;;    * (remove-at '(a b c d) 2)
;;;    (A C D)
