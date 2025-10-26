;;; List operations in Common LISP
;;; Demonstrates list manipulation and recursion

(defun sum-list (lst)
  (if (null lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(defun length-list (lst)
  (if (null lst)
      0
      (+ 1 (length-list (cdr lst)))))

(defun reverse-list (lst)
  (if (null lst)
      nil
      (append (reverse-list (cdr lst))
              (list (car lst)))))

;; Test the functions
(defvar my-list (list 1 2 3 4 5))

(print (sum-list my-list))
(print (length-list my-list))
(print (reverse-list my-list))
