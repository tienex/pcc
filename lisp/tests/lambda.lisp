;;; Lambda expressions in Common LISP
;;; Demonstrates anonymous functions and higher-order functions

;; Simple lambda
(defvar add-one (lambda (x) (+ x 1)))

;; Using lambda with mapcar
(defun apply-to-list (fn lst)
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (apply-to-list fn (cdr lst)))))

;; Test
(print (funcall add-one 5))
(print (apply-to-list (lambda (x) (* x 2)) (list 1 2 3 4 5)))
