;;; Fibonacci sequence in Common LISP
;;; Demonstrates recursion with multiple recursive calls

(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(print (fibonacci 0))
(print (fibonacci 1))
(print (fibonacci 5))
(print (fibonacci 10))
