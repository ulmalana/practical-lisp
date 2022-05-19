;;; verbose version of function for summing two numbers
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

;;; this function returns as soon as it finds the pair whose products is greater
;;; then the argument
(defun ret-foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from ret-foo (list i j))))))

;;; this function plots a simple histogram based on its arguments
;;; example: (plot #'exp 0 4 1/2)
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
