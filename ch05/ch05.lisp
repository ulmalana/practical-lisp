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

;;; optional parameters
(defun foo-optional (a b &optional c d)
  (list a b c d))

(foo-optional 1 2)
;; => (1 2 NIL NIL)
(foo-optional 1 2 3)
;; (1 2 3 NIL)

(foo-optional 1 2 3 4)
;; (1 2 3 4)

(defun foo-optional-def (a &optional (b 10)) (list a b))

(foo-optional-def 1 2)

(foo-optional-def 1)

;;; supplied-p parameters
(defun foo-supplied (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(foo-supplied 1 2)

(foo-supplied 1 2 3)

(foo-supplied 1 2 4)

;;; keyword parameters
(defun foo-key (&key a b c)
  (list a b c))

(foo-key)

(foo-key :a 1 :c 3)

(defun foo-key-mix (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo-key-mix :b 1)

(foo-key-mix :a 2 :b 1 :c 4)

;;; useful for decouple public API and internal details
(defun foo-key-diff (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(foo-key-diff :apple 10 :box 20 :charlie 30)

(defun foo-return-from (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo-return-from (list i j))))))

(foo-return-from 55)

;; get a function object

(defun foo-object (x) (* 2 x))
;; => #<FUNCTION FOO-OBJECT>

(plot #'exp 0 4 1/2)
;; *
;; **
;; ***
;; *****
;; ********
;; *************
;; *********************
;; **********************************
;; *******************************************************
;; NIL

(plot #'(lambda (x) (* 2 x)) 0 10 1)
;;
;; **
;; ****
;; ******
;; ********
;; **********
;; ************
;; **************
;; ****************
;; ******************
;; ********************
;; NIL
