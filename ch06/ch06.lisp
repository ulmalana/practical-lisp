;;;; variables binding and shadowing
(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(dotimes (x 10) (format t "~d " x))
;; 0 1 2 3 4 5 6 7 8 9 
;; NIL

;; not possible with let
;; (let ((x 10)
;;       (y (+ x 10)))
;;   (list x y))

;; should use let*
(let* ((x 10)
       (y (+ x 10)))
  (list x y))

(defparameter *foo-fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

(funcall *foo-fn*)

(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))
;; (#<FUNCTION (LAMBDA ()) {1002D2EF7B}> #<FUNCTION (LAMBDA ()) {1002D2EF9B}> #<FUNCTION (LAMBDA ()) {1002D2EFBB}>)

;;;;; creating dynamic/special/global variables ;;;;;;;
(defvar *count* 0
  "Count of widgets made so far.")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(defvar *x* 10)
(defun foo-x () (format t "X: ~d~%" *x*))

(defun bar-x ()
  (foo-x)
  (let ((*x* 20)) (foo-x))
  (foo-x))

(bar-x)
;; X: 10
;; X: 20
;; X: 10
;; NIL

(defun foo-setf ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

(foo-setf)
;; Before assignment X: 10
;; After assignment  X: 11
;; NIL
