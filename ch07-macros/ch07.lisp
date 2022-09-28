;;;; chapter 07 - macros

(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(dolist (x '(1 2 3)) (print x))
;; 1
;; 2
;; 3

(dolist (x '(1 2 3))
  (print x)
  (if (evenp x)
      (return)))
;; 1
;; 2

(dotimes (i 4)
  (print i))
;; 0 
;; 1 
;; 2 
;; 3 

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10))
  (push i nums))
;; (1 2 3 4 5 6 7 8 9 10)

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))
;; (1 2 3 4 5 6 7 8 9 10)

(loop for i from 1 to 10 collecting i)
;; (1 2 3 4 5 6 7 8 9 10)

;;;;;;;;; extended loop ;;;;;;;;;;;;;;;;
(loop for x from 1 to 10 summing (expt x 2))
;; 385

(loop for x across "the quick brown fox jumps over the lazy dog"
               counting (find x "aiueo"))
;; 11

(loop for i below 10
               and a = 0 then b
               and b = 1 then (+ b a)
               finally (return a))
;; 55
