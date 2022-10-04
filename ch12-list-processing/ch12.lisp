(cons 1 2)
;; (1 . 2)

(car (cons 1 2))
;; 1

(cdr (cons 1 2))
;; 2

(defparameter *cons* (cons 1 2))
;; *CONS*

*cons*
;; (1 . 2)

(setf (car *cons*) 10)
;; 10

(setf (cdr *cons*) 30)
;; 30

*cons*
;; (10 . 30)

(cons 1 nil)
;; (1)

(cons 1 (cons 2 nil))
;; (1 2)

(cons 1 (cons 2 (cons 3 nil)))
;; (1 2 3)

(defparameter *list* (list 1 2 3 4))
;; *LIST*

(first *list*)
;; 1

(rest *list*)
;; (2 3 4)

(first (rest *list*))
;; 2

(list "foo" (list 4 5) 10)
;; ("foo" (4 5) 10)

;;; destructive operations
(append (list 1 2) (list 3 4))
;; (1 2 3 4)

(defparameter *list-1* (list 1 2))
;; *LIST-1*

(defparameter *list-2* (list 3 4))
;; *LIST-2*

(defparameter *list-3* (append *list-1* *list-2*))
;; *LIST-3*

*list-1*
;; (1 2)

*list-2*
;; (3 4)

*list-3*
;; (1 2 3 4)

(setf (first *list-2*) 0)
;; 0

*list-2*
;; (0 4)

*list-3*
;; (1 2 0 4) ;; *list-2* is a shared structure


(defparameter *x* (list 1 2 3))
;; *X*

;; nconc is non-consing version of append
(nconc *x* (list 4 5 6))
;; (1 2 3 4 5 6)

*x*
(1 2 3 4 5 6)
