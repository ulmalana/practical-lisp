;;;; Chapter 11 - COLLECTIONS

;;; Vector
;; - vector literals #(...), as opposed to Clojure [...]
;; - can be fixed size or resizable(vector)

;; fixed size
(vector)
;; #()

(vector 1)
;; #(1)

(vector 1 2)
;; #(1 2)


;; resizable
(make-array 5 :initial-element nil)
;; #(NIL NIL NIL NIL NIL)

;; use :fill-pointer to dynamically store elements
(make-array 5 :fill-pointer 3)
;; #(0 0 0)

(make-array 5 :fill-pointer 0)
;; #()

(defparameter *x* (make-array 5 :fill-pointer 0))
;; *X*

(vector-push 'a *x*)
;; 0

*x*
;; #(A)

(vector-push 'b *x*)
;; 1

*x*
;; #(A B)

(vector-push 'c *x*)
;; 2

*x*
;; #(A B C)

(vector-pop *x*)
;; C

*x*
;; #(A B)

;; completely resizable vector
(defparameter *x-resz* (make-array 5 :fill-pointer 0 :adjustable t))

(vector-push-extend 'a *x-resz*)

;; create a string from vector of character
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
;; ""

;; vector as sequence
(defparameter *x-seq* (vector 1 2 3))
;; *X-SEQ*

(length *x-seq*)
;; 3

(elt *x-seq* 0)
;; 1

(elt *x-seq* 1)
;; 2

(elt *x-seq* 2)
;; 3

;; (elt *x-seq* 3)
;; Evaluation aborted on #<SB-KERNEL:INDEX-TOO-LARGE-ERROR expected-type: (INTEGER 0 2) datum: 3>.

(setf (elt *x-seq* 0) 10)
;; 10

*x-seq*
;; #(10 2 3)

;; sequence iterating functions

(count 1 #(1 2 3 1 2 3 1 1))
;; 4

(remove 1 #(1 2 3 1 2 3 1 1))
;; #(2 3 2 3)

(remove 1 '(1 2 3 1 2 3 1 1))
;; (2 3 2 3)

(remove #\a "foobarbaz")
;; "foobrbz"

(substitute 10 1 #(1 2 3 1 2 3 4))
;; #(10 2 3 10 2 3 4)

(substitute 10 1 '(1 2 3 1 2 3 4))
;; (10 2 3 10 2 3 4)

(substitute #\x #\b "foobarbaz")
;; "fooxarxaz"

(find 1 #(1 2 3 1 2 3))
;; 1

(find 5 #(1 2 3 1 2 3))
;; NIL

(position 1 #(1 2 3 1 2 3))
;; 0

(count "foo" #("foo" "bar" "baz") :test #'string=)
;; 1

(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)
;; (C 30)

(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)
;; (A 10)

(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t)
;; (A 30)

(remove #\a "foobarbaz" :count 1)
;; "foobrbaz"

(remove #\a "foobarbaz" :count 1 :from-end t)
;; "foobarbz"

(defun verbose-first (x)
  (format t "looking at ~s~%" x) (first x))

(defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
;; *V*

(count 'a *v* :key #'verbose-first)
;; looking at (A 10)
;; looking at (B 20)
;; looking at (A 30)
;; looking at (B 40)
;; 2

(count 'a *v* :key #'verbose-first :from-end t)
;; looking at (B 40)
;; looking at (A 30)
;; looking at (B 20)
;; looking at (A 10)
;; 2
