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

;;; hof variants

(count-if #'evenp #(1 2 3 4 5))
;; 2

(count-if-not #'evenp #(1 2 3 4 5))
;; 3

(position-if #'digit-char-p "abcd0001")
;; 4

(remove-if-not #'(lambda (x) (char= (elt x 0) #\f)) #("foo" "bar" "baz" "foom"))
;; #("foo" "foom")

(remove-duplicates #(1 2 1 2 3 4 5 5 4))
;; #(1 2 3 5 4)

(concatenate 'vector #(1 2 3) '(4 5 6))
;; #(1 2 3 4 5 6)

(concatenate 'list #(1 2 3) '(4 5 6))
;; (1 2 3 4 5 6)

(concatenate 'string "abc" '(#\d #\e #\f))
;; "abcdef"

(sort (vector "foo" "bar" "baz") #'string<)
;; #("bar" "baz" "foo")

(merge 'vector #(1 3 5) #(2 4 6) #'<)
;; #(1 2 3 4 5 6)

(merge 'list #(1 3 5) #(2 4 6) #'<)
;; (1 2 3 4 5 6)

;;; subseq manipulations
(subseq "foobarbaz" 3)
;; "barbaz"

(subseq "foobarbaz" 3 6)
;; "bar"

(defparameter *x* (copy-seq "foobarbaz"))
;; *X*

*x*
;; "foobarbaz"

(setf (subseq *x* 3 6) "xxx")
;; "xxx"

*x*
;; "fooxxxbaz"

(setf (subseq *x* 3 6) "abcd")
;; "abcd"

*x*
;; "fooabcbaz"

(setf (subseq *x* 3 6) "xx")
;;"xx"

*x*
;; "fooxxcbaz"

(position #\b "foobarbaz")
;; 3

(search "bar" "foobarbaz")
;; 3

(mismatch "foobarbaz" "foom")
;; 3

(mismatch "foobar" "bar" :from-end t)
;; 3

;;; sequence predicates
(every #'evenp #(1 2 3 4 5))
;; NIL

(some #'evenp #(1 2 3 4 5))
;; T

(notany #'evenp #(1 2 3 4 5))
;; NIL

(notevery #'evenp #(1 2 3 4 5))
;; T

(every #'> #(1 2 3 4) #(5 4 3 2))
;; NIL

(some #'> #(1 2 3 4) #(5 4 3 2))
;; T

(notany #'> #(1 2 3 4) #(5 4 3 2))
;; NIL

(notevery #'> #(1 2 3 4) #(5 4 3 2))
;; T

;;; sequence mapping
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
;; #(10 18 24 28 30)

(reduce #'+ #(1 2 3 4 5 6 7 8 9 10))
;; 55

;;; hash table
(defparameter *h* (make-hash-table))
;; *H*

(gethash 'foo *h*)
;; NIL
;; NIL

(setf (gethash 'foo *h*) 'quux)
;; QUUX

*h*
;; #<HASH-TABLE :TEST EQL :COUNT 1 {1002F81003}>

(gethash 'foo *h*)
;; QUUX
;; T

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(setf (gethash 'bar *h*) nil)
;; NIL

(show-value 'foo *h*)
;; "Value QUUX actually present."

(show-value 'bar *h*)
;; "Value NIL actually present."

(show-value 'baz *h*)
;; "Value NIL because key not found."

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
;; FOO => QUUX
;; BAR => NIL
;; NIL

(loop for k being the hash-keys in *h* using (hash-value v)
      do (format t "~a => ~a~%" k v))
;; FOO => QUUX
;; BAR => NIL
;; NI
