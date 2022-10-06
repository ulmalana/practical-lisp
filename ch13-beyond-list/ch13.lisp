;;;; chapter 13 - beyond lists

;;; trees

;; lists can also be treated as trees.
;; some functions may need to copy the whole tree or only share the structure.
;; for example:
;; COPY-LIST function only copy the original cons cell of the list structure.
;; whereas COPY-TREE will copy the whole tree.

;;; sets

;; sets can also be implemented in cons cell and treated as list.

(defparameter *set* ())
;; *SET*

(adjoin 1 *set*)
;; (1)

*set*
;; NIL

(setf *set* (adjoin 1 *set*))
;; (1)

*set*
;; (1)

(pushnew 2 *set*)
;; (2 1)

*set*
;; (2 1)

(pushnew 2 *set*)
;; (2 1)

*set*
;; (2 1)

(member 1 '(3 2 1))
;; (1)

(member 4 '(3 2 1))
;; NIL

(union '(3 4 5) '(1 2 3))
;; (5 4 1 2 3)

(subsetp '(1 2 3) '(3 4 2 1))
;; T

(subsetp  '(3 4 2 1) '(1 3 2))
;; NIL

;;; alist and plist

;; mapping keys and values in a list using cons cells.
;; it may be dotted pairs like
;; ((A . 1) (B . 2) (C . 3))

;; alist lookup
(assoc 'a '((a . 1) (b . 2) (c . 3)))
;; (A . 1)

(assoc 'c '((a . 1) (b . 2) (c . 3)))
;; (C . 3)

(assoc 'd '((a . 1) (b . 2) (c . 3)))
;; NIL

;; get the value of a key
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3))))
;; 1

;; consing a new key/value to alist
;; (cons (cons 'new-key 'new-value) alist)
;; or
;; (acons 'new-key 'new-value alist)

;; creating alist from two separate list
(pairlis '(a b c) '(1 2 3))
;; ((C . 3) (B . 2) (A . 1))

(pairlis '(1 2 3) '(a b c))
;; ((3 . C) (2 . B) (1 . A))

;; plist
(defparameter *plist* ())
;; *PLIST*

*plist*
;; NIL

(setf (getf *plist* :a) 1)
;; 1

*plist*
;; (:A 1)

(setf (getf *plist* :a) 2)
;; 2

*plist*
;; (:A 2)

(remf *plist* :a)
;; T

*plist*
;; NIL

;;; destructuring bind
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))
;; (:X 1 :Y 2 :Z 3)

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z))
;; (:X 1 :Y (2 20) :Z 3)

(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; (:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; (:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; (:X 1 :Y1 2 :Y2 NIL :Z 3)
(destructuring-bind (&key x y z) (list :y 1 :z 3 :x 2)
  (list :x x :y y :z z))
;; (:X 2 :Y 1 :Z 3)

(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 3 :x 2)
  (list :x x :y y :z z :whole whole))
;; (:X 2 :Y 3 :Z 1 :WHOLE (:Z 1 :Y 3 :X 2))
