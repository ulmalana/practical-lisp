# Variables

Two kinds of variables in Common Lisp: **Lexical** dan **Dynamic**.
(Roughly correspond to "local" and "global" variables).

## Basics

* Common Lisp is **Dynamic, strongly typed**.
* Values are references to objects. Assigning new values only refer to new object and doesnt change previously referenced object (except for mutable objects).
* **DEFUN** and **LET** can be used to introduce new variables
* LET\* can be thought as nested version of LET.

## Lexical Variables and Closure

* By default, all bindings in  Common Lisp is lexically scoped.
* Meaning that all variables can only be accessed in its scope.
* Anonymous function (lambda) is called *closure* because it closes over the binding created by the **LET**.
* Example: the LET containing `count` below can be called by "outsider" because of lambda
* `(let ((count 0)) #'(lambda () (setf count (1+ count))))`
* the caller:
* `(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))`

