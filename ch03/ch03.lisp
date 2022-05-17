;; create a list
(list 1 2 3 4)

;; create a property list or plist
;; (a list with key)
(list :a 1 :b 2 :c 3 :4 4)

;; get a value in plist
(getf (list :a 1 :b 2 :c 3) :b)

;; create a function to store cd information
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped ))

;; create a global variable db
;; set it to nil
;; the asterisk (*) is the convention for global variables
(defvar *db* nil)

;; this function add items to global variable *db*
(defun add-record (cd) (push cd *db*))

;; add song records to *db*
(add-record (make-cd "Piano Concerto No 1" "Tchaikovsky" 10 T))
(add-record (make-cd "Fur Elise" "Beethoven" 9 T))
(add-record (make-cd "Marriage of Figaro" "Mozart" 9 T))
(add-record (make-cd "Piano Concerto 2" "Rachmaninoff" 10 T))

;; this function displays *db* in a better format
;; ~a for aesthetic
;; ~t for tabulating
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; this function reads input from as prompted
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; this function records sing info typed by users
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   ; :junk-allowed allows non-numeric char in the input
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

;; this function stores many records with loop
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; this function saves the database to a file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; this will load the database back from a file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; query only even number in a list
;; #' = get the function with the following name
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8))

;; query only even number with anonymous function (lambda)
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8))

;; get a record from the db by its artist name
(remove-if-not
 #'(lambda (cd) (equal (getf cd :artist) "Mozart")) *db*)

;; this function queries db by artist name
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

;; a more general query function
;; this still needs lambda function as an argument, which may be complicated to type
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; wrap lambda function for querying by artist name
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; with the help of artist-selector, the query becomes simpler
(select (artist-selector "Mozart"))

;; keyword parameters
;; bind values to some keys
(defun foo (&key a b c) (list a b c))
;; (foo :a 1 :b 2 :c 3) ==> (1 2 3)
;; (foo :b 2 :c 3 :a 1) ==> (1 2 3)
;; (foo :a 1 :c 3)      ==> (1 NIL 3)
;; (foo)                ==> (NIL NIL NIL)

;; function with keyword parameters with default values
;; a has no default value
;; b has 20 as default value
;; c has 30 as default value and c-p as supplied-p parameter
;; supplied-p parameter will be set true or false if the argument for the parameter
;;    was actually passed or not.
(defun foo2 (&key a (b 20) (c 30 c-p)) (list a b c c-p))
;; (foo2 :a 1 :b 2 :c 3)  ==> (1 2 3 T)
;; (foo2 :c 3 :b 2 :a 1)  ==> (1 2 3 T)
;; (foo2 :a 1 :c 3)       ==> (1 20 3 T)
;; (foo2)                 ==> (NIL 20 30 NIL)

;; this function is a general version for querying the database
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; updating database records
(defun update-db (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; this function delete rows from the database
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; macro for reversing a list
(defmacro backwards (expr) (reverse expr))

;; this function returns a list of an expression that can be used later
;; single forward quote (') prevents Lisp from evaluating the form
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
;; example
;; > (make-comparison-expr :rating 10)
;; (EQUAL (GETF CD :RATING) 10)

;; back quote (`) is similar to forward quote ('), but back quote will evaluate
;; any subexpression that is preceded by a comma
;; `(1 2 (+ 1 2))  ==> (1 2 (+ 1 2))
;; `(1 2, (+ 1 2)) ==> (1 2 3)

;; using back quote (`) for make-comparison-expr
(defun make-comparison-expr2 (field value)
  `(equal (getf cd ,field) ,value))

;; this function loops for a number of fields list  and make comparison
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr2 (pop fields) (pop fields))))

;; macro version of where
(defmacro where2 (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
;; ,@ splices the value of the following expression
;; difference between , and ,@
;; `(and ,(list 1 2 3))    ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))   ==> (AND 1 2 3)
;; `(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4)
;; &rest allows for arbitrary number of arguments and collects them into a list
