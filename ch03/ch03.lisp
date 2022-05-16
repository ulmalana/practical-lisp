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
