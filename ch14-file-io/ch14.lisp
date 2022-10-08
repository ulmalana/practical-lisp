;;;; chapter 14 - file io

;; reading file
(let ((in (open "lorem.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "1 ~a~%" line))
    (close in)))

(defparameter *s* (open "lorem.txt"))
;; *S*

(read *s*)
;; (1 2 3)

(read *s*)
;; 456

(read *s*)
;; "a string"

(read *s*)
;; ((A B) (C D))

(read *s*)
;; Evaluation aborted on #<END-OF-FILE {100307CDA3}>.

(close *s*)
;; T

;; open and close file automatically
;; (with-open-file (stream "lorem.txt")
;;   (format t "~a~%" (read-line stream)))

;;; pathnames

(pathname-directory (pathname "/foo/bar/baz.txt"))
;; (:ABSOLUTE "foo" "bar")

(pathname-name (pathname "/foo/bar/baz.txt"))
;; "baz"

(pathname-type (pathname "/foo/bar/baz.txt"))
;; "txt"

(pathname "/foo/bar/baz.txt")
;; #P"/foo/bar/baz.txt"

(namestring #p"/foo/bar/baz.txt")
;; "/foo/bar/baz.txt"

(directory-namestring #p"/foo/bar/baz.txt")
;; "/foo/bar/"

(file-namestring #p"/foo/bar/baz.txt")
;; "baz.txt"
