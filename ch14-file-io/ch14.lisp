;;;; chapter 14 - file io

;; reading file
(let ((in (open "lorem.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "~a~%" line))
    (close in)))
