;; rationals
123
;; 123

+123
;; 123

-123
;; -123

123.
;; 123

2/3
;; 2/3

-2/3
;; -2/3

4/6
;; 2/3 *reduced*

6/3
;; 2

#b10101 ;; binary
;; 21

#b1010/1011 ;; binary rational
;; 10/11

#o777 ;; octal
;; 511

#xdede ;; hex
;; 57054

#36rZASTBH345 ;; custom based (36)
;; 99585211099397

;;;; floats

1.0
;; 1.0

1e0 ;; default single
;; 1.0

1d0 ;; double float
;; 1.0d0

123.0
;; 123.0

123e0
;; 123.0

0.123
;; 0.123

.123
;; 0.123

123e-3
;; 0.123

123E-3
;; 0.123

0.123e20
;; 1.23e+19

123d23 ;; double float 
;; 1.23d+25

;;;;; complex numbers

#c(2 1)
;; #C(2 1)

#C(4/5 1/3)
;; #C(4/5 1/3)

#c(1 3.4)
;; #C(1.0 3.4)

#c(1.0 1.0d0)
;; #C(1.0d0 1.0d0)

#c(1/2 1.0)
;; #C(0.5 1.0)

#c(5 0.0)
;; #C(5.0 0.0)

#c(5 0)
;; 5

;; basic arithmetic

(+ 1 2.0)
;; 3.0

(/ 2 3.0)
;; 0.6666667

(+ #c(1 2) 3)
;; #C(4 2)

(+ #c(1 2) 3/2)
;; #C(5/2 2)

(+ #c(1 1) #c(2 -1))
;; 3

;;;; characters

;; use #\ and followed by the charater itself
;; example
;; #\r
;; #\"
;; #\Space
;; #\Tab
;; #\Newline

;; for comparing chars, CHAR= can be used for case-sensitive and
;; CHAR-EQUALP for case-insensitive comparison.
