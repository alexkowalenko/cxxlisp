(defun sum (term a next b)
    (if (> a b)
        0
        (+ (term a) 
           (sum term (next a) next b))))

(defun inc (n) (+ n 1))

(defun cube (n) (* n n n))

(defun sum-cube (a b)
    (sum cube a inc b))

(defun identity (x) x)

(defun sum-integers (a b)
    (sum identity a inc b))

(sum-cube 1 4)

(defun sum-square (a b)
    (sum (lambda (x) (* x x)) a inc b))

(sum-square 1 4)

;; two lambdas
(defun sum-double (a b)
    (sum (lambda (x) (+ x x)) 
         a 
         (lambda (n) (+ n 1)) 
         b))

(sum-double 1 4)
