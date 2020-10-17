(defun flatten (x)
    (labels ((rec (x acc)
                  (cond ((null x) acc)
                        ((atom x) (cons x acc))
                        (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
      (rec x nil)))

(flatten '(a b))
(flatten '(a (c1 v2) b))
(flatten '(a (c1(c1 v2) v2) b))
(flatten '(a (c1 v2) b (c1 (c1 (c1(c1 (c1(c1 v2) v2) v2))))))

(flet ((f (x) x)
        (g (x) (f x)))
    (g 1))

(labels ((f (x) x)
        (g (x) (f x)))
    (g 3.14159))