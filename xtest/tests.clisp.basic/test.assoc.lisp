; assoc takes atom x and a list y in the form created by pair, and returns the
; second element of the first pair in y whose first element is x.

(assoc 'x '((y . 2) (x . 1)))
(assoc 'x '((x . 1) (y . 2)))
(assoc 'x '((x . 1) (y . 2) (x . 3)))
(assoc 'x '((x . new) (x . 1) (y . 2) (x . 3)))
(assoc 'x '((y . 2) (x . (a b))))
(assoc 'z '((y . 2) (x . 1)))
(assoc 'x '())
(assoc 'x 'y)
