(defmacro eight () (+ 3 5))
(eight)

(defmacro if-x (x y z) 
    (list 'cond (list x y) (list t z)))
(if-x (atom 'x) 1 2)
(if-x (atom '(a b)) 1 2)

(defmacro when-x (test expr)
    (list 'cond (list test expr) (list 't 'nil)))
(when-x t 2)
(when-x nil 2)
(when-x (atom '(a b)) 2)