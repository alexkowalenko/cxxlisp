(defun fill (seq x)
    (dotimes (n (length seq))
        (set-elt seq n x))
      seq)

(defun reversex (x)
  (cond ((null x) nil)
        (t (append (reversex (cdr x))
                 (list(car x))))))

(defun pair (x y)
  (cond ((and (null x) (null y)) nil)
        ((and (not (atom x)) (not (atom y)))
         (cons (cons (car x) (car y))
               (pair (cdr x) (cdr y))))))

(defun assoc (x y)
  (cond ((null y) nil)
        ((eq (caar y) x) (car y))
        ('t (assoc x (cdr y)))))

(setq values '((x . 100) (y . 200) (z . 50)))
(assoc 'y values)
(assoc 'a values)

(defun remove (x list)
  (cond ((null list) nil)
        ((eq x (car list)) (cdr list))
        (t (cons (car list) (remove x (cdr list)))))) 

(defun nthcdr (n list)
      (cond ((null list) nil)
            ((<= n 0) (car list))
            (t (nthcdr (- n 1) (cdr list)))))

(defun butlast (l &optional (n 1))
      (cond ((and 
                  (null (cdr l)) 
                  (plusp (- (length l) n))) nil)
            (t (cons (car l) 
                     (butlast (cdr l) n )))))

(defun last (l)
      (defun last-x (l n)
            (cond ((null l) nil)
                  ((= n 1) l)
                  (t (last-x (cdr l) (- n 1)))))
      (last-x l (length l)))

(defun rotate-list (l &key direction distance)
                (if (eq direction 'left)
                    (rotate-list-left l (if distance distance 1))
                    (rotate-list-right l (if distance distance 1)))) 

(defun rotate-list-right (l n)
                (if (zerop n )
                    l
                    (rotate-list-right (append (last l) (butlast l))
                                      (- n 1))))

(defun rotate-list-left (l n)
                (if (zerop n )
                    l
                    (rotate-list-left (append (rest l) (list (first l)))
                                      (- n 1))))
(rotate-list '(a b c d e))

(defun count-if (fn seq)
    (cond
         ((eq (length seq) 0) 0)
         ((funcall fn (elt seq 0)) 
            (+ 1 (count-if fn (cdr seq))))
	   (t (count-if fn (cdr seq)))))

(defmacro count-if-not-x (fn seq)
    `(count-if (complement ,fn) ,seq))    

(defun remove-if (fn seq) 
      (cond ((eq (length seq) 0) nil)
            ((funcall fn (elt seq 0)) (remove-if fn (cdr seq)))
            (t (cons (car seq)
                        (remove-if fn (cdr seq))))))