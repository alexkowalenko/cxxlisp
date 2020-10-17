(or (atom (quote x)) (atom (quote y)))
(or (atom (quote x)) (atom (quote (y))))
(or (atom (quote z)) (eq (quote x) (quote x)))
(or (atom '(a s)) (eq (quote x) (quote 2)))