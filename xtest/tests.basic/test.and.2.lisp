(and (atom (quote x)) (atom (quote y)))
(and (atom (quote x)) (atom (quote (y))))
(and (atom (quote z)) (eq (quote x) (quote x)))
(and (atom (quote z)) (eq (quote x) (quote 2)))