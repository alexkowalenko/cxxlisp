(when t 'true)
(when (atom 's) 'is-atom)
(when (atom '(a b)) 'is-atom)
(when nil 'true)
(when (not (atom 's)) 'is-not-atom)
(when (not (atom '(a b))) 'is-not-atom)
