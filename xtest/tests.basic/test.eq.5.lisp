(eq (quote a) (quote a))
(eq (quote a) (quote b))
(eq (quote (a b)) (quote (a b)) ) ; this is false
(eq (quote (a b)) (quote (a z)) )
(eq (quote a) (quote (a b)))