(cond (t t) 
      (nil nil))
(cond (t nil) 
      (nil t))
(cond (nil t) 
      (t nil))
(cond (nil nil) 
      (t (quote x)))
