(setf temperature 100)
(print temperature)
;(if (< -1 (print (- temperature 98.6)) +1) ; < can't have more than 2 args
;    'normal
;    'abnormal)
(setf name 'kirsh symptoms '(fever rash nausea))
(print (list 'patient name
            'presented (length symptoms)
            'symptoms symptoms))

;(read) ; read no implmented

;(let ((p nil))
;    (print 
;        '(please type a patient name))
;    (setf p (read))
;    (print (append '(ok the name is)
;                   (list p)))
;    p)

(format t "~Hello!")