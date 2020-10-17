(defmacro unit-of-time (value unit)
    `(* ,value
        ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) (/ 1.0 1000))
            ((us) (/ 1.0 1000000)))))

(unit-of-time 1 d)
(unit-of-time 1 h)
(unit-of-time 2 h)
(unit-of-time 2 ms)
(unit-of-time 2 us)