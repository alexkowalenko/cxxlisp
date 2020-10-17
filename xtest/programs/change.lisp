(defun countchange (amount)
    (cc amount 5)) 

(defun cc (amount kindsofcoins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kindsofcoins 0)) 0)
           (t (+ (cc amount (- kindsofcoins 1))
                (cc (- amount (firstdenomination kindsofcoins)) kindsofcoins)))))

(defun firstdenomination (kindsofcoins)
    (cond ((= kindsofcoins 1) 1)
          ((= kindsofcoins 2) 2)
          ((= kindsofcoins 3) 5)
          ((= kindsofcoins 4) 10)
          ((= kindsofcoins 5) 20)
          ((= kindsofcoins 6) 50)
          ((= kindsofcoins 7) 100)
          ((= kindsofcoins 8) 200))
            )

(countchange 106)