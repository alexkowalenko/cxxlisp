(defun dotimes-expt (m n)
    (let ((result 1))
        (dotimes (count n result)
            (setf result (* m result)))))
(dotimes-expt 3 2)
(dotimes-expt 2 3)
(dotimes-expt 2 7)

(setf freezing 32 boiling 212)

(defun count-outlyers (list-of-elements)
    (let ((result 0))
        (dolist (element list-of-elements result)
            (when (or (> element boiling)
                      (< element freezing))
                (setf result (+ result 1))))))

(count-outlyers '(18 75 31 180 270 52))

#| no return statement
(defun count-outlyers (n list-of-elements)
    (let ((result 0)
         (outlyers nil))
      (dolist (element list-of-elements outlyers)
            (cond ((or (> element boiling)
                      (< element freezing))
                    (setf result (+ result 1))
                    (push element outlyers))
                  ((= n result) (return outlyers))))))
|#

(defun do-expt (m n)
    (do ((result 1)
        (exponent n))
        ((zerop exponent) result)
        (setf result (* m result))
        (setf exponent (- exponent 1))))

(do-expt 3 2)
(do-expt 2 3)

#| no return statement
(defun do-expt (m n)
    (do ((result 1)
        (exponent n))
        ()
        (when (zerop exponent)
            (return result))
        (setf result (* m result))
        (setf exponent (- exponent 1))))
|#

(defun do-expt (m n) 
        (do ((result 1 (* m result))
            (exponent n (- exponent 1)))
            ((zerop exponent) result))) 

(do-expt 3 2)
(do-expt 2 3)

(defun do-expt (m n) 
        (do ((result m (* m result))
            (exponent n (- exponent 1))
            (counter (- exponent 1) (- exponent 1)))
            ((zerop exponent) result))) 

(do-expt 3 2)
(do-expt 2 3)

(setf cheers '(cheer cheer cheer))
(setf loop-count 0)

#|
(loop 
    (when (endp cheers) (return loop-count))
    (setf cheers (rest cheers))
    (setf loop-count (+ loop-count 1)))
|#

(prog1 (setf a 'x) (setf b 'y) (setf a 'z))
(progn (setf a 'x) (setf b 'y) (setf a 'z))