;;;
;;; Impletation of life.
;;;

(setq width 10)
(setq height 10)

;; Returns location (x, y)'s element.
(defun get-m (board x y)
  (aref board x y))

;; Returns true if location (x, y)'s value is "@".
(defun alive? (board x y)
  (and (<= 0 x)
       (< x height)
       (<= 0 y)
       (< y width)
       (eq (get-m board x y) '%)))

(defun count-m (board x y)
  (flet ((at (x y)
            (if (alive? board x y) 1 0)))
       (+ (at (- x 1) (- y 1))
          (at (- x 1) y)
          (at (- x 1) (+ y 1))
          (at x (- y 1))
          (at x (+ y 1))
          (at (+ x 1) (- y 1))
          (at (+ x 1) y)
          (at (+ x 1) (+ y 1)))))

(defun next (board x y)
  (let ((c (count-m board x y)))
       (if (alive? board x y)
           (or (= c 2) (= c 3))
         (= c 3))))

;; Print out the given board.
(defun print-m (board)
    (dotimes (x width) 
        (dotimes (y height)
            (princ (get-m board x y))
            (princ #\space))
        (terpri)))

(defun run (board) 
    (let (((newboard (make-array (list width height) :initial-element '_))))
        (dotimes (x width)
            (dotimes (y height) 
                ; (setf (aref board x y) (if (next board x y) '% '_))))
                (princ x)
                (princ y))))
        (print-m newboard))

(defun run (board times) 
     (let ((newboard (make-array (list width height) :initial-element '_)))
        (dotimes (t times)
        (dotimes (x width)
            (dotimes (y height) 
                ;(print  (list x y (next board x y)))
                (setf (aref newboard x y) (if (next board x y) '% '_))
                ;(print  (list x y))
                ))
        (print-m newboard)
        (princ '====================)
        (terpri)
        (setq board newboard))))

(setq matrix (make-array (list width height) 
:initial-contents '((_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _ _ _)
       (_ % % % _ _ _ _ _ _)
       (_ _ _ % _ _ _ _ _ _)
       (_ _ % _ _ _ _ _ _ _))))

(run matrix 10)