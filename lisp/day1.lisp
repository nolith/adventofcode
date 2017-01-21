
(defparameter *input* (list 
  (cons 'L 4) (cons 'L 1) (cons 'R 4) (cons 'R 1) (cons 'R 1) (cons 'L 3) (cons 'R 5) (cons 'L 5) (cons 'L 2) (cons 'L 3) (cons 'R 2) (cons 'R 1) (cons 'L 4) (cons 'R 5) (cons 'R 4) (cons 'L 2) (cons 'R 1) (cons 'R 3) (cons 'L 5) (cons 'R 1) (cons 'L 3) (cons 'L 2) (cons 'R 5) (cons 'L 4) (cons 'L 5) (cons 'R 1) (cons 'R 2) (cons 'L 1) (cons 'R 5) (cons 'L 3) (cons 'R 2) (cons 'R 2) (cons 'L 1) (cons 'R 5) (cons 'R 2) (cons 'L 1) (cons 'L 1) (cons 'R 2) (cons 'L 1) (cons 'R 1) (cons 'L 2) (cons 'L 2) (cons 'R 4) (cons 'R 3) (cons 'R 2) (cons 'L 3) (cons 'L 188) (cons 'L 3) (cons 'R 2) (cons 'R 54) (cons 'R 1) (cons 'R 1) (cons 'L 2) (cons 'L 4) (cons 'L 3) (cons 'L 2) (cons 'R 3) (cons 'L 1) (cons 'L 1) (cons 'R 3) (cons 'R 5) (cons 'L 1) (cons 'R 5) (cons 'L 1) (cons 'L 1) (cons 'R 2) (cons 'R 4) (cons 'R 4) (cons 'L 5) (cons 'L 4) (cons 'L 1) (cons 'R 2) (cons 'R 4) (cons 'R 5) (cons 'L 2) (cons 'L 3) (cons 'R 5) (cons 'L 5) (cons 'R 1) (cons 'R 5) (cons 'L 2) (cons 'R 4) (cons 'L 2) (cons 'L 1) (cons 'R 4) (cons 'R 3) (cons 'R 4) (cons 'L 4) (cons 'R 3) (cons 'L 4) (cons 'R 78) (cons 'R 2) (cons 'L 3) (cons 'R 188) (cons 'R 2) (cons 'R 3) (cons 'L 2) (cons 'R 2) (cons 'R 3) (cons 'R 1) (cons 'R 5) (cons 'R 1) (cons 'L 1) (cons 'L 1) (cons 'R 4) (cons 'R 2) (cons 'R 1) (cons 'R 5) (cons 'L 1) (cons 'R 4) (cons 'L 4) (cons 'R 2) (cons 'R 5) (cons 'L 2) (cons 'L 5) (cons 'R 4) (cons 'L 3) (cons 'L 2) (cons 'R 1) (cons 'R 1) (cons 'L 5) (cons 'L 4) (cons 'R 1) (cons 'L 5) (cons 'L 1) (cons 'L 5) (cons 'L 1) (cons 'L 4) (cons 'L 3) (cons 'L 5) (cons 'R 4) (cons 'R 5) (cons 'R 2) (cons 'L 5) (cons 'R 5) (cons 'R 5) (cons 'R 4) (cons 'R 2) (cons 'L 1) (cons 'L 2) (cons 'R 3) (cons 'R 5) (cons 'R 5) (cons 'R 5) (cons 'L 2) (cons 'L 1) (cons 'R 4) (cons 'R 3) (cons 'R 1) (cons 'L 4) (cons 'L 2) (cons 'L 3) (cons 'R 2) (cons 'L 3) (cons 'L 5) (cons 'L 2) (cons 'L 2) (cons 'L 1) (cons 'L 2) (cons 'R 5) (cons 'L 2) (cons 'L 2) (cons 'L 3) (cons 'L 1) (cons 'R 1) (cons 'L 4) (cons 'R 2) (cons 'L 4) (cons 'R 3) (cons 'R 5) (cons 'R 3) (cons 'R 4) (cons 'R 1) (cons 'R 5) (cons 'L 3) (cons 'L 5) (cons 'L 5) (cons 'L 3) (cons 'L 2) (cons 'L 1) (cons 'R 3) (cons 'L 4) (cons 'R 3) (cons 'R 2) (cons 'L 1) (cons 'R 3) (cons 'R 1) (cons 'L 2) (cons 'R 4) (cons 'L 3) (cons 'L 3) (cons 'L 3) (cons 'L 1) (cons 'L 2)))

(setf *print-circle* t)
(defun circular (first &rest rest)
  (let ((items (cons first rest)))
    (setf (cdr (last items)) items)))

(defparameter *position* (cons 0 0))
(defparameter *direction* 'n)
(defparameter *directions* (circular 'n 'e 's 'w))

(defun rotate (facing direction)
  (format t "rotate: facing ~S direction ~s~%" facing direction)
  (let* ((cords (member facing *directions*))
         (right (cadr cords))
         (left (cadddr cords)))
    (case direction
      (r right)
      (l left))))

(defun move-ahead (position facing distance)
  (format t "move-ahead: position ~S facing ~S distance ~s~%"
          position facing distance)
  (let ((x (car position))
        (y (cdr position)))
    (case facing
      (n (cons x (+ y distance)))
      (e (cons (+ x distance) y))
      (s (cons x (- y distance)))
      (w (cons (- x distance) y)))))

(defun move-1-step (position facing direction distance)
  (format t "position: ~S facing: ~S direction: ~S distance: ~S~%"
          position facing direction distance)
  (let* ((new-facing-direction (rotate facing direction))
         (new-position (move-ahead position new-facing-direction distance)))
    (format t "new position: ~S new-facing-direction: ~S~%"
            new-position new-facing-direction)
    (list new-position new-facing-direction)))

(defun taxicab-distance (a b)
  (let ((abs-x (abs (- (car a) (car b))))
        (abs-y (abs (- (cdr a) (cdr b)))))
    (+ abs-x abs-y)))

(defun walk-the-problem (steps)
  (setf *position* (cons 0 0))
  (setf *direction* 'n)
  (format t "Walking the problem from ~S facing ~S~%~S~%"
          *position* *direction* steps)
  (mapc (lambda (step)
          (format t "~%Step ~S: from ~S facing ~S~%"
                  step *position* *direction*)
          (let* ((turn-direction (car step))
                 (distance (cdr step))
                 (next (move-1-step *position* *direction*
                                    turn-direction distance)))
            (setf *position* (car next))
            (setf *direction* (cadr next))
            (format t "Next step: ~S~%position: ~S~%direction: ~S~%"
                    next *position* *direction*)
            next)) steps)
  (let ((distance (taxicab-distance (cons 0 0) *position*)))
    (format t "~%The final distance is ~D~%" distance)))

(walk-the-problem *input*)

