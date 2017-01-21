
(defparameter *input* (list 
  (cons 'L 4) (cons 'L 1) (cons 'R 4) (cons 'R 1) (cons 'R 1) (cons 'L 3) (cons 'R 5) (cons 'L 5) (cons 'L 2) (cons 'L 3) (cons 'R 2) (cons 'R 1) (cons 'L 4) (cons 'R 5) (cons 'R 4) (cons 'L 2) (cons 'R 1) (cons 'R 3) (cons 'L 5) (cons 'R 1) (cons 'L 3) (cons 'L 2) (cons 'R 5) (cons 'L 4) (cons 'L 5) (cons 'R 1) (cons 'R 2) (cons 'L 1) (cons 'R 5) (cons 'L 3) (cons 'R 2) (cons 'R 2) (cons 'L 1) (cons 'R 5) (cons 'R 2) (cons 'L 1) (cons 'L 1) (cons 'R 2) (cons 'L 1) (cons 'R 1) (cons 'L 2) (cons 'L 2) (cons 'R 4) (cons 'R 3) (cons 'R 2) (cons 'L 3) (cons 'L 188) (cons 'L 3) (cons 'R 2) (cons 'R 54) (cons 'R 1) (cons 'R 1) (cons 'L 2) (cons 'L 4) (cons 'L 3) (cons 'L 2) (cons 'R 3) (cons 'L 1) (cons 'L 1) (cons 'R 3) (cons 'R 5) (cons 'L 1) (cons 'R 5) (cons 'L 1) (cons 'L 1) (cons 'R 2) (cons 'R 4) (cons 'R 4) (cons 'L 5) (cons 'L 4) (cons 'L 1) (cons 'R 2) (cons 'R 4) (cons 'R 5) (cons 'L 2) (cons 'L 3) (cons 'R 5) (cons 'L 5) (cons 'R 1) (cons 'R 5) (cons 'L 2) (cons 'R 4) (cons 'L 2) (cons 'L 1) (cons 'R 4) (cons 'R 3) (cons 'R 4) (cons 'L 4) (cons 'R 3) (cons 'L 4) (cons 'R 78) (cons 'R 2) (cons 'L 3) (cons 'R 188) (cons 'R 2) (cons 'R 3) (cons 'L 2) (cons 'R 2) (cons 'R 3) (cons 'R 1) (cons 'R 5) (cons 'R 1) (cons 'L 1) (cons 'L 1) (cons 'R 4) (cons 'R 2) (cons 'R 1) (cons 'R 5) (cons 'L 1) (cons 'R 4) (cons 'L 4) (cons 'R 2) (cons 'R 5) (cons 'L 2) (cons 'L 5) (cons 'R 4) (cons 'L 3) (cons 'L 2) (cons 'R 1) (cons 'R 1) (cons 'L 5) (cons 'L 4) (cons 'R 1) (cons 'L 5) (cons 'L 1) (cons 'L 5) (cons 'L 1) (cons 'L 4) (cons 'L 3) (cons 'L 5) (cons 'R 4) (cons 'R 5) (cons 'R 2) (cons 'L 5) (cons 'R 5) (cons 'R 5) (cons 'R 4) (cons 'R 2) (cons 'L 1) (cons 'L 2) (cons 'R 3) (cons 'R 5) (cons 'R 5) (cons 'R 5) (cons 'L 2) (cons 'L 1) (cons 'R 4) (cons 'R 3) (cons 'R 1) (cons 'L 4) (cons 'L 2) (cons 'L 3) (cons 'R 2) (cons 'L 3) (cons 'L 5) (cons 'L 2) (cons 'L 2) (cons 'L 1) (cons 'L 2) (cons 'R 5) (cons 'L 2) (cons 'L 2) (cons 'L 3) (cons 'L 1) (cons 'R 1) (cons 'L 4) (cons 'R 2) (cons 'L 4) (cons 'R 3) (cons 'R 5) (cons 'R 3) (cons 'R 4) (cons 'R 1) (cons 'R 5) (cons 'L 3) (cons 'L 5) (cons 'L 5) (cons 'L 3) (cons 'L 2) (cons 'L 1) (cons 'R 3) (cons 'L 4) (cons 'R 3) (cons 'R 2) (cons 'L 1) (cons 'R 3) (cons 'R 1) (cons 'L 2) (cons 'R 4) (cons 'L 3) (cons 'L 3) (cons 'L 3) (cons 'L 1) (cons 'L 2)))

(setf *print-circle* t)
(defun circular (first &rest rest)
  (let ((items (cons first rest)))
    (setf (cdr (last items)) items)))

(defparameter *origin* (cons 0 0))
(defparameter *position* *origin*)
(defparameter *direction* 'n)
(defparameter *directions* (circular 'n 'e 's 'w))

(defun rotate (facing direction)
  (let* ((cords (member facing *directions*))
         (right (cadr cords))
         (left (cadddr cords)))
    (case direction
      (r right)
      (l left))))

(defun move-ahead (position facing distance)
  (car (last (track-ahead position facing distance))))

(defun track-ahead (position facing distance)
  (let ((x (car position))
        (y (cdr position)))
    (loop for i from 1 to distance
          collect (case facing
                    (n (cons x (+ y i)))
                    (e (cons (+ x i) y))
                    (s (cons x (- y i)))
                    (w (cons (- x i) y))))))

(defun move-1-step (position facing direction distance)
  (let* ((new-facing-direction (rotate facing direction))
         (track (track-ahead position new-facing-direction distance)))
    (cons track new-facing-direction)))

(defun taxicab-distance (a b)
  (let ((abs-x (abs (- (car a) (car b))))
        (abs-y (abs (- (cdr a) (cdr b)))))
    (+ abs-x abs-y)))

(defun find-first-duplicate (lst)
  (let ((head (car lst))
        (tail (cdr lst)))
    (cond
      ((null tail) nil)
      ((find head tail :test #'equal) head)
      (t (find-first-duplicate tail)))))

(defun walk-the-problem (steps)
  (setf *position* *origin*)
  (setf *direction* 'n)
  (let* ((positions (mapcar (lambda (step)
                   (let* ((turn-direction (car step))
                          (distance (cdr step))
                          (next (move-1-step *position* *direction*
                                             turn-direction distance))
                          (new-position (car (last (car next)))))
                     (setf *position* new-position)
                     (setf *direction* (cdr next))
                     (format t "Next step: ~S~%" next)
                     next)) steps))
         (first-duplicate (find-first-duplicate
                           (cons *origin*
                                 (reduce #'append (mapcar #'car positions)))))
         (distance (taxicab-distance *origin* *position*)))
    (format t "~%The final distance is ~D~%" distance)
    (if first-duplicate
        (format t "~%The final distance from ~S is ~D~%"
                first-duplicate (taxicab-distance first-duplicate *origin*)))))

(walk-the-problem *input*)

;; (defparameter *sample* (list (cons 'r 8) (cons 'r 4) (cons 'r 4) (cons 'r 8)))
;; (walk-the-problem *sample*)
