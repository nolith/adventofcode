;; http://adventofcode.com/2016/day/2

(defparameter *start1* '(1 . 1))
(defparameter *map1* (make-array '(3 3)
                              :initial-contents '((1 2 3) (4 5 6) (7 8 9))))

(defparameter *start2* '(2 . 0))
(defparameter *map2* (make-array '(5 5)
                                 :initial-contents '(
                                                     (() ()  1 () ())
                                                     (()  2  3  4 ())
                                                     ( 5  6  7  8  9)
                                                     (()  A  B  C ())
                                                     (() ()  D () ()))))

(defun move (position direction map)
  (let* ((limit (array-dimension map 0))
         (row (car position))
         (col (cdr position))
         (new-position (case direction
                        (#\R (cons row (1+ col)))
                        (#\L (cons row (1- col)))
                        (#\U (cons (1- row) col))
                        (#\D (cons (1+ row) col))))
         (new-row (car new-position))
         (new-col (cdr new-position)))
    (cond
      ((or (< new-row 0) (< new-col 0)) position)
      ((or (>= new-row limit) (>= new-col limit)) position)
      ((not (aref map new-row new-col)) position)
      (t new-position))))

(defun process-1-line (line map position)
  (reduce (lambda (pos direction)
            (move pos direction map))
          line
          :initial-value position)
  )

(defun solver (map lines start)
  (reduce (lambda (acc line)
            (let* ((key-cord (process-1-line line map acc))
                   (row (car key-cord))
                   (col (cdr key-cord))
                   (key (aref map row col)))
              (prin1 key)
              key-cord))
          lines :initial-value start))

(defun main ()
  (let ((input (loop
               for line = (read-line nil nil 'eof)
               until (eq 'eof line)
               collect line)))
    (solver *map1* input *start1*)
    (princ #\Newline)
    (solver *map2* input *start2*)))

(main)
