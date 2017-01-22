;; http://adventofcode.com/2016/day/2

(defparameter *default-line-len* 3)
(defparameter *default-limit* (* *default-line-len* *default-line-len*))
(defparameter *default-position* 5)


(defun move (position direction line-len limit)
  (let ((new-position (case direction
                        (#\R (1+ position))
                        (#\L (1- position))
                        (#\U (- position line-len))
                        (#\D (+ position line-len)))))
    (cond
      ((or (< new-position 1) (> new-position limit)) position)
      ((and
        (eq 1 (mod position line-len))
        (eq #\L direction)) position)
      ((and
        (zerop (mod position line-len))
        (eq #\R direction)) position)
      (t new-position))))

(defun process-1-line (line &optional position line-len limit)
  (unless position (setf position *default-position*))
  (unless line-len (setf line-len *default-line-len*))
  (unless limit (setf limit *default-limit*))

  (reduce (lambda (acc elem)
            (move acc elem line-len limit))
          (string-trim (coerce (list #\Newline) 'string) line)
          :initial-value position)
  )

(defun main ()
  (let ((input (loop
               for line = (read-line nil nil 'eof)
               until (eq 'eof line)
               collect line)))
    (reduce (lambda (acc line)
              (let ((key (process-1-line line acc)))
                (format t "~D <= ~S~%" key line)
                key))
            input :initial-value *default-position*)))

(main)
