(in-package :adventoflisp)

(defun solve (year day)
  (cond ((= year 2015)
         (cond ((= day 1) (solve-day01))
               ((= day 2) (solve-day02))
               ((= day 3) (solve-day03))
               ((= day 4) (solve-day04))
               ((= day 5) (solve-day05))))))

(defun format-solutions (year day sol1 sol2)
  (format t
          "~a day ~a part 1: ~a~%~a day ~a part 2: ~a~%"
           year   day        sol1 year  day        sol2))

(defun solve-day01 ()
  "Not quite Lisp"
  (let ((instructions (uiop:read-file-string "day01.txt")))
    (format-solutions 2015 1
                      (instructions->floor instructions)
                      (instructions->basement-pos instructions))))

(defun solve-day02 ()
  "I was told there would be no math"
  (let ((lines (uiop:read-file-lines "day02.txt")))
    (let ((puzzle-input (mapcar 'parse-dimensions lines)))
      (let ((paper-feet (mapcar 'dims->paper puzzle-input))
            (ribbon-feet (mapcar 'dimensions->ribbon puzzle-input)))
        (format-solutions 2015 2
                          (reduce '+ paper-feet)
                          (reduce '+ ribbon-feet))))))

(defun solve-day03 ()
  "Perfectly Spherical Houses in a Vacuum"
  (let ((puzzle-input (uiop:read-file-string "day03.txt")))
    (let ((unique-pos (2dgrid/santa-pos puzzle-input))
          (stereo-pos (2dgrid/santa+robo-pos puzzle-input)))
      (format-solutions 2015 3 unique-pos stereo-pos))))

(defun solve-day04 ()
  "The Ideal Stocking Stuffer"
  (let ((puzzle-input "ckczppom"))
    (let ((lowest-n (find-lowest-five puzzle-input 1))
          (lowest-m (find-lowest-six  puzzle-input 1)))
      (format-solutions 2015 4 lowest-n lowest-m))))

(defun solve-day05 ()
  "Doesn't He Have Intern-Elves For This?"
  (let ((puzzle-input (uiop:read-file-lines "day05.txt")))
    (format-solutions 2015 2
		      (count-nice puzzle-input)
		      (count-nice-again puzzle-input))))
