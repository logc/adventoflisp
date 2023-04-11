(in-package :adventoflisp)

(defun solve (year day)
  (cond ((= year 2015) (solve-2015 day))))

(defun solve-2015 (day)
  (cond ((= day 1) (solve-day01))
        ((= day 2) (solve-day02))
        ((= day 3) (solve-day03))
        ((= day 4) (solve-day04))))

(defun str/head (a-string) (char a-string 0))

(defun str/second (a-string) (char a-string 1))

(defun str/rest (a-string) (subseq a-string 1 (length a-string)))

;; FIXME: check that n is not greater than length
(defun str/n-to-end (a-string n) (subseq a-string n (length a-string)))

(defun open-paren-p (a-char) (char= a-char #\())

(defun close-paren-p (a-char) (char= a-char #\)))

(defun instructions->floor (instructions &optional (floor 0))
  (cond ((= (length instructions) 0) floor)
        ((open-paren-p (str/head instructions))
         (instructions->floor (str/rest instructions) (+ floor 1)))
        ((close-paren-p (str/head instructions))
         (instructions->floor (str/rest instructions) (- floor 1)))
        (t (instructions->floor (str/rest instructions) floor))))

(defun instructions->basement-pos (instructions &optional (floor 0) (pos 0))
  (cond ((or (< floor 0) (= (length instructions) 0)) pos)
        ((open-paren-p (str/head instructions))
         (instructions->basement-pos (str/rest instructions) (+ floor 1) (+ pos 1)))
        ((close-paren-p (str/head instructions))
         (instructions->basement-pos (str/rest instructions) (- floor 1) (+ pos 1)))
        (t (instructions->basement-pos (str/rest instructions) floor (+ pos 1)))))

(defun solve-day01 ()
  "Not quite Lisp"
  (let ((instructions (uiop:read-file-string "day01.txt")))
    (format t "2015 day 1 part 1: ~a~%" (instructions->floor instructions))
    (format t "2015 day 1 part 2: ~a~%" (instructions->basement-pos instructions))))


(defun parse-dimensions (line)
  (let ((cleaned (string-right-trim "\n" line)))
    (let ((tokens (uiop:split-string cleaned :separator "x")))
      (mapcar #'parse-integer tokens))))

(defun dims->paper (dimensions)
  (let ((l (first dimensions))
        (w (second dimensions))
        (h (third dimensions)))
    (let ((a1 (* l w))
          (a2 (* w h))
          (a3 (* h l)))
      (+ (* 2 a1) (* 2 a2) (* 2 a3) (min a1 a2 a3)))))

(defun dimensions->ribbon (dimensions)
  (let ((sorted (sort dimensions #'<)))
    (let ((a (first sorted))
          (b (second sorted))
          (c (third sorted)))
      (+ (* 2 a) (* 2 b) (* a b c)))))

(defun solve-day02 ()
  "I was told there would be no math"
  (let ((lines (uiop:read-file-lines "day02.txt")))
    (let ((puzzle-input (mapcar 'parse-dimensions lines)))
      (let ((paper-feet (mapcar 'dims->paper puzzle-input))
            (ribbon-feet (mapcar 'dimensions->ribbon puzzle-input)))
        (format t "2015 day 2 part 1: ~a~%" (reduce '+ paper-feet))
        (format t "2015 day 2 part 2: ~a~%" (reduce #'+ ribbon-feet))))))

(defun hash/set (hash key val)
  (setf (gethash key hash) val)
  hash)

(defun 2dgrid/update-pos (pos step)
  (let ((x (aref pos 0))
        (y (aref pos 1)))
    (cond ((char= step #\>) (vector (+ x 1) y))
          ((char= step #\<) (vector (- x 1) y))
          ((char= step #\^) (vector x (+ y 1)))
          ((char= step #\v) (vector x (- y 1)))
          (t pos))))

(defun 2dgrid/santa-pos
    (steps
     &optional
       (pos #(0 0))
       (seen (make-hash-table :test 'equalp)))
  (hash/set seen pos t)
  (if (= (length steps) 0)
      (length (alexandria:hash-table-keys seen))
      (let ((new-pos (2dgrid/update-pos pos (str/head steps))))
        (2dgrid/santa-pos (str/rest steps) new-pos seen))))

(defun 2dgrid/santa+robo-pos
    (steps
     &optional
       (santa-pos #(0 0))
       (robo-pos #(0 0))
       (seen (make-hash-table :test 'equalp)))
  (hash/set seen santa-pos t)
  (hash/set seen robo-pos t)
  (if (< (length steps) 2)
      (length (alexandria:hash-table-keys seen))
      (let* ((next-step (str/head steps))
             (next-next-step (str/second steps))
             (new-santa-pos (2dgrid/update-pos santa-pos next-step))
             (new-robo-pos (2dgrid/update-pos robo-pos next-next-step))
             (rest-steps (str/n-to-end steps 2)))
        (2dgrid/santa+robo-pos rest-steps new-santa-pos new-robo-pos seen))))

(defun solve-day03 ()
  "Perfectly Spherical Houses in a Vacuum"
  (let ((puzzle-input (uiop:read-file-string "day03.txt")))
    (let ((unique-pos (2dgrid/santa-pos puzzle-input))
          (stereo-pos (2dgrid/santa+robo-pos puzzle-input)))
      (format t "2015 day 3 part 1: ~a~%" unique-pos)
      (format t "2015 day 3 part 2: ~a~%" stereo-pos))))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-string string)
          do (format s "~2,'0x" code))))

(defun five-zeroes-p (a-string)
  (uiop:string-prefix-p "00000" a-string))

(defun six-zeroes-p (a-string)
  (uiop:string-prefix-p "000000" a-string))

(defun find-lowest-five (key n)
  (let ((hash (md5-hex (concatenate 'string key (write-to-string n)))))
    (if (five-zeroes-p hash)
        n
        (find-lowest-five key (+ n 1)))))

(defun find-lowest-six (key n)
  (let ((hash (md5-hex (concatenate 'string key (write-to-string n)))))
    (if (six-zeroes-p hash)
        n
        (find-lowest-six key (+ n 1)))))

(defun solve-day04 ()
  "The Ideal Stocking Stuffer"
  (let ((puzzle-input "ckczppom"))
    (let ((lowest-n (find-lowest-five puzzle-input 1))
          (lowest-m (find-lowest-six  puzzle-input 1)))
      (format t "2015 day 4 part 1: ~a~%" lowest-n)
      (format t "2015 day 4 part 2: ~a~%" lowest-m))))
