(in-package :adventoflisp)

(defun str/head (a-string) (char a-string 0))

(defun str/second (a-string) (char a-string 1))

(defun str/rest (a-string) (subseq a-string 1 (length a-string)))

(defun str/empty-p (a-string) (= (length a-string) 0))

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

(defun count-char (a-char a-string)
  (count a-char a-string :test #'equal))

(defun count-vowels (a-string)
  (+ (count-char #\a a-string)
     (count-char #\e a-string)
     (count-char #\i a-string)
     (count-char #\o a-string)
     (count-char #\u a-string)))

(defun contains-three-vowels-p (a-string)
  (>= (count-vowels a-string) 3))

(defun contains-repeated-p (a-string)
  (cond ((< (length a-string) 2) nil)
        ((char= (str/head a-string) (str/second a-string)) t)
        (t (contains-repeated-p (str/rest a-string)))))

(defun not-contains-forbidden-p (a-string)
  (not (or (search "ab" a-string)
           (search "cd" a-string)
           (search "pq" a-string)
           (search "xy" a-string))))

(defun is-nice-p (a-string)
  (and (contains-three-vowels-p a-string)
       (contains-repeated-p a-string)
       (not-contains-forbidden-p a-string)))

(defun count-nice (strings &optional (cnt 0))
  (cond ((= (length strings) 0) cnt)
        ((is-nice-p (first strings)) (count-nice (rest strings) (+ cnt 1)))
        (t (count-nice (rest strings) cnt))))

(defun contains-pair-twice-p (a-string)
  (let ((seen (make-hash-table :test #'equal)))
    (loop for i from 0 below (1- (length a-string))
	  for bigram = (subseq a-string i (+ i 2))
	  do (if (and (gethash bigram seen)
		      ;; prevent overlapping bigrams from returning true
		      (< (gethash bigram seen) (1- i)))
		 (return t)
		 (setf (gethash bigram seen) i)))))

(defun contains-repeated-inbetween-p (a-string)
  (loop for i from 0 below (- (length a-string) 2)
	for trigram = (subseq a-string i (+ i 3))
	when (char= (aref trigram 0) (aref trigram 2))
	do (return t)
	finally (return nil)))

(defun count-nice-again (strings)
  (loop for s in strings
	count (and (contains-pair-twice-p s)
		   (contains-repeated-inbetween-p s))))

(defun str->pairs (a-string &optional (acc nil))
  (cond ((str/empty-p a-string) acc)
        ((< (length a-string) 2) acc)
        (t (let* ((a (str/head a-string))
                  (b (str/second a-string))
                  (p (cons a b)))
             (str->pairs (str/rest a-string) (cons p acc))))))

(defun char-pair-equal (char-pair-a char-pair-b)
  (and (char= (first char-pair-a) (first char-pair-b))
       (char= (second char-pair-a) (second char-pair-b))))
