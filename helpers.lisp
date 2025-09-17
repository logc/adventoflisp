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


;; BEGIN: Grid class & public methods to manipulate it
(defclass grid ()
  ((m :initarg :num-rows
      :reader grid-nrows
      :documentation "Number of rows of the grid")
   (n :initarg :num-cols
      :reader grid-ncols
      :documentation "Number of columns of the grid")
   (elems :documentation "Private bit-vector of length m * n")))

(defmethod shared-initialize :after ((g grid) slots &key)
  (declare (ignore slots))
  (with-slots (m n elems) g
    (setf elems (make-array (* m n)
		      :element-type 'bit
		      :initial-element 0))))

(defun make-grid (m n)
  (make-instance 'grid :num-rows m :num-cols n))

(defun grid-index (g row col)
  "Compute linear index for (row, col). 0-based."
  (declare (type fixnum row col))
  (let ((n (grid-nrows g)))
    (+ col (* row n))))

(defun grid-ref (g row col)
  "Return element at position row, col"
  (with-slots (elems) g
    (aref elems (grid-index g row col))))


(defun (setf grid-ref) (value g row col)
  "Set grid element to 0 or 1."
  (with-slots (elems) g
    (setf (aref elems (grid-index g row col))
          (if (eql value 0) 0 1))))

(defun grid-turn-on (g row col)
  (setf (grid-ref g row col) 1))

(defun grid-turn-off (g row col)
  (setf (grid-ref g row col) 0))

(defun grid-toggle (g row col)
  (setf (grid-ref g row col)
        (if (eql (grid-ref g row col) 0) 1 0)))

(defun grid-count-on (g)
  (with-slots (elems) g
    (count 1 elems)))

(defun turn-on-range (g row-start col-start row-end col-end)
  "set all bits in the inclusive rectangle to 1."
  (loop for r from row-start to row-end
        do (loop for c from col-start to col-end
                 do (grid-turn-on g r c))))

(defun turn-off-range (g row-start col-start row-end col-end)
  "Set all bits in the inclusive rectangle to 0."
  (loop for r from row-start to row-end
        do (loop for c from col-start to col-end
                 do (grid-turn-off g r c))))

(defun toggle-range (g row-start col-start row-end col-end)
  "Toggle bits in the inclusive rectangle."
  (loop for r from row-start to row-end
	do (loop for c from col-start to col-end
		 do (grid-toggle g r c))))
;; END: Grid class & public methods to manipulate it

(defstruct point x y)
(defstruct instruction op start end)

(defun parse-instruction (line)
  (multiple-value-bind (whole groups)
      (cl-ppcre:scan-to-strings
       "^(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)$"
       line)
    (declare (ignore whole))
    (let ((op-str (aref groups 0)))
      (make-instruction
       :op (cond ((string= op-str "turn on")  :turn-on)
                 ((string= op-str "turn off") :turn-off)
                 ((string= op-str "toggle")   :toggle))
       :start (make-point :x (parse-integer (aref groups 1))
                          :y (parse-integer (aref groups 2)))
       :end   (make-point :x (parse-integer (aref groups 3))
                          :y (parse-integer (aref groups 4)))))))


(defun apply-instruction-grid (instruction grid)
  (cond ((eql :turn-on (instruction-op instruction))
	 (turn-on-range grid
			(point-x (instruction-start instruction))
			(point-y (instruction-start instruction))
			(point-x (instruction-end instruction))
			(point-y (instruction-end instruction))))
	((eql :turn-off (instruction-op instruction))
	 (turn-off-range grid
			 (point-x (instruction-start instruction))
			 (point-y (instruction-start instruction))
			 (point-x (instruction-end instruction))
			 (point-y (instruction-end instruction))))
	((eql :toggle (instruction-op instruction))
	 (toggle-range grid
		       (point-x (instruction-start instruction))
		       (point-y (instruction-start instruction))
		       (point-x (instruction-end instruction))
		       (point-y (instruction-end instruction))))))


;; BEGIN: Brightness grid class & public API
(defclass bright-grid ()
  ((m :initarg :num-rows
      :reader bright-grid-nrows
      :documentation "Number of rows of the bright grid")
   (n :initarg :num-cols
      :reader bright-grid-ncols
      :documentation "Number of columns of the bright grid")
   (elems :documentation "Private vector of integers of length m * n")))

(defmethod shared-initialize :after ((g bright-grid) slots &key)
  (declare (ignore slots))
  (with-slots (m n elems) g
    (setf elems (make-array (* m n)
			    :element-type 'integer
			    :initial-element 0))))

(defun make-bright-grid (m n)
  (make-instance 'bright-grid :num-rows m :num-cols n))

(defun bright-grid-index (g row col)
  "Compute linear index for (row, col). 0-based."
  (declare (type fixnum row col))
  (let ((n (bright-grid-nrows g)))
    (+ col (* row n))))

(defun bright-grid-ref (g row col)
  (with-slots (elems) g
    (aref elems (bright-grid-index g row col))))


(defun (setf bright-grid-ref) (value g row col)
  "Set bright grid element to value."
  (with-slots (elems) g
    (setf (aref elems (bright-grid-index g row col)) value)))

(defun bright-grid-turn-on (g row col)
  (let ((current (bright-grid-ref g row col)))
    (setf (bright-grid-ref g row col) (1+ current))))

(defun bright-grid-turn-off (g row col)
  (let ((current (bright-grid-ref g row col)))
    (setf (bright-grid-ref g row col)
	  (max 0 (1- current)))))

(defun bright-grid-toggle (g row col)
  (let ((current (bright-grid-ref g row col)))
    (setf (bright-grid-ref g row col) (+ current 2))))

(defun bright-grid-count (g)
  (with-slots (elems) g
    (reduce #'+ elems)))

(defun bright-turn-on-range (g row-start col-start row-end col-end)
  (loop for r from row-start to row-end
        do (loop for c from col-start to col-end
                 do (bright-grid-turn-on g r c))))

(defun bright-turn-off-range (g row-start col-start row-end col-end)
  (loop for r from row-start to row-end
        do (loop for c from col-start to col-end
                 do (bright-grid-turn-off g r c))))

(defun bright-toggle-range (g row-start col-start row-end col-end)
  (loop for r from row-start to row-end
	do (loop for c from col-start to col-end
		 do (bright-grid-toggle g r c))))
;; END: Bright grid class & public API

(defun apply-instruction-bright-grid (instruction grid)
  (cond ((eql :turn-on (instruction-op instruction))
	 (bright-turn-on-range grid
			(point-x (instruction-start instruction))
			(point-y (instruction-start instruction))
			(point-x (instruction-end instruction))
			(point-y (instruction-end instruction))))
	((eql :turn-off (instruction-op instruction))
	 (bright-turn-off-range grid
			 (point-x (instruction-start instruction))
			 (point-y (instruction-start instruction))
			 (point-x (instruction-end instruction))
			 (point-y (instruction-end instruction))))
	((eql :toggle (instruction-op instruction))
	 (bright-toggle-range grid
		       (point-x (instruction-start instruction))
		       (point-y (instruction-start instruction))
		       (point-x (instruction-end instruction))
		       (point-y (instruction-end instruction))))))
