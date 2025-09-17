(in-package :adventoflisp/test)

(in-suite helpers-test)

(test contains-pair-twice
  (is (eql t (aoc::contains-pair-twice-p "xyxy")))
  (is (not (eql t (aoc::contains-pair-twice-p "aaa")))))

(test parse-instruction-turn-on
  (let ((instr (aoc::parse-instruction "turn on 0,0 through 999,999")))
    (is (typep instr 'aoc::instruction))
    (is (eql :turn-on (aoc::instruction-op instr)))
    (is (= 0 (aoc::point-x (aoc::instruction-start instr))))
    (is (= 0 (aoc::point-y (aoc::instruction-start instr))))
    (is (= 999 (aoc::point-x (aoc::instruction-end instr))))
    (is (= 999 (aoc::point-y (aoc::instruction-end instr))))))


(test grid-tests
   (let ((g (aoc::make-grid 3 3)))
     (is (= 3 (aoc::grid-nrows g)))
     (is (= 3 (aoc::grid-ncols g)))
     (is (= 0 (aoc::grid-ref g (random 3) (random 3))))
     (let ((random-x (random 3))
	   (random-y (random 3)))
       (is (= 1 (progn
		  (aoc::setf (aoc::grid-ref g random-x random-y) 1)
		  (aoc::grid-ref g random-x random-y))))
       (is (= 1 (aoc::grid-count-on g)))
       (aoc::grid-turn-off g random-x random-y)
       (is (= 0 (aoc::grid-count-on g)))
       (aoc::grid-turn-on g random-x random-y)
       (is (= 1 (aoc::grid-count-on g)))
       (aoc::grid-toggle g random-x random-y)
       (is (= 0 (aoc::grid-count-on g))))
     (aoc::turn-on-range g 0 0 2 2)
     (is (= 9 (aoc::grid-count-on g)))
     (aoc::turn-off-range g 0 0 2 2)
     (is (= 0 (aoc::grid-count-on g)))))
