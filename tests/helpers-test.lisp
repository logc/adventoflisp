(in-package :adventoflisp/test)

(in-suite helpers-test)

(test contains-pair-twice
  (is (eql t (aoc::contains-pair-twice-p "xyxy")))
  (is (not (eql t (aoc::contains-pair-twice-p "aaa")))))

