(in-package :adventoflisp/test)

(in-suite not-quite-lisp)

(test what-floor-santa
  (is (= 0 (aoc::instructions->floor "(())")))
  (is (= 0 (aoc::instructions->floor "()()")))
  (is (= 3 (aoc::instructions->floor "(((")))
  (is (= 3 (aoc::instructions->floor "(()(()(")))
  (is (= 3 (aoc::instructions->floor "))(((((")))
  (is (= -1 (aoc::instructions->floor "())")))
  (is (= -1 (aoc::instructions->floor "))(")))
  (is (= -3 (aoc::instructions->floor ")))")))
  (is (= -3 (aoc::instructions->floor ")())())"))))

(test what-pos-enter-basement
  (is (= 1 (aoc::instructions->basement-pos ")")))
  (is (= 5 (aoc::instructions->basement-pos "()())"))))

(in-suite no-math)

(test square-feet-wrapping-paper
  (is (= 58 (aoc::dims->paper (aoc::parse-dimensions "2x3x4"))))
  (is (= 43 (aoc::dims->paper (aoc::parse-dimensions "1x1x10")))))

(test feet-of-ribbon
  (is (= 34 (aoc::dimensions->ribbon (aoc::parse-dimensions "2x3x4"))))
  (is (= 14 (aoc::dimensions->ribbon (aoc::parse-dimensions "1x1x10")))))

(in-suite perfectly-spherical)

(test houses-receive-present
  (is (= 2 (aoc::2dgrid/santa-pos ">")))
  (is (= 4 (aoc::2dgrid/santa-pos "^>v<")))
  (is (= 2 (aoc::2dgrid/santa-pos "^v^v^v^v^v"))))

(test robo-santa
  (is (= 3 (aoc::2dgrid/santa+robo-pos "^v")))
  (is (= 3 (aoc::2dgrid/santa+robo-pos "^>v<")))
  (is (= 11 (aoc::2dgrid/santa+robo-pos "^v^v^v^v^v"))))

(in-suite ideal-stocking-stuffer)

;; Disabled because slow, but should pass
;; (test mine-advent-coins
;;   (is (= 609043 (aoc::find-lowest-five "abcdef" 1)))
;;   (is (= 1048970 (aoc::find-lowest-five "pqrstuv" 1))))

(in-suite intern-elves)

(test how-many-nice-strings
  (is (= 1 (aoc::count-nice '("ugknbfddgicrmopn"))))
  (is (= 1 (aoc::count-nice '("aaa"))))
  (is (= 0 (aoc::count-nice '("jchzalrnumimnmhp"))))
  (is (= 0 (aoc::count-nice '("haegwjzuvuyypxyu"))))
  (is (= 0 (aoc::count-nice '("dvszwmarrgswjxmb")))))

(test count-nice-again
  (is (= 1 (aoc::count-nice-again '("qjhvhtzxzqqjkmpb"))))
  (is (= 1 (aoc::count-nice-again '("xxyxx"))))
  (is (= 0 (aoc::count-nice-again '("uurcxstgmygtbstg"))))
  (is (= 0 (aoc::count-nice-again '("ieodomkazucvgmuy")))))

(in-suite probably-fire-hazard)

(test how-many-lights-lit
  (let ((g (aoc::make-grid 1000 1000))
	(i (aoc::parse-instruction "turn on 0,0 through 999,999")))
    (aoc::apply-instruction-grid i g)
    (is (= (* 1000 1000) (aoc::grid-count-on g))))
  (let ((g (aoc::make-grid 1000 1000))
	(i (aoc::parse-instruction "toggle 0,0 through 999,0")))
    (aoc::apply-instruction-grid i g)
    (is (= 1000 (aoc::grid-count-on g))))
  (let ((g (aoc::make-grid 1000 1000))
	(i1 (aoc::parse-instruction "turn on 0,0 through 999,999"))
	(i2 (aoc::parse-instruction "turn off 499,499 through 500,500")))
    (aoc::apply-instruction-grid i1 g)
    (aoc::apply-instruction-grid i2 g)
    (is (= (- (* 1000 1000) 4)) (aoc::grid-count-on g))))
