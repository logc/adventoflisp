(defsystem adventoflisp
  :name "Solutions to Advent of Code puzzles"
  :version "0.1.0"
  :author "Luis Osa <luis.osa.gdc@gmail.com>"
  :depends-on (#:alexandria #:md5)
  :components ((:file "package")
               (:file "solutions")
               (:file "helpers"))
  :in-order-to ((test-op (test-op :adventoflisp/test)))

  )

(defsystem adventoflisp/test
  :depends-on (#:adventoflisp #:fiveam)
  :components ((:module "tests"
		:components ((:file "package")
			     (:file "solutions-test"))))
  :perform (test-op (o c)
		    (uiop:symbol-call :fiveam :run-all-tests)))
