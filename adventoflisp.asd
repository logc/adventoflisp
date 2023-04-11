(defsystem #:adventoflisp
  :name "Solutions to Advent of Code puzzles"
  :version "0.1.0"
  :author "Luis Osa <luis.osa.gdc@gmail.com>"
  :depends-on (#:alexandria #:md5)
  :components ((:file "package")
               (:file "solutions")))
