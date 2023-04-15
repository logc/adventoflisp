(defun asdf/load-local (filename)
  (asdf:load-asd (concatenate 'string (namestring (uiop:getcwd)) filename)))
