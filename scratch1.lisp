

(defun foo () (write-line "Global foo"))

(funcall #'foo) ; => Global foo
(funcall 'foo) ; => Global foo

(flet ((foo () (write-line "Local foo")))
  (funcall #'foo) ; => Local foo
  (funcall 'foo)) ; => Global foo
