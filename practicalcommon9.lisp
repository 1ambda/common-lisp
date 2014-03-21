(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form)
  result)

(defmacro check (&body forms)
  `(progn
     (and
      ,@(loop for form in forms
	     collect `(report-result ,form ',form)))))

(defun test+- ()
  (check
    (= (+ 1 4) 1)
    (= (+ 1 1) 2)))
