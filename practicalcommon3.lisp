(defvar *person-db* nil)

(defun make-person (name age address)
  (list :name name :age age :address address))

(defun add-person (person)
  (push person *person-db*))

(defun dump-person-db ()
  (dolist (person *person-db*)
    (format t "~{~a:~10t~a~%~}~%" person))) 

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-person ()
  (make-person
   (prompt-read "Name")
   (or (parse-integer (prompt-read "Age")  :junk-allowed t) 0)
   (prompt-read "Address")))
   
(defun add-people ()
  (loop (add-person (prompt-for-person))
	(if (not (y-or-n-p "Another [y/n] : ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
		  (with-standard-io-syntax (print *person-db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
		  (with-standard-io-syntax (setf *person-db* (read in)))))

(defun select (selector)
  (remove-if-not selector *person-db*))

(defmacro where (&rest args)
  `#'(lambda (person)
       (and ,@(loop while args
		    collect `(equal (getf person ,(pop args)) ,(pop args))))))

(defmacro update (selector &rest args)
  `(setf *person-db*
	 (mapcar #'(lambda (person)
		     (when (funcall ,selector person)
		       ,@(loop while args
			       collect `(setf (getf person ,(pop args)) ,(pop args))))
		     person)
		 *person-db*)))

(defun delete-person (selector)
  (setf *person-db* (remove-if selector *person-db*)))
		      
		      


