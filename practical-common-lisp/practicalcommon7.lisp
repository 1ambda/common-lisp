
; Chapter 7 - MACROS : STANDARD CONTROL CONSRUCTS

;; WHEN and UNLESS
(defmacro when1 (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro unless1 (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; COND
(defmacro cond1 (&rest body)
  (when body
    (let ((clause (first body)))
	  `(if ,(first clause) (progn ,@(rest clause))
	       (cond1 ,@(rest body))))))

;; AND, OR, and NOT
(defmacro and1 (x &rest other)
  `(if ,(not other)
       ,x
       (if ,x (and1 ,@other) nil)))

(defmacro or1 (x &rest other)
  `(if ,(not other)
       ,x
       (if ,x ,x (or1 ,@other))))

;; DOTIMES and DOLIST
(dolist (x '(1 2 3)) (princ x) (princ x)) ;; 112233
(dotimes (x 4) (princ x) (princ x)) ;; 00112233
(dotimes (x 4) (print x) (if (= (/ x 2) 0) (return))) ;; 0 1 2
(dotimes (x 20)
	   (dotimes (y 20)
	     (format t "x: ~d, y: ~d~%" x y)))

;; DO
(do ((n 0 (1+ n))
	      (cur 0 next)
	      (next 1 (+ cur next)))
	     ((= 10 n) cur))

