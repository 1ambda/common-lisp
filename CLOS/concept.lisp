(class-of "a")
(class-of 'a)
(class-of 12)
(class-of '(a b))
(class-of '#(a b c))

(defstruct cat 
  (name)
  (age))

(defclass person ()
  ((name :accessor person-name
	 :initarg :name)
   (age :accessor person-age
	:initarg :age)))

(defclass fruit ()
  ((price :initarg :price
	  :accessor fru-price)))

(defvar p1 nil)
(setf p1 (make-instance 'person :name 'bob :age 26))

(class-of p1)
(find-class 'person)

(describe p1)
(describe 'person)

(defclass teacher (person)
  ((subject :accessor teacher-subject
	    :initarg :subject
	    :initform "nothing")))

(defclass math-teacher (teacher)
  ((subject :initform "math")))

(defvar t1 nil)
(defvar m1 nil)

(setf t1 (make-instance 'teacher :name 'bob :age 25))
(setf m1 (make-instance 'math-teacher :name 'john :age 25))

(defgeneric work (teacher)
  (:documentation "do something based on their job"))

(defmethod work ((t1 teacher))
  (format t "~a is teaching ~a~%"
    (slot-value t1 'name)
    (slot-value t1 'subject)))

(defmethod work :before ((t1 teacher))
  (format t "~a is preparing the class~%"
    (slot-value t1 'name)))

(defmethod work :after ((t1 teacher))
  (format t "~a has done his class~%"
    (slot-value t1 'name)))

(defmethod work :around ((t1 teacher))
  (format t "start around for teacher class~%")
  (let ((result (call-next-method)))
    (format t "end around for teacher class~%")
    result))

(defmethod work :before ((t1 math-teacher))
  (format t "Math teacher ~a is preparing the math class~%"
    (slot-value t1 'name)))

(defmethod work :after ((t1 math-teacher))
  (format t "Math teacher ~a has done his math class~%"
    (slot-value t1 'name)))

(defmethod work :around ((t1 math-teacher))
  (format t "start around for math-teacher class~%")
  (let ((result (call-next-method)))
    (format t "end around for math-teacher class~%")
    result))

(defmethod work ((t1 math-teacher))
  (format t "~a's major is math~%"
    (slot-value t1 'name))
  (call-next-method))

