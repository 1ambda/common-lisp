## CLOS

CLOS Brief Guide based on

1. http://www.aiai.ed.ac.uk/~jeff/clos-guide.html
2. http://www.cs.northwestern.edu/academics/courses/325/readings/clos.php
3. http://cl-cookbook.sourceforge.net/clos-tutorial/
4. http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html

### 1. class-of

Built-in Classes

- SYMBOL
- STRING
- INTEGER
- CONS
- VECTOR
- and so on

<br/>
### 2. defstruct

`defstruct` automatically define access-function and constructor

```lisp
(defstruct cat 
  (name)
  (age))

(setf c1 (make-cat :name 'blue :age 6))
(cat-age c1) ;; 6
(cat-name c1) ;; BLUE
```
<br/>

What defined by `defstruct` is `STRUCTURE-CLASS`

```lisp
(class-of c1)

;; #<STRUCTURE-CLASS CAT)
```
<br/>

### 3. defclass

defclass gives you more controll. You can specify **initform**, **initarg**, **accessor** and so on.

```lisp
;; syntax
(DEFCLASS class-name (superclass-name*)
  (slot-description*)
  class-option*)

(defclass person ()
  ((name :initarg :name
         :initform 'bill
		 :accessor person-name)
   (age :initarg :age
        :initform 26
		:accessor person-age)))

(setf p1 (make-instance 'person :name 'bob :age 26))
```
<br/>

What defined by `defclass` is `STANDARD-CLASS` which subclasses `STANDARD-OBJECT`

```lisp
(class-of p1)
(find-class 'person)

;; #<STANDARD-CLASS PERSON>
```
<br/>

### 4. Slot

Class defined by `defclass` can have slots. Slot is member-variable.

```lisp
(setf p1 (make-instance 'person :name 'bob :age 26''))
(slot-value p1 'name)

;; BOB
```
<br/>

`describe` function can be used to show instance or classes information

```lisp
(describe p1) ;; instance information
;; #<PERSON {1002E70513}>
;;   [standard-object]
;; 
;; Slots with :INSTANCE allocation:
;;   NAME  = HOON
;;   AGE   = 26

(describe 'person) ;; class information
;; COMMON-LISP-USER::PERSON
;;   [symbol]
;; 
;; PERSON names the standard-class #<STANDARD-CLASS PERSON>:
;;   Class precedence-list: PERSON, STANDARD-OBJECT, SB-PCL::SLOT-OBJECT, T
;;   Direct superclasses: STANDARD-OBJECT
;;   No subclasses.
;;   Direct slots:
;;     NAME
;;       Initargs: :NAME
;;       Readers: PERSON-NAME
;;       Writers: (SETF PERSON-NAME)
;;     AGE
;;       Initargs: :AGE
;;       Readers: PERSON-AGE
;;       Writers: (SETF PERSON-AGE)
```
<br/>

### 5. Slot Option Inheritance

`initform` Slot options can be overrided. But `initarg` and `accessor` can't

```lisp
(defclass teacher (person)
  ((subject :accessor teacher-subject
	    :initarg :subject
	    :initform "None")))

(defclass math-teacher (teacher)
  ((subject :initform "Mathmatics")))

(defvar t1 nil)
(defvar m1 nil)

(setf t1 (make-instance 'teacher :name 'bob :age 25))
(setf m1 (make-instance 'math-teacher :name 'john :age 25))

(describe t1)

;; #<TEACHER {10040F9363}>
;;   [standard-object]
;; 
;; Slots with :INSTANCE allocation:
;;   NAME     = BOB
;;   AGE      = 25
;;   SUBJECT  = "None"


(describe m1)

;; #<MATH-TEACHER {10040FE653}>
;;   [standard-object]
;; 
;; Slots with :INSTANCE allocation:
;;   NAME     = JOHN
;;   AGE      = 25
;;   SUBJECT  = "Mathmatics"
```
<br/>

### 6. Multiple Inheritance

CLOS supports **Multiple Inheritance**. But I'm not going to discuss.

If you are interested in how CLOS solves diamond problem, see [Wiki](http://en.wikipedia.org/wiki/Multiple_inheritance)

>Common Lisp CLOS attempts to provide both reasonable default behavior and the ability to override it. By default, the method with the most specific argument classes is chosen; then in the order in which parent classes are named in the subclass definition. However, the programmer can override this, by giving a specific method resolution order or stating a rule for combining methods. This is called method combination, which may be fully controlled. The MOP (metaobject protocol) also provides means to modify the inheritance, dynamic dispatch, class instantiation, and other internal mechanisms without affecting the stability of the system.

Basically, There are two rules about inheritance.

1. Each class is more specific than its superclasses.
2. For a given class, superclasses listed earlier are more specific than those listed later.

The second rule is useful in case of

```lisp
(defclass a (b c) ...)
```
<br/>
`initform` or `method` are can be from both class `b` and class `c`. But class 'b' precedes class 'c', class 'c''s `method' or `initform` will be selected.

<br/>
### 7. Generic function

In common lisp, class have only **data**. That means **code (method)** is separeted from class and is **not** tired to any class contrary to popular object-oriented language like Java, C++. This is why we call it **generic** method or function.

```lisp

;; inherit standard object
(defclass person ()
  ((name :accessor person-name
	 :initarg :name)
   (age :accessor person-age
	:initarg :age)))

;; inherit person
(defclass teacher (person)
  ((subject :accessor teacher-subject
	    :initarg :subject
	    :initform "nothing")))

;; inherit teacher
(defclass math-teacher (teacher)
  ((subject :initform "math")))

;; create instance
(setf t1 (make-instance 'teacher :name 'bob :age 25))
(setf m1 (make-instance 'math-teacher :name 'john :age 25))

;; call method

(work t1)
;; BOB is teaching nothing

(work m1)
;; JOHN is teaching math
```

<br/>
Calling method like this is somewhat different from other languages. In Java, methods are binded in a class. So we can call methods `Class.method(args)`. This is **Single dispatch**. According to [wikipedia](http://en.wikipedia.org/wiki/Dynamic_dispatch#Single_and_multiple_dispatch)

>If the decision of which version of a method to call is based entirely on the class of the object x, then this is known as single dispatch because an implementation is chosen based on a single type — the type of the instance. Single dispatch is supported by many object-oriented languages, including statically typed languages such as C++ and Java, and dynamically typed languages such as Smalltalk and Objective-C.

But in Common Lisp, Method is invoked like `(method classes args)`. CLOS select which method will be called based on **classes**. This is **Multiple dispatch**.

>In some languages such as Common Lisp and Dylan, methods or functions can also be dynamically dispatched based on the run-time type of arguments. Expressed in pseudocode, the code manager.handle(y) could call different implementations depending on the types of both objects manager and y. This is known as multiple dispatch.

<br/>
### 8. Method Combination

More than one method are applicable to a given set of arguments, the applicable methods are called **single effective method**. Common Lisp comes with predefined **standard method combination**. Also, It is possible to define new kinds of method combination. Standard method combination consists of

* primary
* :before
* :after
* :around

```lisp
(defgeneric work (teacher)
  (:documentation "do something based on their job"))

(defmethod work ((t1 teacher))
  (format t "~a is teaching ~a"
    (slot-value t1 'name)
    (slot-value t1 'subject)))

(defmethod work ((t1 math-teacher))
  (format t "~a's major is math~%"
    (slot-value t1 'name))
  (call-next-method))

(setf t1 (make-instance 'teacher :name 'bob :age 25))
(setf m1 (make-instance 'math-teacher :name 'john :age 25))

(work m1)
;; BOB is teaching nothing

(work t1)
;; JOHN's major is math
;; JOHN is teaching math
```

<br/>
If `around`, `before`, `after` keyword is not specified, the generic method is **Primary** method which is responsible for providing primary implementation. Above `work` methods are primary. `call-next-method` indicates that control should be passaed from this method to the method specialized on superclass as arguments.

```lisp
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

(defmethod work ((t1 math-teacher))
  (format t "~a's major is math~%"
    (slot-value t1 'name))
  (call-next-method))

(work t1)
;; start around for teacher class
;; BOB is preparing the class
;; BOB is teaching nothing
;; BOB has done his class
;; end around for teacher class
;; NIL

(work m1)
;; start around for teacher class
;; JOHN is preparing the class
;; JOHN's major is math
;; JOHN is teaching math
;; JOHN has done his class
;; end around for teacher class
;; NIL
```

<br/>
These `:before`, `:after`, `:around` are auxiliary methods. Superclass's auxiliary methods are automatically called when you invoke subclass's a primary method. Of course, we can define subclass's auxiliary methods.

```lisp
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

(work m1)
;; start around for math-teacher class
;; start around for teacher class
;; Math teacher JOHN is preparing the math class
;; JOHN is preparing the class
;; JOHN's major is math
;; JOHN is teaching math
;; JOHN has done his class
;; Math teacher JOHN has done his math class
;; end around for teacher class
;; end around for math-teacher class
;; NIL
```
