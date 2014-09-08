;; based on ANSI Common Lisp - Chapter 14

;;; A package is a Lisp object that maps names to symbols.
;;; The current package is always stored in the global variable *package*
(package-name *package*) ;; will be "COMMON-LISP-USER"
(find-package "COMMON-LISP-USER") ;; returns the package with a given name

;;; Usually a symbol is interned in the package that was current at the time it was read
;;; Interestingly, this expression returns the value although we don't define symbol 'sym
;;; Because it has to be read before it could be evaluated
;;; and reading expression caused sym to be interned
(symbol-package 'sym) ;; returns the package in which the symbol is interned
(setq sym 3)

(setf *package* (make-package '1ambda
			      :use '(COMMON-LISP)))
;;; If you want to evaluate symbol `sym` use
COMMON-LISP-USER::sym

;;; But using double colon to get symbol in another package is bad practice
;;; By doing so, you can violate the modularity that packages are supposed to provide
;;; you should use colon and should export symbol before using it
(in-package common-lisp-user)
(export 'bar)
(setf bar 5)
(in-package 1ambda)

common-lisp-user:bar ;; will give 5
common-lisp-user:sym ;; will give error

;;; also, we can import bar in common-lisp-user package
(import 'common-lisp-user:bar)

;;; if you want import all symbols in a package, try 'use-package'
(use-package 'common-lisp-user)

;;; package is just the symbol container
