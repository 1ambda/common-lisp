(defun primep (number)
  (loop for x from 2 to (isqrt number)
       never (zerop (mod number x))))

(defun next-prime (number)
  (loop for x from number when (primep x)
       return x))

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(do-primes (p 0 19)
  (format t "~d " p))
