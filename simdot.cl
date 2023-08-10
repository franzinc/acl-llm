;;;; simdot.cl
(in-package #:cl-user)

(defpackage #:simdot
  (:use #:cl #:excl #:ff)
  (:export #:simdot))

(in-package #:simdot)

(eval-when (:compile-toplevel)
 (declaim (optimize (speed 3) (safety 0) (space 0))))

(def-foreign-call (.simdot. "simdot") ((a (* :float))
                                       (b (* :float))
                                       (n :int))
  :returning :float
  :arg-checking nil
  :call-direct t)

(defun simdot (a b)
  (declare (type (simple-array single-float (*)) a b))
  (the single-float (.simdot. a b (length a))))
