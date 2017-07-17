;; opengl-utils.lisp

#|
The MIT license.

Copyright (c) 2011 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :ogl)

;; *ogl-vector* is a per-thread variable that allocs memory used for staging
;; vector values that are passed into opengl functions. The OpenGL documentation
;; says that all passed in values are immediately copied, so this space can be
;; immediately reused. We malloc enough space for 4 64-bit values.

(defvar *ogl-vector* (#_malloc 32))

(defmacro ogl-vector (lisp-seq)
  (let ((lisp-vector (gensym)))
    `(let ((,lisp-vector (coerce ,lisp-seq '(simple-array single-float))))
       (ccl::%copy-ivector-to-ptr ,lisp-vector 0 *ogl-vector* 0 (* 4 (length ,lisp-vector)))
       *ogl-vector*)))

(defmacro do-foreign-array ((foreign-ptr-sym arr-ptr arr-type arr-count &optional result-form) &body body)
  (let ((indx-sym (gensym))
        (type-size-sym (gensym)))
    `(let ((,type-size-sym (ccl::foreign-size ,arr-type :bytes)))
       (with-macptrs ((,foreign-ptr-sym ,arr-ptr))
         (dotimes (,indx-sym ,arr-count ,result-form)
           ,@body
           (%incf-ptr ,foreign-ptr-sym ,type-size-sym))))))

(provide :opengl-utils)