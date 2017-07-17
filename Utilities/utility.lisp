;;; utility.lisp

#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

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

(in-package :iu)

(defmacro do-sequence ((seq-elt seq &optional (return t)) &rest body)
  (let ((seq-indx (gensym)))
    `(dotimes (,seq-indx (length ,seq) ,return)
       (let ((,seq-elt (elt ,seq ,seq-indx)))
         ,@body))))

(defmethod delete-from-seq ((seq vector) thing)
  (let ((pos (position thing seq)))
    (dotimes (i (- (fill-pointer seq) pos 1))
      (setf (aref seq (+ pos i))
            (aref seq (+ pos i 1)))))
  (vector-pop seq))

(provide :utility)