;; doc-controller-hash.lisp

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

;; defines a structure and methods for mapping classes to doc-controllers that manage them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize))

(in-package :iu)
   
(defun ensure-class-name (thing)
  (cond ((symbolp thing)
         thing)
        ((stringp thing)
         (and (internable-string-p thing) (read-from-string thing)))
        ((subtypep (type-of thing) 'class)
         (class-name thing))
        (t
         (error "Argument to ensure-class-name should be a symbol, internable string, or class; not ~a" thing))))

(let ((doc-ctrl-hash (make-hash-table)))

  (defun doc-controller-for-class (doc-class-name)
    (gethash (ensure-class-name doc-class-name) doc-ctrl-hash))

  (defun set-doc-controller-for-class (doc-class-name doc-ctrl)
    (setf (gethash (ensure-class-name doc-class-name) doc-ctrl-hash) doc-ctrl))

)

(provide :doc-controller-hash)
