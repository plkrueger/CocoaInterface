;; debug-tools.lisp


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

;; miscellaneous functions that come in handy once in a while

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ns-string-utils)
  (require :selector-utils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash-table utility functions

(defun ht-keys (ht)
  ;; return all the keys in a hash-table
  (let ((keys nil))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (push k keys))
             ht)
    keys))

(defun ht-to-acons (ht)
  (let ((pairs nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) pairs))
             ht)
    pairs))

(defun print-ht (ht)
  (maphash #'(lambda (k v)
                 (format t "~%Key: ~a~%~5t~s" k v))
             ht)
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prl (thing)
  ;; print a list or Objective-C array readably in the listener
  (format t "~{~%~s~}" (iu::coerce-obj thing 'list))
  (values))