;; file-directory-utils.lisp

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

;; some directory/file search utility functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes))

(in-package :iu)

(defun find-in-ccl (fname &key (directories nil) (files t))
  (probe-file (first (directory (concatenate 'string "ccl:**;" (string fname))
                                :directories directories
                                :files files))))

(defun find-nib-in-ccl (fname)
  (find-in-ccl fname :directories t :files t))


(defun find-in-contrib (fname &key (directories nil) (files t))
  (let ((f (first (directory (concatenate 'string "ccl:contrib;**;" (string fname))
                             :directories directories
                             :files files))))
    (when f (probe-file f))))

(defun find-nib-in-contrib (fname)
  (find-in-contrib fname :directories t :files t))


(defun find-in-krueger-contrib (fname &key (directories nil) (files t))
  (let ((f (first (directory (concatenate 'string "cocoa-pk:**;" (string fname))
                             :directories directories
                             :files files))))
    (when f (probe-file f))))

(defun find-nib-in-krueger-contrib (fname)
  (find-in-contrib fname :directories t :files t))

(provide :file-directory-utils)
