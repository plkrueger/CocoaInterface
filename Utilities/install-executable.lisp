;; install-executable.lisp

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

;; This file is required by a subordinate CCL process that just loads as little as necessary and then
;; saves itself into an application bundle as directed by by the superordinate lisp process. See
;; lisp-app-doc.lisp function install-executable.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize)
  (require :custom-app-init)
  (require :save-gui-app))

;; app-dev will the package within which we execute some code in a subordinate CCL. This is mostly
;; a convenience to make it easy to use a remote-let call from functions in the app-dev package.

(in-package :iu)

(defmacro with-errors-reported (&body forms)
  ;; returns either the result of the execution of forms or a string with the error
  (let ((res (gensym)))
    `(let ((,res nil))
       (handler-case (setf ,res (progn ,@forms))
         (error (c)
                (setf ,res (format nil "~a" c))))
       ,res)))

(defmacro make-default-type-method (type-string)
  `(objc:defmethod  (#/defaultType :id)
                    ((self gui::hemlock-document-controller))
     (#/autorelease (objc::make-nsstring ,type-string))))

(provide :install-executable)
