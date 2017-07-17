;; selector-utils.lisp

#|
The MIT license.

Copyright (c) 2013 Paul L. Krueger

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

(defun get-selector (name)
  ;; we can't use the @selector macro for some uses because it requires a literal name
  ;; argument and sometimes we want to take a name passed in as a parameter and cause it
  ;; to be registered as an objective C selector name. We could do this directly with
  ;; a call to the C function #_sel_get_uid and let lisp figure it out for itself later
  ;; but we'll play nice with the objective C bridge instead which hashes these things.
  (cond ((and (typep name 'macptr) (find-selector-match name))
         name)
        ((stringp name)
         (ccl::%get-selector (ccl::ensure-objc-selector name)))
        (t
         (error "~s is not a string or valid selector" name))))

(defun find-selector-match (sel)
  (maphash #'(lambda (key val)
               (when (eql (ccl::objc-selector-%sel val) sel) 
                 (return-from find-selector-match key)))
           ccl::*objc-selectors*))

(defmethod responds-to-selector ((obj ns:ns-object) str)
   (#/respondsToSelector: obj (ccl::%get-selector (ccl::load-objc-selector  str))))

(provide :selector-utils)