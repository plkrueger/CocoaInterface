;; preferences.lisp

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

;; support for user preferences in Lisp Cocoa applications

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :ns-object-utils))

(in-package :iu)

(defun user-default (key-string &key (lisp-class nil))
  (ns-to-lisp-object (#/objectForKey: (#/standardUserDefaults ns:ns-user-defaults)
                                      (lisp-to-temp-nsstring key-string))
                     :lisp-class lisp-class))

(defun set-user-default (key-string value)
  (let* ((trans-val (lisp-to-ns-object value))
         (val-type (type-of trans-val))
         (val (if (or (subtypep val-type 'ns::ns-data)
                      (subtypep val-type 'ns::ns-string)
                      (subtypep val-type 'ns::ns-number)
                      (subtypep val-type 'ns::ns-array)
                      (subtypep val-type 'ns::ns-dictionary)
                      (subtypep val-type 'ns::ns-calendar-date))
                trans-val
                (#/archiveDataWithRootObject: ns::ns-keyed-archiver val)
  (#/setObject:forKey: (#/standardUserDefaults ns:ns-user-defaults)
                       (lisp-to-ns-object value)
                       (lisp-to-temp-nsstring key-string)))


(provide :preferences)