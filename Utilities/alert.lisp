;; alert.lisp

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
  (require :interface-packages)
  (require :ns-string-utils))

(in-package :iu)

;; Interface to the alert panel. There is always a default right button which has the
;; default label of "OK". Other buttons are optional. Title and text are optional with
;; default title being "ALERT". Return value is a keyword that tells you which button
;; was pressed or nil if some sort of error occurred.

(defun alert (&key (title "ALERT") (right "OK") (left nil) (middle nil) (text ""))
  (let ((res (#_NSRunAlertPanel (lisp-to-temp-nsstring title)
                                (lisp-to-temp-nsstring text)
                                (lisp-to-temp-nsstring right)
                                (if left (lisp-to-temp-nsstring left) (%null-ptr))
                                (if middle (lisp-to-temp-nsstring middle) (%null-ptr)))))
    (cond ((eql res #$NSAlertDefaultReturn)
           :right)
          ((eql res #$NSAlertAlternateReturn)
           :left)
          ((eql res #$NSAlertOtherReturn)
           :middle)
          ((eql res #$NSAlertErrorReturn)
           nil))))

(defmacro with-errors-alerted (&rest forms)
  `(handler-case (progn ,@forms)
     (error (c)
            (alert :text (format nil "~a" c)
                   :right "Ignored"))))
