;; hello.lisp

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

#|

Implements a simple "hello world" window

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :window-utils)
  (require :window-controller)
  (require :text-views)
  (require :constraint-layout))

(in-package :hello)

(defmethod make-hello-window ((lc lisp-window-controller))
  (let* ((hello-label (make-instance 'label-view
                        :title "Hello World!"))
         (win (make-instance 'ns:ns-window
                :title "Hello World"
                :resizable t
                :content-subviews (list hello-label)))
         (cview (#/contentView win)))

    ;; constrain the size of the label to the minimum necessary
    (constrain-to-natural-size hello-label)

    ;; make sure the content view is always large enough to contain the label
    (constrain-size-relative-to cview hello-label :rel :>=)

    ;; align the center of the label with the center of the window in both x and y
    ;; directions
    (center-in-view cview hello-label)

    ;; Return the window, the objects created, and nil since we have added 
    ;; no objects for the window-controller to manage.
    (values win (list win hello-label))))

(defun hello ()
  (let ((wc (make-instance 'lisp-window-controller
              :build-method #'make-hello-window)))
    (show-window wc)
    wc))

(provide :hello)
