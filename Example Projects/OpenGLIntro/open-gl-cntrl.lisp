;;; open-gl-cntrl.lisp

#|
The MIT license.

Copyright (c) 2014 Paul L. Krueger

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
  (require :open-gl-classes)
  (require :open-gl-view)
  (require :lisp-controller)
  (require :constraint-layout))

(in-package :ogl)

#|
;; The ogl-win-controller class

(defclass ogl-win-controller (lisp-window-controller)
  ((ogl-view :accessor ogl-view))
  (:metaclass ns:+ns-object))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the build-function for the ogl window

(defmethod make-ogl-window ((oc ogl-win-controller))
  (with-slots (ogl-view) oc
    (let* ((ogl-view (make-instance 'ogl-graphic-view
                       :doc (#/document oc)))
           (win (make-instance ns:ns-window
                  :title "Open GL Test Window"
                  :resizable t
                  :content-subviews (list ogl-view)))
           (cv (#/contentView win)))

      ;; anchor the ogl-view to the window
      (anchor cv ogl-view '(:top :leading :trailing :bottom))

      (values win (list ogl-view win)))))

(provide :open-gl-cntrl)
