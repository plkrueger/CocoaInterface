;; open-gl-intro.lisp

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

;; Translation to Lisp of examples found in:
;; http://developer.apple.com/library/mac/#documentation/GraphicsImaging/Conceptual/OpenGL-MacProgGuide/opengl_drawing/opengl_drawing.html
;; Duplicates Figure 2-2 from that page

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :nslog-utils)
  (require :ns-object-utils)
  (require :lisp-document)
  (require :lisp-app-delegate)
  (require :open-gl-classes)
  (require :open-gl-cntrl))

(in-package :ogl)

#|
(defclass ogl-graphic (lisp-document)
  ((graphic-list :accessor graphic-list
                 :initform nil))
  (:metaclass ns:+ns-object))
|#

(defmethod archive-slots ((self ogl-graphic))
  (declare (ignore self))
  (list 'graphic-list))

(defun init-ogl-app (app)
  ;; The function that gets called to initialize the application
  ;; It creates and installs a main menu.
  (let ((*app-name-for-menus* "OGL"))
    (#/setMainMenu: app (standard-main-menu))
    (set-windows-menu)))

(defmethod window-build-funcs ((self ogl-graphic))
  (list #'make-ogl-window))

(defmethod document-window-controller-classes ((self ogl-graphic))
  (list (find-class 'ogl-win-controller)))
  
(provide :open-gl-intro)