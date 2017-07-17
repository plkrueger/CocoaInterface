;; open-gl-view.lisp

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

;; This is a fairly direct translation to Lisp from Apple's example code: 
;;     http://developer.apple.com/library/mac/#samplecode/CocoaGL/Introduction/Intro.html

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :open-gl-classes))

(in-package :ogl)

#|
(defclass ogl-graphic-view (ns:ns-opengl-view)
  ((ogl-doc :accessor ogl-doc
            :initarg :doc))
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/drawRect: :void)
                ((self ogl-graphic-view) (rect #>NSRect))
  ;; this is broken up so the same draw function can be called for printing
  (draw-in-view-rect self (ogl-doc self) (#/bounds self) rect))

(defmethod draw-in-view-rect ((self ns:ns-view) (doc ogl-graphic) bounds rect)
  (declare (ignore doc bounds rect))
  (#_glClearColor 0.0 0.0 0.0 0.0)
  (#_glClear #$GL_COLOR_BUFFER_BIT)
  (draw-an-object)
  (#_glFlush))

(defun draw-an-object ()
  (#_glColor3f 1.0 0.85 0.35)
  (#_glBegin #$GL_TRIANGLES)
  (#_glVertex3f 0.0 0.6 0.0)
  (#_glVertex3f -0.2 -0.3 0.0)
  (#_glVertex3f 0.2 -0.3 0.0)
  (#_glEnd))
    
(provide :open-gl-view)
