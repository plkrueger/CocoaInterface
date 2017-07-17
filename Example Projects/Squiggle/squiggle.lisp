;; squiggle.lisp

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

;; This is a fairly direct translation to Lisp from Apple's example code: 
;;     http://developer.apple.com/mac/library/samplecode/Squiggles/Introduction/Intro.html

(in-package :sq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; squiggle class

#|
(defclass squiggle ()
  ((path :accessor path :initform nil)
   (color :accessor color :initform (#/retain (random-color)))
   (thickness :accessor thickness :initform (+ 1.0 (random 3.0)))))
|#

;; Construct or continue extending this Squiggle
(defmethod add-point ((self squiggle) point)
  (with-slots (path) self
    (if path
      (#/lineToPoint: path point)
      (progn
        (setf path (make-instance ns:ns-bezier-path))
        (#/moveToPoint: path point)))))

(provide :squiggle)
