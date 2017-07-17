;; squiggle-classes

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

(defpackage :squiggle
  (:nicknames :sq)
  (:use :iu :ccl :common-lisp :lv))

(in-package :sq)

(defclass squiggle-doc (lisp-document)
  ((squiggle-list :accessor squiggle-list
                  :initform nil)
   (rotations :accessor rotations
              :initform 1
              :kvo "rotations"
              :undo "set rotations")
   (squiggle-view :accessor squiggle-view
                  :initform nil)
   (squiggle-win :accessor squiggle-win
                 :initform nil)
   (back-color-well :accessor back-color-well
                    :initform nil)
   (back-color :accessor back-color
               :initform (#/blueColor ns:ns-color)
               :kvo "backColor"
               :undo "set background color")
   (rotation-slider :accessor rotation-slider
                    :initform nil)
   (window-frame :accessor window-frame
                 :initform (list 200 200 400 400))
   (notif-handler :accessor notif-handler
                  :initform nil))
  (:metaclass ns:+ns-object))

(defclass squigg-view (ns:ns-view)
  ((document :accessor document
             :initarg :document))
  (:default-initargs
    :document nil)
  (:metaclass ns:+ns-object))

(defclass squiggle ()
  ((path :accessor path :initform nil)
   (color :accessor color :initform (#/retain (random-color)))
   (thickness :accessor thickness :initform (+ 1.0 (random 3.0)))))

(provide :squiggle-classes)