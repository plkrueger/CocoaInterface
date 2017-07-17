;; squigg-view.lisp

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

#|
(defclass squigg-view (ns:ns-view)
  ((document :accessor document
             :initarg :document))
  (:default-initargs
    :document nil)
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/drawRect: :void)
                ((self squigg-view) (rect #>NSRect))
  ;; this is broken up so the same draw function can be called for printing
  (draw-in-view-rect self (document self) (#/bounds self) rect))

(defmethod draw-in-view-rect ((self ns:ns-view) (doc squiggle-doc) bounds rect)
  (declare (ignore rect))
  (let ((rotations (rotations doc))
        (initial-transform (#/transform ns:ns-affine-transform))
        (transform (#/transform ns:ns-affine-transform))
        (my-bounds (#/bounds self))
        (gradient (#/initWithStartingColor:endingColor:
                   (#/alloc ns:ns-gradient)
                   (#/blackColor ns:ns-color)
                   (back-color doc))))
    ;; Draw the background gradient.
    (#/drawInRect:angle: gradient my-bounds 45.0)
    ;; For printing, center everything on the print view. This does nothing in a window where
    ;; mybounds and bounds are the same.
    (#/translateXBy:yBy: initial-transform
                         (/ (- (ns:ns-rect-width my-bounds)
                               (ns:ns-rect-width bounds))
                            (gui::cgfloat 2.0))
                         (/ (- (ns:ns-rect-height my-bounds)
                               (ns:ns-rect-height bounds))
                            (gui::cgfloat 2.0)))
    (#/concat initial-transform)
    ;; Create a coordinate transformation based on the value of the rotation slider to be 
    ;; repeatedly applied below. Rotate around the center point of the displayed view by 
    ;; translating before rotating and then translating back. Note that if we rotated around
    ;; the center of the print view as well, we'd get something that looked pretty different when
    ;; printed than it did in the original window.
    (#/translateXBy:yBy: transform
                         (/ (ns:ns-rect-width bounds) (gui::cgfloat 2.0))
                         (/ (ns:ns-rect-height bounds) (gui::cgfloat 2.0)))
    (#/rotateByDegrees: transform (/ (gui::cgfloat 360.0) rotations))
    (#/translateXBy:yBy: transform
                         (/ (ns:ns-rect-width bounds) (gui::cgfloat -2.0))
                         (/ (ns:ns-rect-height bounds) (gui::cgfloat -2.0)))
    ;; for each rotation draw all the squiggles
    (dotimes (i rotations)
      (dolist (squiggle (squiggle-list doc))
        (let ((path (path squiggle)))
          (#/setLineWidth: path (thickness squiggle))
          (#/set (color squiggle))
          (#/stroke path)))
      ;; now apply the transform to rotate a little more for the next iteration
      (#/concat transform))))

;; Now override two of NSResponder's mouse handling methods to address only the events we want

;; Start drawing a new squiggle when mouse down is delivered to us

(objc:defmethod (#/mouseDown: :void)
                ((self squigg-view) (event :id))
  (unless (eql (document self) (%null-ptr))
    ;; convert from the window's coordinate system to this view's coordinates and start a new squiggle there
    (start-new-squiggle (document self)
                        (#/convertPoint:fromView: self
                                                  (#/locationInWindow event)
                                                  (%null-ptr)))))

(objc:defmethod (#/mouseDragged: :void)
                ((self squigg-view) (event :id))
  (unless (eql (document self) (%null-ptr))
    ;; convert from the window's coordinate system to this view's coordinates and continute the squiggle there
    (continue-squiggle (document self)
                       (#/convertPoint:fromView: self
                                                 (#/locationInWindow event)
                                                 (%null-ptr)))))

(objc:defmethod (#/isOpague #>BOOL)
                ((self squigg-view))
  #$YES)

(provide :squigg-view)

