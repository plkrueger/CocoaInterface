;;; simplesum.lisp

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

;;; Sample lisp/Cocoa interface that builds and shows a simple window with a button and
;;; some controls.
;;; A definition is provided for the "sum-window-data" class and the window build function creates an
;;; instance that will be managed by the window-controller. That instance is also linked
;;; appropriately to the input fields, output sum text field, and Sum button.
;;; When the Sum button is pushed the sum will be computed and placed into the sum 
;;; field in the window.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :button)
  (require :organized-box-view)
  (require :window-utils)
  (require :text-views)
  (require :constraint-layout)
  (require :window-controller))

(in-package :simplesum)

(defclass sum-window-data (ns:ns-object)
  ((input1 :accessor input1
           :initarg :input1)
   (input2 :accessor input2
           :initarg :input2)
   (sum :accessor sum
        :initarg :sum))
  (:metaclass ns:+ns-object))

;; methods called as a result of button actions

(objc:defmethod (#/doSum: :void) 
                ((self sum-window-data) (s :id))
  (declare (ignore s))
  (with-slots (input1 input2 sum) self
    (#/setIntValue: sum (+ (#/intValue input1) (#/intValue input2)))))

(defmethod make-sum-window ((lc lisp-window-controller))
  (let* ((arg1 (make-instance 'ns:ns-text-field
                 :alignment :right
                 :frame-size '(60 21)))
         (arg2 (make-instance 'ns:ns-text-field
                 :alignment :right
                 :frame-size '(60 21)))
         (sum (make-instance 'ns:ns-text-field
                :alignment :right
                :frame-size '(60 21)
                :selectable nil))
         (box (make-instance 'organized-box-view
                :views (list arg1 25 arg2)))
         (data-holder (make-instance 'sum-window-data
                        :input1 arg1
                        :input2 arg2
                        :sum sum))
         (sum-button (make-instance 'ns:ns-button
                       :title "Compute Sum"
                       :bezel-style :round-rect
                       :button-type :momentary-push-in
                       :target data-holder
                       :action "doSum:"))
         (win (make-instance 'ns:ns-window
                :title "Simple Sum"
                :resizable t
                :content-subviews (list box sum sum-button)))
         (cview (#/contentView win)))

    ;; constrain the size of the fields to the minimum necessary
    ;; we don't want them changing in response to changes of the window size
    ;; arg1 and arg2 are already constrained when we put them in the invisible box
    (constrain-to-natural-size sum)
    (constrain-to-natural-size sum-button)

    ;; Put the button in the center of the window
    (center-in-view cview sum-button)

    ;; Vertically align the box, the button, and the sum field
    (order-views :orientation :v
                 :views (list box 20 sum-button 20 sum)
                 :align :center-x)

    ;; make sure the content view is always large enough to contain all the objects
    (constrain (>= (leading box) (+ (leading cview) 20)))
    (constrain (>= (trailing cview) (+ (trailing box) 20)))
    (constrain (<= (top cview) (- (top box) 20)))
    (constrain (>= (bottom cview) (+ (bottom sum) 20)))

    ;; Return the window, the list of objects created
    (values win (list arg1 arg2 sum box data-holder sum-button win))))

(defun test-sum ()
  (on-main-thread
   (let ((wc (make-instance 'lisp-window-controller
               :build-method #'make-sum-window)))
     (show-window wc)
     wc)))

(provide :simplesum)