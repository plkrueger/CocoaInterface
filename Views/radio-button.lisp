;; radio-button.lisp

#|
The MIT license.

Copyright (c) 2013 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subjec to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

#|
This implements a radio-button-controller class that handles the task of making sure that only
one of a set of radio buttons is set.

Also implemented are subclasses of organized-box-view and resizable-box-view that have radio
buttons and a radio-button-controller within them.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :organized-box-view)
  (require :button)
  (require :coerce-obj))

(in-package :lv)

(defclass radio-button-controller (button-controller)
  ((current-pushed :accessor current-pushed
                   :initform nil))
  (:default-initargs
    :button-type :radio)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self radio-button-controller)
                                       &key
                                       (default nil)
                                       &allow-other-keys)
  (with-slots (buttons current-pushed) self
    (let ((def-button (or (and default 
                               (find default buttons
                                     :test #'string=
                                     :key #'(lambda (b)
                                              (coerce-obj (#/title b) 'string))))
                          (first buttons))))
      (when def-button
        (#/setState: def-button #$NSOnState)
        (setf current-pushed def-button)))))

(objc:defmethod (#/buttonPushed: :void) 
                ((self radio-button-controller) (sender :id))
  ;; different from button-controller method in that it won't pass on repeated pushes of
  ;; the same button
  (with-slots (current-pushed target action-func) self
    (unless (eq sender current-pushed)
      (#/setState: current-pushed #$NSOffState)
      (setf current-pushed sender)
      (funcall action-func target (#/title sender) (#/tag sender)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; radio-button-box-view
;;
;; Radio buttons are arranged in a fixed size row or column (default). Set the
;; :spacing keyword argument to specify the space between the buttons.

(defclass radio-button-box-view (organized-box-view)
  ((controller :accessor controller
               :initform nil))
  (:default-initargs
    :orientation :v
    :margin (list 3 3)
    :align :leading)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self radio-button-box-view)
                                       &key
                                       (titles nil)
                                       (target nil)
                                       (action-func nil)
                                       (default nil)
                                       &allow-other-keys)
  (setf (controller self)
        (make-instance 'radio-button-controller
          :titles titles
          :default default
          :target target
          :action-func action-func))
  (add-box-subviews self (buttons (controller self))))

(objc:defmethod (#/dealloc :void)
                ((self radio-button-box-view))
  (when (and (controller self) (not (eql (controller self) (%null-ptr))))
    (#/release (controller self))
    (setf (controller self) nil))
  (call-next-method)
  (objc:remove-lisp-slots self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; radio-button-array-view
;;
;; The view adjusts how the buttons are displayed depending on its size.
;; Could be anyhing from a single horizontal row to a single vertical column
;; or an array of any shape in between.

(defclass radio-button-array-view (resizable-box-view)
  ((controller :accessor controller
               :initform nil))
  (:default-initargs
    :orientation :v)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self radio-button-array-view)
                                       &key
                                       (titles nil)
                                       (target nil)
                                       (action-func nil)
                                       (default nil)
                                       &allow-other-keys)
  (setf (controller self)
        (make-instance 'radio-button-controller
          :titles titles
          :default default
          :target target
          :action-func action-func))
  (add-box-subviews self (buttons (controller self))))

(objc:defmethod (#/dealloc :void)
                ((self radio-button-array-view))
  (when (and (controller self) (not (eql (controller self) (%null-ptr))))
    (#/release (controller self))
    (setf (controller self) nil))
  (call-next-method)
  (objc:remove-lisp-slots self))

(provide :radio-button)
