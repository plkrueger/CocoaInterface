;; button.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :organized-box-view)
  (require :coerce-obj)
  (require :selector-utils)
  (require :list-utils))

(in-package :lv)

(defclass lisp-button (ns:ns-button)
  ((action-func :accessor action-func
                :initarg :action-func))
  (:default-initargs
      :action-func nil)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self lisp-button)
                                       &key
                                       &allow-other-keys)
  (#/setTarget: self self)
  (#/setAction: self (get-selector "doIt:")))

(objc:defmethod (#/doIt: :void) 
                ((self lisp-button) (sender :id))
  (declare (ignore sender))
  (when (action-func self)
    (funcall (action-func self))))

(defclass button-controller (ns:ns-object)
  ((buttons :accessor buttons
            :initform nil)
   (target :accessor target
           :initarg :target)
   (action-func :accessor action-func
                :initarg :action-func))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self button-controller)
                                       &key
                                       (titles nil)
                                       (button-type :momentary-light)
                                       (bezel-style :round-rect)
                                       (target nil)
                                       (action-func nil)
                                       &allow-other-keys)
  (with-slots (buttons) self
    (let ((tag -1))
      (dolist (title titles)
        (push (make-instance 'ns:ns-button
                :title title
                :tag (incf tag)
                :button-type button-type
                :bezel-style bezel-style
                :target self
                :action "buttonPushed:")
             buttons)))
    (setf buttons (nreverse buttons))
    (setf (target self) (or target self))
    (setf (action-func self) (or action-func #'button-pushed))))

(defmethod added-class-key-values ((self (eql (find-class 'button-controller))))
  ;; provide keyword documenation for args to initialize-instance
  (list (list :titles "A list of button titles: strings or ns:ns-strings")
        (list :action-func "Lisp function called when a button is pushed. Takes three args: <target> <button-title> <button-tag>")
        (list :target "Lisp object that will be the first argument when :action-func is called")
        ;; The following specifies that doc for any other keys for the button-controller class
        ;; can be taken from the ns:ns-button class
        (list 'button-controller ns:ns-button)))

(objc:defmethod (#/dealloc :void)
                ((self button-controller))
  (when (buttons self)
    (dolist (b  (buttons self))
      (unless (eql b (%null-ptr))
        (#/release b))))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc:defmethod (#/buttonPushed: :void) 
                ((self button-controller) (sender :id))
  (with-slots (current-pushed target action-func) self
    (funcall action-func target (#/title sender) (#/tag sender))))

(defmethod button-pushed ((self button-controller) button-title button-tag)
  ;; a sample button-pushed method that just logs the fact that a button was pushed
  (ns-log-format "Button #~s (~s) pushed" button-tag button-title))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; button-box-view
;;
;; Buttons are arranged in a fixed size row or column (default). Set the
;; :spacing keyword argument to specify the space between the buttons.

(defclass button-box-view (organized-box-view)
  ((controller :accessor controller
               :initform nil))
  (:default-initargs
    :orientation :v)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self button-box-view)
                                       &key
                                       (titles nil)
                                       (button-type :momentary-push-in)
                                       (target nil)
                                       (action-func nil)
                                       (equal-width nil)
                                       (button-width nil)
                                       &allow-other-keys)
  (with-slots (controller) self
    (setf controller
          (make-instance 'button-controller
            :titles titles
            :button-type button-type
            :target target
            :action-func action-func))
    (add-box-subviews self (buttons controller))
    (cond ((numberp button-width)
           (dolist (b (buttons controller))
             (constrain (= (width b) button-width) :install-view self)))
          (equal-width
           (do* ((b-list (buttons controller)
                         (rest b-list))
                 (b1 (first b-list)
                     (first b-list))
                 (b2 (second b-list)
                     (second b-list)))
                ((null b2))
             (constrain (= (width b1) (width b2)) :install-view self))))))

(defmethod added-class-key-values ((self (eql (find-class 'button-box-view))))
  ;; provide keyword documenation for args to initialize-instance
  ;; since many of these are just passed along to the make-instance for button-controller, we'll just
  ;; grab those and add a couple of others
  (list (list 'button-box-view (find-class 'button-controller))
        (list :equal-width "t or nil")
        (list :button-width "number specifying the fixed width of all buttons")))

(objc:defmethod (#/dealloc :void)
                ((self button-box-view))
  (when (and (controller self) (not (eql (controller self) (%null-ptr))))
    (#/release (controller self))
    (setf (controller self) nil))
   (call-next-method))

(objc:defmethod (#/dealloc :void)
                ((self button-box-view))
  (when (and (controller self) (not (eql (controller self) (%null-ptr))))
    (#/release (controller self))
    (setf (controller self) nil))
  (call-next-method)
  (objc:remove-lisp-slots self))

(provide :button)