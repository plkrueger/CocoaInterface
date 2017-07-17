;; menu-test.lisp

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

;;; Reprise of simplesum.lisp that also uses a window delegate and adds menu items to the CCL menubar

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :button)
  (require :organized-box-view)
  (require :window-utils)
  (require :text-views)
  (require :constraint-layout)
  (require :window-controller)
  (require :menu-utils))

(in-package :menu-test)

(defclass sum-data (ns:ns-object)
  ((input1 :foreign-type :id
           :accessor input1
           :initarg :input1)
   (input2 :foreign-type :id
           :accessor input2
           :initarg :input2)
   (sum :foreign-type :id
        :accessor sum
        :initarg :sum))
  (:metaclass ns:+ns-object))


(defmethod initialize-instance :after ((self sum-data) 
                                       &key 
                                       input1
                                       input2
                                       sum
                                       &allow-other-keys)
  
  ;; bug in CCL makes it not process :initargs for NSObject subclasses, 
  ;; so set the links explicitly here
  (setf (input1 self) input1)
  (setf (input2 self) input2)
  (setf (sum self) sum))

;; methods called as a result of button actions

(objc:defmethod (#/doSum: :void) 
                ((self sum-data) (s :id))
  (declare (ignore s))
  (with-slots (input1 input2 sum) self
    (#/setIntValue: sum (+ (#/intValue input1) (#/intValue input2)))))


;; method called as a result of selecting a menu-item and by virtue of this object
;; being the window's delegate.
(objc:defmethod (#/doFirstThing: :void) 
                ((self sum-data) (s :id))
  (declare (ignore s))
  ;;; test function for menu tests
  (#/doSum: self (%null-ptr)))

(defmethod make-sum-window ((lc lisp-window-controller))
  (let* ((arg1 (#/autorelease (make-instance 'ns:ns-text-field :frame-size '(60 0))))
         (arg2 (#/autorelease (make-instance 'ns:ns-text-field :frame-size '(60 0))))
         (sum (#/autorelease (make-instance 'ns:ns-text-field
                               :frame-size '(60 0)
                               :selectable nil)))
         (box (#/autorelease (make-instance 'organized-box-view
                               :views (list arg1 25 arg2))))
         (data-holder (make-instance 'sum-data
                        :input1 arg1
                        :input2 arg2
                        :sum sum))
         (sum-button (#/autorelease (make-instance 'ns:ns-button
                       :title "Compute Sum"
                                      :button-type :momentary-light
                                      :target data-holder
                                      :action "doSum:")))
         (win (make-instance 'ns:ns-window
                :title "Simple Sum"
                :resizable t
                :delegate data-holder
                :content-subviews (list box sum sum-button)))
         (cview (#/contentView win)))

    ;; constrain the size of the text fields to the minimum necessary
    ;; arg1 and arg2 are already constrained when we put them in the organized box
    (constrain-to-natural-size sum :install-view cview)

    ;; Put the button in the center of the window
    (center-in-view cview sum-button)

    ;; Vertically align the box, the button, and the sum field
    (order-views :orientation :v
                 :views (list box 20 sum-button 20 sum)
                 :align :center-x)

    ;; make sure the content view is always large enough to contain all the objects
    (constrain (<= (leading cview) (leading box)))
    (constrain (>= (trailing cview) (trailing box)))
    (constrain (<= (top cview) (top box)))
    (constrain (>= (bottom cview) (bottom sum)))

    ;; Return the window and nil since we have added no objects for the 
    ;; window-controller to manage.
    (values win (list data-holder win))))


(defun test-menu ()
  (on-main-thread  
   (iu::make-and-install-menu "New App Menu" 
                              '("Menu Item1" "doFirstThing:")
                              '("Menu Item2" "doSecondThing:"))
   (let* ((wc (make-instance 'lisp-window-controller
                :build-method #'make-sum-window)))
     (show-window wc)
     wc)))

(defun test2-menu ()
  (iu::make-and-install-menuitems-after "File" "New"
                                        '("New myDoc" "newMyDoc")))


#|
(iu::pop-up-select w "Pick it:" (list "a" "b" "1" "2") #'(lambda (it indx) (format t "~%~a is at ~s" it indx)))
                                         
|#

(provide :menu-test)
