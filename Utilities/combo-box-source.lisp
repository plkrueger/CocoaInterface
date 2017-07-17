;; combo-box-source.lisp


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

;; This provides the combo-box-source class and methods for it that make it easy to provide
;; a data source object for pop-up menus attached to combo-boxes in a user interface. In
;; addition this class acts as a delegate for NSComboBox objects so that it can update the 
;; menu just before it is opened. You must add an instance of some subclass of combo-box-source
;; to your nib file in IB and link this object as the delegate of the target combo-box in the
;; interface window.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes)
  (require :ns-string-utils))

(in-package :iu)

#|
(defclass combo-box-source (ns:ns-object)
  ((source-items :accessor source-items
                 :initform nil))
  (:metaclass ns:+ns-object))
|#

(defmethod combo-box-source-items ((self combo-box-source))
  ;; This method should be defined in your subclass to return a sequence of items
  ;; that can be coerced to strings via a funcall to #'string.
  nil)

(defmethod update-items ((self combo-box-source))
  (let ((new-items (mapcar #'string (combo-box-source-items self))))
    (when (not (equal new-items (source-items self)))
      (setf (source-items self) new-items))))

;; Methods needed to be a NSComboBoxDataSource

(objc:defmethod (#/numberOfItemsInComboBox: #>NSInteger)
                ((self combo-box-source) (cbox :id))
  (declare (ignore cbox))
  (update-items self)
  (list-length (source-items self)))

(objc:defmethod (#/comboBox:objectValueForItemAtIndex: :id)
                ((self combo-box-source) (cbox :id) (indx #>NSInteger))
  (declare (ignore cbox))
  (lisp-to-temp-nsstring (elt (source-items self) indx)))

;; NSComboBox delegate methods

(objc:defmethod (#/comboBoxWillPopUp: :void)
                ((self combo-box-source) (notif :id))
  (let ((cbox (#/object notif)))
    (when (update-items self)
      (#/noteNumberOfItemsChanged cbox))))

(provide :combo-box-source)
    