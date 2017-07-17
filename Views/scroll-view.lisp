;; scroll-view.lisp

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
This implements a make-scroll-view method for creating and initializing ns:ns-scroll-view objects.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :lv-classes)
  (require :text-views)
  (require :attributed-strings)
  (require :coerce-obj))

(in-package :lv)

#|
(defclass vscrolled-text-view (ns:ns-scroll-view)
  ((view-stream :initform nil))
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self vscrolled-text-view)
                                       &key
                                       (text nil)
                                       (background-color nil)
                                       (selectable t)
                                       (editable t))
  ;; Initializes a ns-scroll-view with a ns-text-view as its content. The text width is
  ;; constrained to be the same as the scroller's content view and its height is
  ;; constrained to be at least as high as the content view.

  ;; text argument can be either a string or attributed-string instance
  (let* ((tv (make-instance 'ns:ns-text-view
               :editable editable
               :selectable selectable
               :autoresizing-mask (list :width-sizable :height-sizable))))
    (#/setDocumentView: self tv)
    (#/setHasVerticalScroller: self t)
    (when text
      (#/setAttributedString: (#/textStorage tv)
                              (coerce-obj text 'ns:ns-attributed-string)))
    (if background-color
      (#/setBackgroundColor: tv background-color)
      (#/setDrawsBackground: tv #$NO))))

(defmethod view-stream ((self vscrolled-text-view))
  (or (slot-value self 'view-stream)
      (let ((tv (#/documentView self)))
        (setf (slot-value self 'view-stream)
              (make-instance 'attributed-string
                :str (#/textStorage tv)
                :view tv)))))

(provide :scroll-view)