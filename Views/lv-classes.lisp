;; lv-classes.lisp

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

;; Defines all the classes used within the lisp-view (lv) package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :lv)

;; rotated-text-view
;; like an ns-text-view that allows text to be slanted at an arbitrary angle

(defclass rotated-text-view (ns:ns-view)
  ((rtv-str :accessor rtv-str
            :initarg :string)
   (rtv-attributes :accessor rtv-attributes
                   :initarg :attributes)
   (rtv-v-just :accessor rtv-v-just
               :initarg :v-just) ;; one of :top :center :bottom
   (rtv-h-just :accessor rtv-h-just
               :initarg :h-just) ;; one of :left :center :right
   (rtv-v-margin :accessor rtv-v-margin
                 :initarg :v-margin)
   (rtv-h-margin :accessor rtv-h-margin
                 :initarg :h-margin)
   (rtv-angle :accessor rtv-angle
              :initform 0)
   (rtv-text-width :accessor rtv-text-width
                   :initform 0.0)
   (rtv-text-height :accessor rtv-text-height
                    :initform 0.0)
   (rtv-rotated-text-width :accessor rtv-rotated-text-width
                           :initform 0.0)
   (rtv-rotated-text-height :accessor rtv-rotated-text-height
                            :initform 0.0)
   (rtv-min-width :accessor rtv-min-width
                  :initform 0.0)
   (rtv-min-height :accessor rtv-min-height
                   :initform 0.0)
   (rtv-text-storage :accessor rtv-text-storage)
   (rtv-layout-manager :accessor rtv-layout-manager)
   (rtv-text-container :accessor rtv-text-container)
   (rtv-glyph-range :accessor rtv-glyph-range))
  (:default-initargs
    :string "Test String"
    :attributes nil
    :v-just :top
    :h-just :left
    :v-margin 0.0
    :h-margin 0.0)
  (:metaclass ns:+ns-object))

;; form-view
;; a box containing right-aligned ns-text-fields of the same width and left-aligned
;; labels for each text field. Size of text fields change as width of the box changes.
(defclass form-view (ns:ns-view)
  ((text-fields :accessor text-fields
                :initarg :text-fields)
   (enabled :accessor enabled
            :foreign-type #>BOOL))
  (:metaclass ns:+ns-object))

;; label-view
;; A simple fixed label
(defclass label-view (ns:ns-text-field)
  ()
  (:metaclass ns:+ns-object))

;; labeled-text-field
;; a box containing a label and a text-field
(defclass labeled-text-field (ns:ns-view)
  ((text-field :accessor text-field))
  (:metaclass ns:+ns-object))

;; lisp-app-doc-print-view
;; view that provides support for printing a document
(defclass lisp-app-doc-print-view (ns:ns-view)
  ((lisp-app-doc :accessor lisp-app-doc 
                 :initarg :doc)
   (lines :accessor lines
          :initform nil)
   (attributes :accessor attributes 
               :initform (make-instance ns:ns-mutable-dictionary))
   (page-line-count :accessor page-line-count
                    :initform 0)
   (line-height :accessor line-height)
   (num-pages :accessor num-pages
              :initform 0)
   (page-num :accessor page-num
             :initform 0)
   (print-op-initialized :accessor print-op-initialized
                         :initform nil)
   (page-rect :accessor page-rect))
  (:metaclass ns:+ns-object))

;; vscrolled-text-view
;; A text-view inside a vertiacall scroll-view which also has a stream acceptable to lisp format calls
(defclass vscrolled-text-view (ns:ns-scroll-view)
  ((view-stream :initform nil))
  (:metaclass ns:+ns-object))