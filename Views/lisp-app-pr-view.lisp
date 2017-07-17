;; lisp-app-pr-view.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lv-classes)
  (require :date)
  (require :nslog-utils))

(in-package :lv)

;; provides a default view class used to print lisp-app-doc information

#|
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
|#

(defmethod initialize-instance :after ((self lisp-app-doc-print-view)
                                       &key &allow-other-keys)
  ;; assure lisp-app-doc still exists if user closes the window while we are printing
  (#/retain (lisp-app-doc self)))

(objc:defmethod (#/dealloc :void)
                ((self lisp-app-doc-print-view))
  (#/release (lisp-app-doc self))
  (#/release (attributes self))
  (call-next-method)
  (objc:remove-lisp-slots self))

(defmethod init-print-operation ((self lisp-app-doc-print-view))
  (let* ((font-name (lisp-to-temp-nsstring (font-name (lisp-app-doc self))))
         (font-size (float (font-size (lisp-app-doc self))))
         (font (#/fontWithName:size: ns:ns-font font-name font-size))
         (pr-op (#/currentOperation ns:ns-print-operation))
         (pr-info (#/printInfo pr-op))
         ;; (pg-size (#/paperSize pr-info))
         ;; (left-margin (#/leftMargin pr-info))
         ;; (right-margin (#/rightMargin pr-info))
         ;; (top-margin (#/topMargin pr-info))
         ;; (bottom-margin (#/bottomMargin pr-info))
         (image-rect (#/imageablePageBounds pr-info))
         (pg-rect (ns:make-ns-rect 0 
                                   0 
                                   (ns:ns-rect-width image-rect)
                                   (ns:ns-rect-height image-rect))))
    (setf (line-height self) (* (+ (#/ascender font) (abs (#/descender font))) 1.5))
    (#/setObject:forKey: (attributes self) font #$NSFontAttributeName)
    ;; compute printing parameters and set the range
    ;; (log-size pg-size "pg-size: ")
    ;; (log-4floats left-margin right-margin top-margin bottom-margin 
    ;;              (list "Margins: left = " " right = " " top = " " bottom = "))
    ;; (log-rect image-rect "imageable rect: ")
    (setf (page-rect self) pg-rect)
    ;; (log-rect pg-rect "my page rect: ")
    (#/setFrame: self pg-rect)
    ;; (log-float (line-height self) "Line Height: ")
    (setf (page-line-count self) (floor (ns:ns-rect-height pg-rect) 
                                        (line-height self)))
    (setf (lines self) (print-lines (lisp-app-doc self) (page-line-count self)))
    (setf (num-pages self)
          (if (lines self)
              ;; compute the number of pages for the number of lines to be printed
              (ceiling (list-length (lines self))
                       (page-line-count self))
              ;; or ask the document to specify a number of graphics pages
              (num-graphic-pages (lisp-app-doc self))))
    (setf (print-op-initialized self) t)))

(objc:defmethod (#/knowsPageRange: :<BOOL>) 
                ((self lisp-app-doc-print-view) (range (:* #>NSRange)))
  (unless (print-op-initialized self)
    (init-print-operation self))
  ;; start on page 1
  (setf (ns:ns-range-location range) 1)
  (setf (ns:ns-range-length range) (num-pages self))
  #$YES)

(objc:defmethod (#/rectForPage: #>NSRect)
                ((self lisp-app-doc-print-view) (pg #>NSInteger))
  (unless (print-op-initialized self)
    (init-print-operation self))
  (setf (page-num self) (1- pg))
  (page-rect self))

(objc:defmethod (#/isFlipped #>BOOL)
                ((self lisp-app-doc-print-view))
  (unless (print-op-initialized self)
    (init-print-operation self))
  (if (lines self)
    #$YES
    #$NO))

(objc:defmethod (#/drawRect: :void)
                ((self lisp-app-doc-print-view) (r #>NSRect))
  (with-slots (lisp-app-doc attributes page-line-count line-height page-num page-rect
                            lines) self
    (ns:with-ns-rect (line-rect (ns:ns-rect-x r) 
                                (- (ns:ns-rect-y r) line-height)
                                (ns:ns-rect-width r) 
                                line-height)
      (labels ((draw-next-line (str)
                 (incf (ns:ns-rect-y line-rect) line-height)
                 (#/drawInRect:withAttributes: 
                  (lisp-to-temp-nsstring str)
                  line-rect
                  attributes)))
        ;; print the appropriate lines for this page
        (if lines
          (let* ((strt (* page-num page-line-count))
                 (end (min (list-length lines) (+ strt page-line-count))))
            (dolist (pline (subseq lines strt end))
              (draw-next-line pline)))
          (print-graphic lisp-app-doc self r))))))

(provide :lisp-app-doc-pr-view)