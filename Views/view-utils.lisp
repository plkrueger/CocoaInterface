;; view-utils.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :coerce-obj)
  (require :nslog-utils))

(in-package :lv)

(defmethod natural-size ((v ns:ns-view))
  ;; This is the default way if getting the right size for something.
  ;; First priority is to use the current frame. The assumption is that this was
  ;; set by some explicit user action and so should be used as the first choice.
  ;; After that we consider the intrinsic size which tells us about any natural size
  ;; that is determined by the view class itself. A width or height of -1.0 indicates 
  ;; there is no intrinsic size for that dimension.
  ;; The fittingSize computes a size based on the constraints of its subviews. All of
  ;; these may fail to provide a size and we'll just adopt a default and log that this
  ;; happened. If objects you for which you (constrain-to-natural-size ...) turn out
  ;; to have a funny size, check the console log for this message and add some other
  ;; constraint or explicitly set the view's frame to get the effect you want.
  (let* ((frame-rect (#/frame v))
         (frame-width (ns:ns-rect-width frame-rect))
         (frame-height (ns:ns-rect-height frame-rect))
         (fs (#/fittingSize v))
         (fs-width (ns:ns-size-width fs))
         (fs-height (ns:ns-size-height fs))
         (is (#/intrinsicContentSize v))
         (is-width (ns:ns-size-width is))
         (is-height (ns:ns-size-height is))
         (w (cond ((not (= frame-width 0))
                   frame-width)
                  ((and (not (= is-width -1)) (not (= is-width 0)))
                   is-width)
                  (t
                   fs-width)))
         (h (cond ((not (= frame-height 0))
                   frame-height)
                  ((and (not (= is-height -1)) (not (= is-height 0)))
                   is-height)
                  (t
                   fs-height))))
    (when (zerop w)
      ;; (ns-log (format nil "Cannot compute width for ~s, assuming 60" v))
      (setf w 60.0d0))
    (when (zerop h)
      ;; (ns-log (format nil "Cannot compute height for ~s, assuming 20" v))
      (setf h 20.0d0))
    (ns:make-ns-size w h)))

#| 
;; Since the #/intrinsicContentSize method was added to labels, is is no longer useful
;; But maybe someday the technique will come in handy for something ...
(defmethod natural-size ((v ns:ns-text-view))
  ;; if user set the frame size, use it. Otherwise make a reasonable size from
  ;; whatever string is currently the text of the view. This is only reasonable for
  ;; text-views like labels that have a fixed text and are not editable.
  (let* ((frame-rect (#/frame v))
         (frame-width (ns:ns-rect-width frame-rect))
         (frame-height (ns:ns-rect-height frame-rect))
         (base-size (#/size (#/attributedString v)))
         (base-width (ns:ns-size-width base-size))
         (base-height (ns:ns-size-height base-size))
         (w (cond ((not (zerop frame-width))
                   frame-width)
                  ((not (zerop base-width))
                   (+ 10.0d0 base-width))
                  (t
                   (ns-log (format nil "Frame and text width for ~s are both zero, assuming 60" v))
                   60.0d0)))
         (h (cond ((not (zerop frame-height))
                   frame-height)
                  ((not (zerop base-height))
                   base-height)
                  (t
                   (ns-log (format nil "Frame and text height for ~s are both zero, assuming 20" v))
                   20.0d0))))
    (ns:make-ns-size w h)))
|#

(defmethod add-subviews ((parent ns:ns-view) &rest views)
  ;; adds views to parent unless they are already part of its view hierarchy
  (dolist (v views)
    (when (and (view-p v) (not (#/isDescendantOf: v parent)))
      (#/addSubview: parent v))))

(defun view-p (thing)
  (typep thing 'ns:ns-view))

(defun zero-size-p (ns-size)
  (or (zerop (ns:ns-size-width ns-size))
      (zerop (ns:ns-size-height ns-size))))

(defmethod view-width ((self ns:ns-view))
  (ns:ns-rect-width (#/frame self)))

(defmethod natural-view-width ((self ns:ns-view))
  (ns:ns-size-width (natural-size self)))

(defmethod view-height ((self ns:ns-view))
  (ns:ns-rect-height (#/frame self)))

(defmethod natural-view-height ((self ns:ns-view))
  (ns:ns-size-height (natural-size self)))

(defmethod superview (thing)
  (declare (ignore thing))
  ;; not a view
  nil)

(defmethod superview ((v ns:ns-view))
  (obj-if-not-null (#/superview v)))

(defmethod common-superview (v1 v2)
  (cond ((and (view-p v1) (view-p v2))
         (if (eql v1 v2)
           (superview v1)
           (obj-if-not-null (#/ancestorSharedWithView: v1 v2))))
        ((view-p v1)
         (superview v1))
        ((view-p v2)
         (superview v2))
        (t
         nil)))

(defmethod joint-superview (v-list)
  ;; v-list may contain non-view objects
  (cond ((null v-list)
         nil)
        ((eql 1 (list-length v-list))
         (and (view-p (first v-list)) (superview (first v-list))))
        (t
         (reduce #'(lambda (v1 v2)
                     (#/ancestorSharedWithView: v1 v2))
                 (remove-if-not #'view-p v-list)))))

(defmethod view-tree ((v ns:ns-view))
  ;; return a list containing v as the first element and subview trees for
  ;; each subview. Mostly used for debugging.
  (list* v (mapcar #'view-tree (coerce-obj (#/subviews v) 'list))))

(defmethod print-view-tree ((v ns:ns-view) &optional (strm t))
  (format strm "~{~%~a~}" (view-tree-lines v)))

(defmethod view-tree-lines ((v ns:ns-view) &optional (indent 0))
  (list* (format nil
                 "~a~s"
                 (make-string indent :initial-element #\space)
                 v)
         (mapcan #'(lambda (sv)
                     (view-tree-lines sv (+ indent 2)))
                 (coerce-obj (#/subviews v) 'list))))
           

(provide :view-utils)