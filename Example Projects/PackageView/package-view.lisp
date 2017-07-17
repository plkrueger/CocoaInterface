;;; package-view.lisp

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
  (require :demo-packages)
  (require :coerce-obj)
  (require :lisp-controller)
  (require :window-utils)
  (require :menu-utils)
  (require :constraint-layout)
  (require :window-controller)
  (require :text-views))

(in-package :pv)

(defclass package-data ()
  ((package-ctrl :accessor package-ctrl)
   (use-ctrl :accessor use-ctrl)
   (used-by-ctrl :accessor used-by-ctrl)
   (current-package :accessor current-package :initform nil)))

;; Adjust the contents of the use and used-by tables when something new is
;; selected in the primary package table.
(defmethod pkg-selected ((self package-data)
                         (lc lisp-controller)
                         pkg-list
                         selected-indx
                         col-indx
                         selected-pkg)
  (declare (ignore pkg-list selected-indx col-indx))
  (with-slots (current-package use-ctrl used-by-ctrl) self
    (unless (eq current-package selected-pkg)
      (setf current-package selected-pkg)
      (setf (root use-ctrl) (package-use-list selected-pkg))
      (setf (root used-by-ctrl) (package-used-by-list selected-pkg)))))

(defmethod nickname-string ((self package))
  (format nil "~{~a~^,~}" (package-nicknames self)))

(defmethod make-package-window ((lc lisp-window-controller))
  (let* ((pkg-data (make-instance 'package-data))
         (pkg-lc (make-instance 'lisp-controller
                   :func-owner pkg-data
                   :select-func #'pkg-selected
                   :sort-key #'package-name
                   :sort-pred #'string<
                   :col-ids (list "PkgCol" "NickCol")
                   :col-keys (list #'package-name #'nickname-string)
                   :root (list-all-packages)))
         (pkg-col (#/autorelease (make-instance ns:ns-table-column
                                   :column-title "Package"
                                   :identifier "PkgCol"
                                   :min-width 80
                                   :editable nil
                                   :selectable t)))
         (nick-col (#/autorelease (make-instance ns:ns-table-column
                                    :column-title "Nicknames"
                                    :identifier "NickCol"
                                    :min-width 80
                                    :editable nil
                                    :selectable nil)))
         (pkg-tab (make-instance ns:ns-table-view
                    :columns (list pkg-col nick-col)
                    :data-source pkg-lc
                    :delegate pkg-lc
                    :allows-column-resizing nil
                    :column-autoresizing-style :uniform))
         (pkg-scroll-view (make-instance ns:ns-scroll-view
                            :autohides-scrollers t
                            :has-horizontal-scroller nil
                            :has-vertical-scroller t
                            :document-view pkg-tab))
         (use-lc (make-instance 'lisp-controller
                   :sort-key #'package-name
                   :sort-pred #'string<
                   :col-ids (list "UseCol")
                   :col-keys (list #'package-name)))
         (use-col (#/autorelease (make-instance ns:ns-table-column
                                   :column-title "Package Uses"
                                   :identifier "UseCol"
                                   :min-width 80
                                   :editable nil)))
         (use-tab (make-instance ns:ns-table-view
                    :columns (list use-col)
                    :data-source use-lc
                    :delegate use-lc
                    :allows-column-resizing nil
                    :column-autoresizing-style :uniform))
         (use-scroll-view (make-instance ns:ns-scroll-view
                            :autohides-scrollers t
                            :has-horizontal-scroller nil
                            :has-vertical-scroller t
                            :document-view use-tab))
         (used-by-lc (make-instance 'lisp-controller
                       :sort-key #'package-name
                       :sort-pred #'string<
                       :col-ids (list "UsedByCol")
                       :col-keys (list #'package-name)))
         (used-by-col (#/autorelease (make-instance ns:ns-table-column
                                       :column-title "Package Used By"
                                       :identifier "UsedByCol"
                                       :min-width 80
                                       :editable nil)))
         (used-by-tab (make-instance ns:ns-table-view
                        :columns (list used-by-col)
                        :data-source used-by-lc
                        :delegate used-by-lc
                        :allows-column-resizing nil
                        :column-autoresizing-style :uniform))
         (used-by-scroll-view (make-instance ns:ns-scroll-view
                                :autohides-scrollers t
                                :has-horizontal-scroller nil
                                :has-vertical-scroller t
                                :document-view used-by-tab))
         (win (make-instance 'ns:ns-window
                :title "Package Browser"
                :resizable t
                :released-when-closed nil
                :content-subviews (list pkg-scroll-view
                                        use-scroll-view
                                        used-by-scroll-view)))
         (cview (#/contentView win)))
    
    ;; Set the links to the table controllers in the pkg-data object
    (setf (package-ctrl pkg-data) pkg-lc)
    (setf (use-ctrl pkg-data) use-lc)
    (setf (used-by-ctrl pkg-data) used-by-lc)

    ;; connect the views to the controllers
    (setf (view pkg-lc) pkg-tab)
    (setf (view use-lc) use-tab)
    (setf (view used-by-lc) used-by-tab)

    ;; add constraints
    ;; Fix the position of all the views relative to the content view
    (anchor cview pkg-scroll-view (list :leading :top :trailing))
    (anchor cview use-scroll-view (list :leading :bottom))
    (anchor cview used-by-scroll-view (list :bottom :trailing))

    ;; Make all tables he same height
    (constrain (= (height pkg-scroll-view) (height use-scroll-view)))
    (constrain (= (height used-by-scroll-view) (height use-scroll-view)))

    ;; order the views vertically
    (order-views :orientation :v :views (list pkg-scroll-view 15 use-scroll-view))
    (order-views :orientation :v :views (list pkg-scroll-view 15 used-by-scroll-view))

    ;; order the views horizontally
    (order-views :orientation :h :views (list use-scroll-view used-by-scroll-view))

    ;; Make the two tables at the bottom the same width
    (constrain (= (width used-by-scroll-view) (width use-scroll-view)))

    ;; Make sure the window is never too narrow so that something disappears
    (constrain (>= (width pkg-scroll-view) 300))

    ;; return the window and all instantiated objects
    (values win (list pkg-scroll-view use-scroll-view used-by-scroll-view
                      pkg-lc use-lc used-by-lc
                      pkg-tab use-tab used-by-tab 
                      pkg-data win))))

(defvar *package-window-controller*
  (make-instance 'lisp-window-controller
    :build-method #'make-package-window))

(install-menuitems-after "Tools"
                         "Search Files..."
                         (make-instance ns:ns-menu-item
                           :title "Package Browser"
                           :action "showWindow:"
                           :target *package-window-controller*))

;; or test by

(defun test-package ()
  (on-main-thread 
   (let ((wc (make-instance 'lisp-window-controller
               :build-method #'make-package-window)))
     (show-window wc)
     wc)))

(provide :package-view)