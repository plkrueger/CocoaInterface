;; dev-tools-interface.lisp

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
  (require :objc-initialize)
  (require :dev-tools)
  (require :button)
  (require :table-utils)
  (require :menu-utils)
  (require :text-views)
  (require :window-controller)
  (require :lisp-controller)
  (require :constraint-layout))

(in-package :dev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window for displaying keywords and keyword values

(defmethod make-keyword-window ((wc lisp-window-controller))
  (let* ((search-text (#/autorelease (make-instance 'labeled-text-field
                                       :title "Search: "
                                       :resizable t
                                       :label-pos :left)))
         (lc1 (make-instance 'lisp-controller
                :search-key #'(lambda (cl)
                                (symbol-name (class-name cl)))
                :search-test #'(lambda (cl-name str)
                                 (search str cl-name :test #'string-equal))
                :sort-key #'class-name
                :sort-pred #'string<
                :child-key #'class-direct-subclasses
                :col-ids (list "ClassCol")
                :col-keys (list #'class-name)
                :root ns:ns-object))
         (next-button (#/autorelease (make-instance ns:ns-button
                                       :title "Next"
                                       :action "searchNext:"
                                       :target lc1
                                       :bezel-style :round-rect
                                       :button-type :momentary-light)))
         (prev-button (#/autorelease (make-instance ns:ns-button
                                       :title "Previous"
                                       :action "searchPrev:"
                                       :target lc1
                                       :bezel-style :round-rect
                                       :button-type :momentary-light)))
         (reload-button (#/autorelease (make-instance 'lisp-button
                                         :title "Reload"
                                         :action-func #'reload-doc-window
                                         :bezel-style :round-rect
                                         :button-type :momentary-light)))
         (lc2 (make-instance 'lisp-controller
                :col-ids (list "KeyCol")
                :col-keys-0 (list #'(lambda (class-key-list)
                                      (key-name (second class-key-list))))
                :child-key-0 #'(lambda (class-key-list)
                                 (list (apply #'key-doc-string class-key-list)))
                :row-height-1 35
                :col-keys-1 (list #'identity)))
         (lc3 (make-instance 'lisp-controller
                :col-ids (list "BindCol")
                :col-keys (list #'identity)))
         (class-col (#/autorelease (make-instance ns:ns-table-column
                                     :column-title "Class"
                                     :identifier "ClassCol"
                                     :min-width 60
                                     :editable nil
                                     :selectable t)))
         (tv1 (make-instance 'ns:ns-outline-view
                :columns (list class-col)
                :data-source lc1
                :delegate lc1
                :allows-column-resizing nil
                :column-autoresizing-style :uniform
                :outline-table-column class-col))
         (kw-col (#/autorelease (make-instance ns:ns-table-column
                                  :column-title "Keywords for initialize-instance"
                                  :identifier "KeyCol"
                                  :min-width 120
                                  :editable nil
                                  :wraps t
                                  :selectable t)))
         (tv2 (make-instance 'ns:ns-outline-view
                :columns (list kw-col)
                :data-source lc2
                :delegate lc2
                :allows-column-resizing nil
                :column-autoresizing-style :uniform
                :outline-table-column kw-col))
         (bind-col (#/autorelease (make-instance ns:ns-table-column
                                    :column-title "Binding targets for class instances"
                                    :identifier "BindCol"
                                    :min-width 120
                                    :editable nil
                                    :wraps t
                                    :selectable t)))
         (tv3 (make-instance 'ns:ns-table-view
                :columns (list bind-col)
                :data-source lc3
                :delegate lc3
                :allows-column-resizing nil
                :column-autoresizing-style :uniform))
         (scroll-view1 (make-instance ns:ns-scroll-view
                         :autohides-scrollers t
                         :has-horizontal-scroller t
                         :has-vertical-scroller t
                         :document-view tv1))
         (scroll-view2 (make-instance ns:ns-scroll-view
                         :autohides-scrollers t
                         :has-horizontal-scroller t
                         :has-vertical-scroller t
                         :document-view tv2))
         (scroll-view3 (make-instance ns:ns-scroll-view
                         :autohides-scrollers t
                         :has-horizontal-scroller t
                         :has-vertical-scroller t
                         :document-view tv3))
         (win (make-instance 'ns:ns-window
                :title "Class Keywords and Binding Targets"
                :resizable t
                :released-when-closed nil
                :content-subviews (list search-text prev-button next-button reload-button scroll-view1 scroll-view2 scroll-view3)))
         (cview (#/contentView win)))

    ;; connect the views to the controllers
    (setf (view lc1) tv1)
    (setf (view lc2) tv2)
    (setf (view lc3) tv3)

    ;; bind the value of the search-text view to the controller search-string slot
    (bind search-text "value" lc1 "searchString")

    ;; bind enabled for the search buttons
    (bind next-button "enabled" lc1 "canSearchNext")
    (bind prev-button "enabled" lc1 "canSearchPrev")

    ;; bind the root of the second table to the current selection of the first, so that when
    ;; a new class is selected, its keywords are immediately shown in the second table
    (bind lc2 "root" lc1 (list "selection" 
                               #'(lambda (class)
                                   (when (typep class 'objc:objc-class)
                                     (mapcar #'(lambda (keys)
                                                 (list class keys))
                                             (init-keys class :return-list t))))))

    ;; bind the root of the third table to the current selection of the first, so that when
    ;; a new class is selected, its binding targets are immediately shown in the third table
    (bind lc3 "root" lc1 (list "selection" 
                               #'(lambda (class)
                                   (when (typep class 'objc:objc-class)
                                     (valid-bindings class)))))

    (anchor cview search-text (list :leading :top))
    (anchor cview scroll-view1 (list :leading :bottom))
    (anchor cview scroll-view2 (list :trailing))
    (anchor cview scroll-view3 (list :trailing :bottom))
    (anchor cview reload-button (list :trailing))
    
    (constrain (>= (width search-text) 200))
    (constrain (>= (width scroll-view1) 200))
    (constrain (>= (height scroll-view1) 300))
    (constrain (>= (width search-text) (* 0.5 (width cview))))
    (constrain (= (width scroll-view1) (width scroll-view2)))
    (constrain (= (width scroll-view1) (width scroll-view3)))
    (constrain (= (height scroll-view2) (height scroll-view3)))
    (order-views :orientation :h
                 :views (list search-text prev-button next-button)
                 :align :center-y)
    (order-views :orientation :v
                 :views (list search-text scroll-view2 scroll-view3))
    (order-views :orientation :h
                 :views (list scroll-view1 10 scroll-view2)
                 :align :top)
    (order-views :orientation :h
                 :views (list scroll-view1 10 scroll-view3)
                 :align :bottom)
    (align-views :views (list next-button reload-button)
                 :align :center-y)

    (values win (list scroll-view1 scroll-view2 scroll-view3 lc1 lc2 lc3 tv1 tv2 tv3 win))))

(defvar *dev-tools-window-controller*
  (make-instance 'lisp-window-controller
    :build-method #'make-keyword-window))


;; define a function that can be used to reset the documentation window
(defun reload-doc-window ()
  (let ((lc (find-if #'(lambda (x)
                         (typep x 'lisp-controller))
                     (iu::instantiated-objects *dev-tools-window-controller*))))
    (when lc
      (reload-documentation)
      (setf (root lc) ns:ns-object)
      (show-window *dev-tools-window-controller*))))


(install-menuitems-after "Tools"
                         "Search Files..."
                         (make-instance ns:ns-menu-item
                           :title "Class Keywords and Binding Targets"
                           :action "showWindow:"
                           :target *dev-tools-window-controller*))

#| optionally make a separate menu if you want
(make-and-install-menu "Dev Tools"
                       (list "Class Keywords Documentation" "showWindow:" "" *dev-tools-window-controller*))
|#

(provide :dev-tools-interface)
