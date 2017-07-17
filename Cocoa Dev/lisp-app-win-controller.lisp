;; lisp-app-win-controller.lisp

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
  (require :objc-initialize)
  (require :window-controller)
  (require :lisp-controller)
  (require :window-utils)
  (require :text-views)
  (require :button)
  (require :constraint-layout)
  (require :lisp-app-doc))

(in-package :ad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Methods

(defun new-hemlock-view-with-text (new-text)
  ;; opens a new hemlock view and inserts the specified new-text into it
  (ccl::call-in-event-process #'(lambda ()
                                  (let* ((hv (gui::find-or-make-hemlock-view))
                                         (hb (hi::hemlock-view-buffer hv))
                                         (buffer-point (hi::buffer-point hb)))
                                    (let ((hi::*current-buffer* hb))
                                      (hi::insert-string buffer-point new-text))
                                    hv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-app-win-controller
;;
;; The class that controls the lisp-app-doc window

(defclass lisp-app-win-controller (lisp-window-controller)
  ((doc-type-controller :accessor doc-type-controller
                        :initform nil)
   (app-type-controller :accessor app-type-controller
                        :initform nil))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void)
                ((self lisp-app-win-controller))
  (when (doc-type-controller self)
    (#/release (doc-type-controller self))
    (setf (doc-type-controller self) nil))
  (when (app-type-controller self)
    ;; disconnect it from the document
    (unbind (app-type-controller self)))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc:defmethod (#/awakeFromNib :void) 
                ((self lisp-app-win-controller))
  (setf (win-controller (#/document self)) self))

(objc:defmethod (#/initBundle: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Initializes a bundle for the lisp-app-doc
  (init-bundle (#/document self))
  ;; can cause the doc-type table to be modified so make sure
  ;; controller is notified
  (update-window self))

(objc:defmethod (#/reinitBundle: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Initializes a bundle for the lisp-app-doc
  (reinit-bundle (#/document self))
  ;; can cause the doc-type table to be modified so make sure
  ;; controller is notified
  (update-window self))

(objc:defmethod (#/useBundle: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Initializes a bundle for the lisp-app-doc
  (use-bundle (#/document self))
  ;; can cause the doc-type type table to be modified so make sure
  ;; controller is notified
  (update-window self))

(objc:defmethod (#/loadLispApp: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Loads the bundle for the lisp-app-doc, which loads the source files
  ;; and effectively "executes" the app inside the IDE.
  (load-lisp-app (#/document self)))

(objc:defmethod (#/unloadLispApp: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; unloads by releasing the lisp-doc-controller for this app doc
  (unload-lisp-app (#/document self)))

(objc:defmethod (#/installExec: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Creates an executable for the lisp-app-doc and puts it into
  ;; the lisp-app-doc's bundle
  (install-executable (#/document self)))

(objc:defmethod (#/runStandAlone: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Runs the app from the bundle
  (run-standalone-app (#/document self)))

(objc:defmethod (#/selectIconFile: :void) 
                ((self lisp-app-win-controller) (sender :id))
  ;; Selects an icon file to reference in the info.plist
  (when (string= "" (coerce-obj (#/stringValue sender) 'string))
    (select-icon-file (#/document self) (coerce-obj (selection (doc-type-controller self)) t))))

(objc:defmethod (#/selectAppIconFile: :void) 
                ((self lisp-app-win-controller) (sender :id))
  ;; Selects an icon file to reference in the info.plist
  (when (string= "" (coerce-obj (#/stringValue sender) 'string))
    (select-app-icon-file (#/document self))))

(objc:defmethod (#/selectSrcFile: :void) 
                ((self lisp-app-win-controller) (sender :id))
  ;; Selects an icon file to reference in the info.plist
  (when (string= "" (coerce-obj (#/stringValue sender) 'string))
    (select-src-file (#/document self))))

(objc:defmethod (#/editPlist: :void) 
                ((self lisp-app-win-controller) (sender :id))
  (declare (ignore sender))
  ;; Edit the info.plist
  (edit-plist (#/document self)))

(objc:defmethod (#/comboBoxWillPopUp: :void)
                ((self lisp-app-win-controller) (notif :id))
  (declare (ignore notif))
  ;; update all the classes just before a pop-up menu on one of our interface combo
  ;; boxes is set to open. This makes sure that most current choices are always displayed.
  (update-available-classes (#/document self)))

;;; Handle notifications

(objc:defmethod (#/windowWillClose: :void) 
                ((self lisp-app-win-controller) (sender :id))
  ;; called because we made self the delegate for its window
  (declare (ignore sender))
  (when (doc-type-controller self)
    (#/release (doc-type-controller self))
    (setf (doc-type-controller self) nil))
  
  (window-will-close (#/document self)))

(defmethod make-app-doc-window ((wc lisp-app-win-controller))
  (let* ((app-doc (#/document wc))
         (frame1 (list 200 0))
         (app-name-txt (#/autorelease (make-instance ns:ns-text-field
                                        :frame-size frame1)))
         (app-class-txt (#/autorelease (make-instance ns:ns-combo-box
                                         :frame-size frame1
                                         :delegate wc)))
         (app-del-class-txt (#/autorelease (make-instance ns:ns-combo-box
                                             :frame-size frame1
                                             :delegate wc)))
         (bundle-id-txt (#/autorelease (make-instance ns:ns-text-field
                                         :frame-size frame1)))
         (app-init-func-txt (#/autorelease (make-instance ns:ns-text-field
                                             :frame-size frame1)))
         (executable-txt (#/autorelease (make-instance ns:ns-text-field
                                          :frame-size frame1)))
         (version-txt (#/autorelease (make-instance ns:ns-text-field
                                       :frame-size frame1)))
         (min-os-txt (#/autorelease (make-instance ns:ns-text-field
                                      :frame-size frame1)))
         (bundle-sig-txt (#/autorelease (make-instance ns:ns-text-field
                                          :frame-size frame1)))
         (req-src-txt (#/autorelease (make-instance ns:ns-text-field
                                       :frame-size frame1
                                       :action "selectSrcFile:"
                                       :target wc)))
         (app-icon-txt (#/autorelease (make-instance ns:ns-text-field
                                        :frame-size frame1
                                        :action "selectAppIconFile:"
                                        :target wc)))
         (app-form1 (#/autorelease (make-instance 'form-view
                                     :labels (list "Application Name:" "Application Class:" "App Delegate Class:"
                                                   "Bundle Identifier:" "App Init Function:" "Executable:" "Version:"
                                                   "Minimum OS Level:" "Bundle Signature:" "Required Source File:" "App Icon File:")
                                     :text-fields (list app-name-txt app-class-txt app-del-class-txt
                                                        bundle-id-txt app-init-func-txt
                                                        executable-txt version-txt min-os-txt bundle-sig-txt req-src-txt
                                                        app-icon-txt))))
         ;; Note: no autorelease on the lisp-controller because we'll save it in the app-win-controller
         ;; and release it upon dealloc
         (lc (make-instance 'lisp-controller
               :root (app-doc-types (#/document wc))
               :col-ids (list "DTCol")
               :col-keys (list #'dt-doc-type)
               :initform '(make-instance 'ad::lisp-doc-type)
               :func-owner app-doc
               :added-func #'ad::doc-type-added
               :undo-doc app-doc
               :undo-name "document type table"))
         (dt-col (#/autorelease (make-instance ns:ns-table-column
                                  :column-title "Supported Document Types"
                                  :identifier "DTCol"
                                  :min-width 120
                                  :editable t
                                  :wraps t
                                  :selectable t)))
         (tv (#/autorelease (make-instance 'ns:ns-table-view
                              :columns (list dt-col)
                              :data-source lc
                              :delegate lc
                              :allows-column-resizing nil
                              :column-autoresizing-style :uniform)))
         (scroll-view (#/autorelease (make-instance ns:ns-scroll-view
                                       :autohides-scrollers t
                                       :has-horizontal-scroller t
                                       :has-vertical-scroller t
                                       :document-view tv)))
         (insert-dt-button (#/autorelease (make-instance ns:ns-button
                                            :title "+"
                                            :frame-size '(20 0)
                                            :bezel-style :round-rect
                                            :target lc
                                            :action "insert:")))
         (remove-dt-button (#/autorelease (make-instance ns:ns-button
                                            :title "-"
                                            :frame-size '(20 0)
                                            :bezel-style :round-rect
                                            :target lc
                                            :action "remove:")))
         (type-name-txt (#/autorelease (make-instance ns:ns-text-field
                                         :frame-size frame1)))
         (uti-txt (#/autorelease (make-instance ns:ns-text-field
                                   :frame-size frame1)))
         (file-ext-txt (#/autorelease (make-instance ns:ns-text-field
                                        :frame-size frame1)))
         (doc-class-txt (#/autorelease (make-instance ns:ns-combo-box
                                         :frame-size frame1
                                         :delegate wc)))
         (role-txt (#/autorelease (make-instance ns:ns-combo-box
                                    :frame-size frame1
                                    :delegate wc)))
         (icon-txt (#/autorelease (make-instance ns:ns-text-field
                                    :frame-size frame1
                                    :action "selectIconFile:"
                                    :target wc)))
         (app-form2 (#/autorelease (make-instance 'form-view
                                     :labels (list "Type Name:" "UTI:" "File Extension:"
                                                   "Document Class:" "Role:" "Icon File:")
                                     :text-fields (list type-name-txt uti-txt file-ext-txt
                                                        doc-class-txt role-txt icon-txt)
                                     :enabled nil)))
         (export-uti-cb (#/autorelease (make-instance ns:ns-button
                                         :title "Export UTI"
                                         :button-type :switch)))
         (app-owns-cb (#/autorelease (make-instance ns:ns-button
                                       :title "App owns Doc Type"
                                       :button-type :switch)))
         (doc-type-label (#/autorelease (make-instance 'label-view
                                          :title "Detail for Selected Document Type")))
         (win (#/autorelease (make-instance 'ns:ns-window
                               :delegate wc
                               :resizable t
                               :content-subviews (list app-form1 #| select-src-button
                                                       select-app-icon-button  |#
                                                       doc-type-label scroll-view insert-dt-button remove-dt-button 
                                                       app-form2 export-uti-cb app-owns-cb #| select-icon-button |#))))
         (cv (#/contentView win)))

    ;; save off lc so that we can unbind it from the doc when the window is about to close
    (setf (app-type-controller wc) lc)

    ;; connect the doc type table view to its controller and the table controller to the window controller
    (setf (view lc) tv)
    (setf (doc-type-controller wc) lc)

    ;; Bind values of all the text display fields which contain document information
    ;; Go through document slot value of the window controller
    (bind app-name-txt "value" wc (list "document" "appName"))
    (bind executable-txt "value" wc (list "document" "appExec"))
    (bind bundle-id-txt "value" wc (list "document" "appBundleID"))
    (bind app-init-func-txt "value" wc (list "document" "appInitFunc"))
    (bind bundle-sig-txt "value" wc (list "document" "appBundleSig"))
    (bind version-txt "value" wc (list "document" "appVersion"))
    (bind min-os-txt "value" wc (list "document" "appMinOS"))
    (bind app-class-txt "value" wc (list "document" "appClass"))
    (bind app-del-class-txt "value" wc (list "document" "appDelegateClass"))
    (bind req-src-txt "value" wc (list "document" "appAbbrevSrc"))
    (bind app-icon-txt "value" wc (list "document" "appIconFile"))

    ;; Bind the menus of combo-boxes
    (bind app-class-txt "contentValues" wc (list "document" "appClasses"))
    (bind app-del-class-txt "contentValues" wc (list "document" "appDelegateClasses"))
    (bind doc-class-txt "contentValues" wc (list "document" "appDocClasses"))
    (bind role-txt "contentValues" wc (list "document" "appDocRoles"))

    ;; Bind root value for the lisp controller for the doc type table 
    (bind lc "root" (#/document wc) (list "docTypes"))
    
    ;; bind enabled for the insert and remove buttons and doc type detail fields
    (bind insert-dt-button "enabled" lc "canInsert")
    (bind remove-dt-button "enabled" lc "canRemove")
    (bind app-form2 "enabled" lc "hasSelection")
    (bind export-uti-cb "enabled" lc "hasSelection")
    (bind app-owns-cb "enabled" lc "hasSelection")
    ;; (bind select-icon-button "enabled" lc "hasSelection")

    ;; bind the values of the text display fields which contain doc-type information
    ;; relative to the doc type currently displayed in the table
    (bind type-name-txt "value" lc (list "selection" "dtDocType"))
    (bind uti-txt "value" lc (list "selection" "dtUTI"))
    (bind file-ext-txt "value" lc (list "selection" "dtFileExt"))
    (bind doc-class-txt "value" lc (list "selection" "dtDocClass"))
    (bind role-txt "value" lc (list "selection" "dtDocRole"))
    (bind icon-txt "value" lc (list "selection" "dtIconFile"))

    ;; Bind the values of the check boxes for doc type information
    (bind export-uti-cb "value" lc (list "selection" "dtExportUTI"))
    (bind app-owns-cb "value" lc (list "selection" "dtOwnerForDoc"))

    ;;;;;;;;;;;;; Constraints

    ;; First constrain some sizes
    (constrain (>= (height scroll-view) 60))

    ;; We'll start at the top and work our way down the window

    (anchor cv app-form1 '(:top :left :right))

    ;; constrain objects going down the left side of the window
    (order-views :views (list app-form1 scroll-view insert-dt-button doc-type-label export-uti-cb app-form2 :border)
                 :orientation :v
                 :align :leading)
    ;; line up buttons below the doc-type table
    (order-views :views (list insert-dt-button 2 remove-dt-button)
                 :orientation :h
                 :align :center-y)
    ;; line up check boxes at top of details section
    (order-views :views (list export-uti-cb 10 app-owns-cb)
                 :orientation :h
                 :align :center-y)
    ;; constrain the width of the scrolling type table and the second form table)
    (constrain (= (width scroll-view) (width app-form1)))
    (constrain (= (width app-form2) (width app-form1)))
    
    ;; return window and objects to manage
    ;; Since window will own all of the various sub-views created and the window itself will be 
    ;; released when dealloc is called for this controller, we don't have to manage those subviews

    (values win (list lc))))

(provide :lisp-app-win-controller)
