;; lisp-document.lisp

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

;; Defines a lisp-document base class
;; Documents in lisp may inherit from this class rather than from ns:ns-document
;; to get some additional functionality.
;; Specifically you get:

;; The ability to specify a window-controller class or use a default window-controller
;; Implement a document-window-controller-class method for your lisp-document subclass
;; which returns a class object that should be some subclass of NSWindowController.
;;
;; The ability to specify the name of the nib for the main document window
;; If your document uses a single window then implement the method window-nib-name
;; for your lisp-document subclass and return a string with the name of the nib. This
;; string can either be a complete path to some nib or just the nib name, in which case
;; the nib is presumed to be resident with the bundle from which the document was loaded.
;; If your document uses multiple windows, then you will need to define the Objective-C
;; method #/makeWindowControllers for your document class and take advantage of the method
;; (bundle-for-class <objc-class>) to help you find an appropriate bundle from which
;; the nib file can be loaded.
;;
;; Provides a lisp-friendly interface to the undo manager
;; To specify how to undo some action call 
;;   (set-undo <target> <undo-closure> <undo-string>)
;; where <target> is the instance being modified
;; <undo-closure is a function of 0 arguments that when funcalled will undo the change
;; being made, and
;; <undo-string> is a string that names the action that will be undone or redone
;; Typically the undo-string should name the action. So in a method that is adding 
;; something to a view, the undo-string should be something like "add". When in the process
;; of "undoing" the set-undo method will ignore this sting and the underlying Objective-C
;; method will take the action name for the redo menu item from the undo menu item. 
;;
;; Automatic support for saving the document to disk and reloading it
;; Default methods are provided for #/readFromData:ofType:error: and #/dataOfType:error:
;; that will simply save all non :foreign-type slots of the document instance and restore 
;; them (in response to standard "save" and "open" menu functionality). It's possible to
;; easily specify an alternate set of slots to be saved and restored by defining the method
;; archive-slots; for examaple:
;; (defmthod archive-slots ((obj your-new-doc-class))
;;   (list 'slot-a 'slot-c 'slot-d))
;; Slots may contain pretty much any sort of Lisp or Objective-C object.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize)
  (require :menu-utils)
  (require :lisp-controller)
  (require :lisp-app-pr-view)
  (require :lisp-bundle)
  (require :window-controller)
  (require :quick-window))

(in-package :iu)

;;; The lisp-document class

#|
(defclass lisp-document (ns:ns-document)
  ()
  (:metaclass ns:+ns-object))
|#

;; This method mostly exists so that when running an app under the CCL IDE
;; the lisp-doc-controller can find out when a document is closed
(objc:defmethod (#/close :void)
                ((self lisp-document))
  (call-next-method)
  (#/postNotificationName:object: (#/defaultCenter ns:ns-notification-center)
                                  (coerce-obj "LispDocumentDidClose" 'ns:ns-string)
                                  self))

(objc:defmethod (#/makeWindowControllers :void) 
                ((self lisp-document))
  (let* ((um (#/undoManager self))
         (build-funcs (window-build-funcs self))
         (wc-classes (document-window-controller-classes self)))
    (#/disableUndoRegistration um)
    (flet ((make-a-window (build-func wc-class)
             (let ((wc (#/autorelease (make-instance wc-class
                                        :build-method build-func))))
               (#/addWindowController: self wc))))
      (mapcar #'make-a-window build-funcs wc-classes))
    (#/enableUndoRegistration (#/undoManager self))
    (#/removeAllActionsWithTarget: um self)))

#|
(objc:defmethod (#/showWindows :void)
                ((self lisp-document))
  (ns-log-format "showWindows called for ~s" self)
  ;;(ns-log-format "wc's = ~s" (coerce-obj (#/windowControllers self) 'list))
  ;;(call-next-method)
  (dolist (wc (coerce-obj (#/windowControllers self) 'list))
    (#/loadWindow wc)))
|#

#| old method that uses nibs
(objc:defmethod (#/makeWindowControllers :void) 
                ((self lisp-document))
  (let* ((nib-names (or (mapcar #'lisp-to-temp-nsstring (window-nib-names self))
                        (list (#/windowNibName self))))
         (wc-classes (document-window-controller-classes self))
         (bundle (bundle-for-class (class-of self))))
    (flet ((make-a-window (nib-name wc-class)
             (let* ((nib-path (and (not (eql nib-name (%null-ptr)))
                                  (or (name-is-full-path nib-name)
                                      (#/pathForResource:ofType: bundle nib-name #@"nib"))))
                    (wc (#/alloc wc-class))
                    (owner (if (document-is-nib-file-owner self nib-name) self wc)))
               (setf wc (if (or (null nib-path) (eql nib-path (%null-ptr)))
                          (#/initWithWindow: wc (make-quick-window :text "Proxy window for your document"))
                          (#/initWithWindowNibPath:owner: wc
                                                          nib-path
                                                          owner)))
               (#/setShouldCloseDocument: wc #$YES)
               (#/addWindowController: self wc))))
      (mapcar #'make-a-window nib-names wc-classes))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for common document functions

(defmethod document-did-open ((self lisp-document))
  ;; do nothing, can be overridden in sub-classes to do whatever ...
  )

(defun name-is-full-path (ns-pathname)
  ;; if ns-pathname identifies a complete path to a file, return it
  ;; If is is only a relative pathname return nil
  (if (and (not (eql ns-pathname (%null-ptr))) (probe-file (ns-to-lisp-string ns-pathname)))
    ns-pathname))

(defmethod document-window-controller-classes ((self lisp-document))
  (list lisp-window-controller))

(defmethod window-build-funcs ((self lisp-document))
  ;; default method returns nil
  (list #'make-default-lisp-window))

(defmethod make-default-lisp-window ((wc lisp-window-controller))
  (values (make-quick-window 
           :text (format nil "Default Test Window for ~s" (#/document wc)))
          nil))

;; Necessary overrides of NSDocument methods
;; Methods called to read data from and write data to external files

(objc:defmethod (#/readFromData:ofType:error: #>BOOL) 
                ((self lisp-document) (data :id) (dtype :id) (err (:* :id)))
  (declare (ignore err dtype))
  (let ((um (#/undoManager self)))
    (#/disableUndoRegistration um)
    (while-unconverting
     (objc-to-std-instance (#/unarchiveObjectWithData: ns:ns-keyed-unarchiver data) self))
    (document-did-open self)
    (#/enableUndoRegistration (#/undoManager self))
    #$YES))

(objc:defmethod (#/dataOfType:error: :id) 
                ((self lisp-document) (dtype :id) (err (:* :id)))
  (declare (ignore dtype err))
  (while-converting
   (#/archivedDataWithRootObject: ns:ns-keyed-archiver (std-instance-to-objc self))))

;; Method to validate save menu item

(objc:defmethod (#/validateMenuItem: #>BOOL) 
                ((self lisp-document) (item :id))
  (let* ((action (#/action item)))
    (cond ((eql action (ccl::@selector #/saveDocument:))
           (#/isDocumentEdited self))
          (t (call-next-method item)))))

;; Methods to support printing

(defmethod print-view-class ((self lisp-document))
  ;; Return the class of the print-view to use.
  ;; This default method returns the lisp-doc-print-view class.
  ;; For a custom print view, override this method for the lisp-document subclass
  (find-class 'lv::lisp-app-doc-print-view))

(defun break-lines (line &optional (max-length 110))
   ;; break long lines into a list of smaller lines
  (let ((lin-length (length line)))
    (if (<= lin-length max-length)
      (list line)
      (do* ((lines nil)
            (rest-of-line line
                          (if space-pos
                            (subseq rest-of-line (1+ (length next-line)))
                            (subseq rest-of-line (length next-line))))
            (rest-length (length rest-of-line)
                         (length rest-of-line))
            (next-chunk (subseq rest-of-line 0 (min rest-length (1+ max-length)))
                        (subseq rest-of-line 0 (min rest-length (1+ max-length))))
            (space-pos (position #\space next-chunk :test #'char= :from-end t)
                       (position #\space next-chunk :test #'char= :from-end t))
            (next-line (if space-pos
                         (subseq next-chunk 0 space-pos)
                         (subseq next-chunk 0 (min max-length (length next-chunk))))
                       (if space-pos
                         (subseq next-chunk 0 space-pos)
                         (subseq next-chunk 0 (min max-length (length next-chunk))))))
           ((zerop rest-length) (nreverse lines))
        (push next-line lines)))))

(defmethod font-name ((self lisp-document))
  "Courier")

(defmethod font-size ((self lisp-document))
  8.0)

(defmethod print-lines ((self lisp-document) lines-per-page)
  (declare (ignore lines-per-page))
  ;; Default method to support printing a document with text.
  ;; This just prints the values of all the document slots.
  ;; If you are printing graphically, this should be overriden
  ;; to return nil
  (let ((lines nil)
        (blank-line ""))
    (push (format nil "Slot Values for ~s" self) lines)
    (push blank-line lines)
    (dolist (slot (archive-slots self) (nreverse lines))
      (dolist (l (break-lines (format nil 
                                      "~a => ~s" 
                                      (string slot)
                                      (if (slot-boundp self slot)
                                        (slot-value self slot) 
                                        :unbound))
                              (floor 880 (font-size self))))
        (push l lines)))))

(defmethod print-graphic ((self lisp-document) pr-view rect)
  (declare (ignore pr-view rect)) ;; just to suppress warnings here
  ;; By default do nothing. Override for your document to print
  ;; a single graphic page. Print-lines must return nil or this
  ;; method will not be called.
  ;; If your document will print more than one page of graphics you
  ;; can retrieve the current page number being printed by 
  ;;        (page-num pr-view)
  ;; "page-num" is exported from the :lv package which you should use
  )

(defmethod num-graphic-pages ((self lisp-document))
  ;; By default return 1 page
  ;; lisp-documents that wish to print more than a single graphics page should
  ;; override this method to return a different number
  1)

(objc:defmethod (#/printOperationWithSettings:error: :id)
                ((self lisp-document) (settings :id) (err (:* :id)))
  (declare (ignore err settings))
  (let ((pr-view (make-instance (print-view-class self)
                   :doc self)))
    (#/printOperationWithView:printInfo: ns:ns-print-operation 
                                         pr-view 
                                         (#/printInfo self))))

(provide :lisp-document)
  