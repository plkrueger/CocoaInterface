;; lisp-doc-controller.lisp

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
  (require :iu-classes)
  (require :menu-utils)
  (require :selector-utils)
  (require :nslog-utils)
  (require :lisp-bundle)
  (require :lisp-document)
  (require :lisp-app-delegate)
  (require :doc-controller-hash)
  (require :nib)
  (require :open-panel))

(in-package :iu)

;; lisp-doc-controller class
;; This class does some of the same things that the shared NSDocumentController instance
;; does for stand-alone application programs. We use it so that we don't have to mess
;; with CCL's existing interfaces to or its NSApplication delegate objects. We will
;; create specific menu-items that target an instance of this class to create new documents
;; of a specified type that the CCL IDE knows nothing about. We will tell the shared
;; NSDocumentController about our documents so that it can manage things like saving and
;; closing them. This class will also handle opening files of a specified type.
;; The creator of one of these objects can also specify a name to use for inserting
;; "New <doc-type>" and "Open <doc-type>" menuitems which will target this instance.

;; The class also has functionality that permits a developer to create a main-menu
;; creation function and execute it within a CCL IDE environment. This would be done by 
;; invoking the "Load App Under IDE" command from the "Dev" menu.
;; If the user also specifies a delegate class that is connected to the File Owner's delegate
;; outlet, then any menu commands that are targeted to the File Owner will be passed
;; to the delegate object, just as they would be passed by an NSApplication instance to its delegate
;; in a stand-alone application. If there is no delegate specified or if the delegate cannot handle
;; a message, then it is redirected to the IDE's application instance (i.e. the value of $&NSApp).

#|
(defclass lisp-doc-controller (ns:ns-object)
  ((document-class :accessor document-class
                   :initform nil)
   (doc-type-name :accessor doc-type-name
                  :initform nil)
   (saved-menu-key :accessor saved-menu-key
                   :initform nil)
   (file-ext :accessor file-ext
             :initform nil)
   (doc-ctrlr :accessor doc-ctrlr
              :initform nil)
   (ldc-open-pnl :accessor ldc-open-pnl
                 :initform nil)
   (type-ns-str :accessor type-ns-str
                :initform nil)
   (ext-ns-str :accessor ext-ns-str
               :initform nil)
   (documents :accessor documents
              :initform nil)
   (installed-menuitems :accessor installed-menuitems
                        :initform nil)
   (bundle-objects :accessor bundle-objects
                   :initform nil)
   (delegate-class :accessor delegate-class
                   :initform nil)
   (delegate :accessor delegate
             :foreign-type :id))
  (:metaclass ns:+ns-object))
|#

(let ((dc-hash (make-hash-table :test #'eql))
      (doc-type-dc-hash (make-hash-table :test #'equal)))

  (defmethod initialize-doc-controller ((self lisp-doc-controller) 
                                        &key 
                                        (doc-class nil)
                                        (delegate-class nil)
                                        (doc-type nil)
                                        (file-ext nil)
                                        (app-bundle nil))
    (let ((starting-main-menu (#/mainMenu #&NSApp)))
      (save-main-menu) ;; save the current main menu for future menu swapping
      (setf (document-class self) doc-class)
      (setf (delegate-class self) delegate-class)
      (setf (doc-type-name self) doc-type)
      (setf (gethash doc-type doc-type-dc-hash) self)
      (setf (file-ext self) file-ext)
      (setf (doc-ctrlr self) (#/sharedDocumentController ns:ns-document-controller))
      ;;(setf (ldc-open-pnl self)
      ;;      (#/retain (make-open-panel 
      ;;                 :allowed-file-types (and file-ext (list file-ext)))))
      (when app-bundle
        ;; load the main nib from the bundle with this object as the "owner" of the nib OR
        ;; run the app-init function specified in the info.plist
        (let* ((dict (#/infoDictionary app-bundle))
               (nib-name (if (not (eql (%null-ptr) dict))
                           (#/objectForKey: dict  #@"NSMainNibFile")))
               (main-func-name (if (not (eql (%null-ptr) dict))
                                 (#/objectForKey: dict  #@"CLMainFunc")))
               (lisp-func-name (unless (eql main-func-name (%null-ptr))
                                 (coerce-obj main-func-name 'string))))
          (cond ((non-empty-string lisp-func-name)
                 (funcall (read-from-string lisp-func-name) self)
                 (#/awakeFromNib self))
                ((and nib-name (not (eql nib-name (%null-ptr))))
                 ;; load the nib
                 (setf (bundle-objects self)
                       (load-nibfile nib-name 
                                     :nib-owner self
                                     :retain-top-objs t
                                     :bundle app-bundle))
                 (#/awakeFromNib self))
                (t
                 ;; if there isn't a valid nib or main-func process, make sure there
                 ;; is a menu-name so that we create some default menu-items.
                 (if (and (null doc-type) doc-class)
                   (setf doc-type (symbol-name (class-name doc-class))))))
          (let ((mm (#/mainMenu #&NSApp)))
            (unless (eql mm starting-main-menu)
              ;; loading the nib or running a main-func resulted in a new main menu,
              ;; so capture it
              (save-main-menu)
              (setf (saved-menu-key self) mm)
              (set-app-menu mm) ;; only 1 app main menu permitted at a time
              (setf (gethash mm dc-hash) self)))))
      (when (and doc-type (eql (#/mainMenu #&NSApp) starting-main-menu))
        ;; Use the document type to create some additional menus if loading the bundle
        ;; didn't already result in a new main menu.
        (setf (type-ns-str self) (ccl::%make-nsstring doc-type))
        (push (make-and-install-menuitems-after "File" "New"
                                                (list (concatenate 'string "New " doc-type) 
                                                      "newDocument"
                                                      nil
                                                      self))
              (installed-menuitems self))
        (push (make-and-install-menuitems-after "File" "Open..."
                                                (list (concatenate 'string "Open " doc-type "...") 
                                                      "openDocument"
                                                      nil
                                                      self))
              (installed-menuitems self))
        (push (make-and-install-menuitems-after "File" "Print"
                                                (list (concatenate 'string "Print " doc-type) 
                                                      "printDocument:"
                                                      nil
                                                      self))
              (installed-menuitems self)))))

  (defun doc-controller-for-menu-item (menu-item)
    (gethash (key-for-menuitem menu-item) dc-hash nil))

  (defun doc-controller-for-doc-type (doc-type-str)
    (gethash doc-type-str doc-type-dc-hash nil))

  (defun controlled-doc-types ()
    (let ((types nil))
      (maphash #'(lambda (k v)
                   (declare (ignore v))
                   (push k types))
               doc-type-dc-hash)
      types))

)

(defun docs-of-type (doc-type-str)
  (let ((dc (doc-controller-for-doc-type doc-type-str)))
    (when dc
      (open-documents dc))))

;; Methods to create a doc-controller specific to a specified class
;; This is called when the document is to be managed under the CCL IDE

(defun make-doc-controller (doc-class-name delegate-class-name doc-type-string file-ext-string &optional (bundle-path nil))
  (on-main-thread
   (let ((cn (ensure-class-name doc-class-name))
         (dcn (and delegate-class-name 
                   (non-empty-string delegate-class-name)
                   (ensure-class-name delegate-class-name)))
         (bundle (and bundle-path (lisp-bundle-with-path bundle-path)))
         (ldc (make-instance 'lisp-doc-controller)))
     (when bundle
       ;; Required so that we can later find the correct bundle for the document class
       ;; which in turn is needed when loading the nib for some document window
       (load-bundle bundle))
     (initialize-doc-controller ldc
                                :doc-class (or (find-class cn nil) (find-class 'lisp-document))
                                :delegate-class (and dcn (find-class dcn nil))
                                :doc-type doc-type-string
                                :file-ext file-ext-string
                                :app-bundle bundle)
     (set-doc-controller-for-class cn ldc))))

(defmethod open-documents ((self lisp-doc-controller))
  ;; If objects in our documents slot have a reference count of 1, then
  ;; they are no longer also owned by the shared-document-controller and
  ;; must have been closed. We return what's left.
  (setf (documents self)
        (mapcan #'(lambda (doc)
                    (unless (eql (#_CFGetRetainCount doc) 1)
                      (list doc)))
                (documents self))))

(defmethod close-document ((self lisp-doc-controller) doc)
  (when (find doc (open-documents self) :test #'eql)
    (setf (documents self) (delete doc (documents self) :test #'eql))
    (#/close doc)
    (#/release doc)))

(defmethod close-open-documents ((self lisp-doc-controller))
  (dolist (doc (open-documents self))
    (close-document self doc)))

(defmethod watch-document ((self lisp-doc-controller) doc)
  (#/addObserver:selector:name:object: 
   (#/defaultCenter ns:ns-notification-center)
   self
   (ccl::@selector "lispDocumentDidClose:")
   (coerce-obj "LispDocumentDidClose" 'ns:ns-string)
   doc))

(objc:defmethod (#/lispDocumentDidClose: :void)
                ((self lisp-doc-controller) (notif :id))
  ;; remove doc from the list of documents
  (let ((doc (#/object notif)))
    (when (find doc (open-documents self) :test #'eql)
      (#/release doc)
      (setf (documents self) (delete doc (documents self) :test #'eql)))))

(objc:defmethod (#/delegate :id)
                ((self lisp-doc-controller))
  (delegate self))

(objc:defmethod (#/setDelegate: :void)
                ((self lisp-doc-controller) (del :id))
  (setf (delegate self) del))

(objc:defmethod (#/dealloc :void)
                ((self lisp-doc-controller))
  ;; make sure that any menu items for docs of this type are removed.
  (when (#/isRunning #$NSApp)
    (add-to-main-menu (starting-menu) 0)
    (delete-menu (saved-menu-key self))
    (set-app-menu nil)
    (setf (saved-menu-key self) nil) ;; just a precaution
    (dolist (im (installed-menuitems self))
      (remove-menuitems im))
    (when (type-ns-str self)
      (#/release (type-ns-str self)))
    (when (ext-ns-str self)
      (#/release (ext-ns-str self)))
    ;;(when (ldc-open-pnl self)
    ;;  (#/release (ldc-open-pnl self)))
    (dolist (obj (bundle-objects self))
      (unless (typep obj 'ns:ns-menu)
        (#/release obj)))
    (dolist (doc (documents self))
      ;; open documents should have been retained by the shared-document-controller
      ;; so we can release them here without harm
      (#/release doc))
    (call-next-method)
    (objc:remove-lisp-slots self)))

(objc:defmethod (#/newDocument :void)
                ((self lisp-doc-controller))
  (when (document-class self)
    (let ((new-doc (#/initWithType:error: (#/alloc (document-class self))
                                          (lisp-to-temp-nsstring (doc-type-name self))
                                          (%null-ptr))))
      (when (obj-if-not-null new-doc)
        (push new-doc (documents self))
        (watch-document self new-doc)
        ;; register the document with the shared controller so that things like
        ;; "save" and "close" will work properly
        (#/addDocument: (doc-ctrlr self) new-doc)
        (#/makeWindowControllers new-doc)
        (#/showWindows new-doc)))))

(objc:defmethod (#/openDocument :void)
                ((self lisp-doc-controller))
  (when (document-class self)
    (let* ((ldc-open-pnl (make-open-panel 
                          :allowed-file-types (and (file-ext self) (list (file-ext self)))))
           (result (#/runModal ldc-open-pnl)))
      (when (eql result #$NSOKButton)
        (let ((urls (#/URLs ldc-open-pnl)))
          (dotimes (i (#/count urls))
            (let ((doc (#/alloc (document-class self))))
              (setf doc (#/initWithContentsOfURL:ofType:error: 
                         doc
                         (#/objectAtIndex: urls i)
                         (lisp-to-temp-nsstring (doc-type-name self))
                         (%null-ptr)))
              (if doc
                (progn
                  (pushnew doc (documents self))
                  (watch-document self doc)
                  (#/addDocument: (doc-ctrlr self) doc)
                  (#/makeWindowControllers doc)
                  (#/showWindows doc))
                (#_NSRunAlertPanel #@"ALERT" 
                                   #@"Could not open specified file ... ignoring it."
                                   #@"OK"  
                                   (%null-ptr)
                                   (%null-ptr))))))))))
  
(objc:defmethod (#/validateMenuItem: #>BOOL) 
                ((self lisp-doc-controller) (item :id))
  (let* ((action (#/action item))
         (top-win (#/keyWindow #&NSApp))
         (top-doc (and top-win
                       (#/document top-win))))
    (cond ((eql action (ccl::@selector "printDocument:"))
           (if (and top-doc (not (%null-ptr-p top-doc)) (eq (class-of top-doc) (document-class self)))
             #$YES
             #$NO))
          ((or (eql action (ccl::@selector "openDocument:"))
               (eql action (ccl::@selector "newDocument:")))
           (if (document-class self)
             #$YES
             #$NO))
          (t
           (call-next-method item)))))

(objc:defmethod (#/printDocument: :void)
                ((self lisp-doc-controller) (sender :id))
  ;; This gets printDocument messages that are directed here instead of to the first responder
  ;; in order to avoid causing problems for CCL documents that don't implement everything needed
  ;; to make this work.
  ;; We just redirect back to the top document since we know if the menuitem was enabled
  ;; that the top window must have been of the correct type.
  (let ((top-doc (#/objectAtIndex: (#/orderedDocuments #&NSApp) 0)))
    (#/printDocument: top-doc sender)))
  
(objc:defmethod (#/awakeFromNib :void)
                ((self lisp-doc-controller))
  ;; called after we have loaded the nib for some application being loaded under the IDE
  ;; First save off the main menu. If it was replaced by virtue of loading the NIB then
  ;; we will now have both the original and new versions available for menu swapping.
  (save-main-menu)
  (when (and (eql (%null-ptr) (delegate self)) (delegate-class self))
    ;; set our delegate to an instance of the specified class
    (setf (delegate self) (make-instance (delegate-class self))))
  (when (and (not (eql (%null-ptr) (delegate self)))
             (#/respondsToSelector: (delegate self) (ccl::@selector "applicationWillFinishLaunching:")))
    (#/applicationWillFinishLaunching: (delegate self) 
                                       (#/notificationWithName:object:
                                        ns:ns-notification
                                        #&NSApplicationWillFinishLaunchingNotification
                                        self))))

(objc:defmethod (#/methodSignatureForSelector: :id)
                ((self lisp-doc-controller) (sel #>SEL))
  (cond ((and (not (eql (%null-ptr) (delegate self)))
              (#/respondsToSelector: (delegate self) sel))
         (#/methodSignatureForSelector: (delegate self) sel))
        ((#/respondsToSelector: #&NSApp sel)
         (#/methodSignatureForSelector: #&NSApp sel))
        (t 
         ;; shouldn't ever happen
         (%null-ptr))))

(objc:defmethod (#/forwardInvocation: :void)
                ((self lisp-doc-controller) (inv :id))
  (let ((sel (#/selector inv)))
    (cond ((and (not (eql (%null-ptr) (delegate self)))
                (#/respondsToSelector: (delegate self) sel))
           ;;(ns-log (format nil "Forwarding ~s to ~s" (find-selector-match sel) (delegate self)))
           (#/invokeWithTarget: inv (delegate self)))
          ((#/respondsToSelector: #&NSApp sel)
           ;; (ns-log (format nil "Forwarding ~s to ~s" (find-selector-match sel) #&NSApp))
           (#/invokeWithTarget: inv #&NSApp))
          (t 
           (#/doesNotRecognizeSelector: self sel)))))

(objc:defmethod (#/forwardingTargetForSelector: :id)
                ((self lisp-doc-controller) (sel #>SEL))
  (cond ((and (not (eql (%null-ptr) (delegate self)))
              (#/respondsToSelector: (delegate self) sel))
         (delegate self))
        ((#/respondsToSelector: #&NSApp sel)
         #&NSApp)
        ((#/respondsToSelector: (doc-ctrlr self) sel)
         (doc-ctrlr self))
        (t 
         (call-next-method sel))))

(objc:defmethod (#/respondsToSelector: #>BOOL)
                ((self lisp-doc-controller) (sel #>SEL))
  ;; since we're going to forward anything we don't understand to the NSApp
  ;; we can say that we respond to everything that it does.
  (if (or (call-next-method sel)
          (#/respondsToSelector: #$NSApp sel)
          (and (not (eql (%null-ptr) (delegate self)))
              (#/respondsToSelector: (delegate self) sel))
          (member sel
                  (list (ccl::@selector "newDocument")
                        (ccl::@selector "openDocument")
                        (ccl::@selector "awakeFromNib"))
                  :test #'eql))
    #$YES
    #$NO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional method for lisp-documents used only when running them 
;; under the IDE. (which is why it is declared in this file).
;; Overrides the method declared in lisp-document.lisp.

(objc:defmethod (#/prepareSavePanel: #>BOOL) 
                ((self lisp-document) (panel :id))
  (let* ((dc (doc-controller-for-class (class-name (class-of self))))
         (typ (and dc (ext-ns-str dc))))
    (when (obj-if-not-null typ)
      (#/setRequiredFileType: panel typ)))
  #$YES)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifications to CCL functionality to permit loading apps under the CCL IDE

;; Since 
;; 1. #/newDocument: and #/openDocument: are sent to the first responder, and
;; 2. the Applications delegate is in the first responder chain before the 
;;    sharedDocumentController, and 
;; 3. these methods are not already implemented in the default delegate for the CCL IDE,
;; We can implement these as additional methods for the CCL IDE delegate and divert
;; them to the appropriate lisp-doc-controller if needed.

(objc:defmethod (#/newDocument: :void)
                ((self gui::ide-application-delegate) (sender :id))
  ;; Check to see whether this came from one of our app menus. If so hand it to 
  ;; the lisp-doc-controller that handles that app. If not, hand it off to the
  ;; sharedDocumentController, just as would have happened had we not intervened.
  (let* ((dc (doc-controller-for-menu-item sender)))
    (if dc
      (#/newDocument dc)
      (#/newDocument: (#/sharedDocumentController ns:ns-document-controller) sender))))

(objc:defmethod (#/openDocument: :void)
                ((self gui::ide-application-delegate) (sender :id))
  ;; Check to see whether this came from one of our app menus. If so hand it to 
  ;; the lisp-doc-controller that handles that app. If not, hand it off to the
  ;; sharedDocumentController, just as would have happened had we not intervened.
  (let ((dc (doc-controller-for-menu-item sender)))
    (if dc
      (#/openDocument dc)
      (#/openDocument: (#/sharedDocumentController ns:ns-document-controller) sender))))

(provide :lisp-doc-controller)
