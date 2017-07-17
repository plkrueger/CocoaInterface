;; lisp-app-doc.lisp

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

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize)
  (require :ccl-additions-for-cocoa-tools)
  ;;(require :builder-utilities)
  (let* ((ccl:*default-file-character-encoding* :iso-8859-1))
    (require :builder-utilities))
  (require :file-monitor)
  (require :lisp-controller)
  (require :class-convert)
  (require :ide-bundle)
  (require :lisp-app-delegate)
  (require :lisp-document)
  (require :lisp-doc-controller)
  (require :custom-app-init)
  (require :menu-utils)
  (require :open-panel)
  (require :utility)
  (require :ts-queue)
  (require :alert)
  (require :class-convert)
  (require :interactive-app))

(in-package :ad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions used here and called by interface objects

(defvar *doc-role-strings* (list "Editor" "Viewer" "Shell" "None"))

(defstatic *ccl-ide-proto-plist* 
           (probe-file "ccl:contrib;cocoa-ide;krueger;InterfaceProjects;Cocoa Dev;CCL IDE Add In Proto.plist"))

(defun find-app-classes ()
  (mapcar #'(lambda (cls)
              (string-downcase (class-name-string cls)))
          (cons ns:ns-application (recursive-map #'class-direct-subclasses ns:ns-application))))

(defun find-app-delegate-classes ()
  (let ((c1 (find-class 'gui::lisp-application-delegate nil))
        (c2 (find-class 'simple-lisp-app-delegate nil)))
    (nconc
     (mapcar #'(lambda (cls)
                 (string-downcase (class-name-string cls)))
             (cons c1 (recursive-map #'class-direct-subclasses c1)))
     (mapcar #'(lambda (cls)
                 (string-downcase (class-name-string cls)))
             (cons c2 (recursive-map #'class-direct-subclasses c2))))))

(defun find-document-classes ()
  (mapcar #'(lambda (cls)
              (string-downcase (class-name-string cls)))
          (cons ns:ns-document
                (recursive-map #'class-direct-subclasses ns:ns-document))))

(defun uti-extension (uti)
  ;; a uti is in the form: com.<something>. ... .<extension> so we just extract the extension
  (when uti
    (let* ((str (string uti))
           (last-dot-pos (position #\. str :test #'char= :from-end t)))
      (subseq str (if last-dot-pos (1+ last-dot-pos) 0)))))

(defun make-empty-adjustable-array ()
  (make-array '(8) :adjustable t :fill-pointer 0))

(defun string-first (str)
  ;; returns first element of a string as a string
  (if (stringp str)
    (let ((sp-pos (position #\space str)))
      (if sp-pos
        (subseq str 0 sp-pos)
        str))
    ""))

(defun array-to-string (arr)
  ;; arr should be an array of strings
  (format nil "~{~a~^ ~}" (coerce arr 'list)))

(defun make-string-array (str)
  ;; take a string that is a set of space-delimited items and 
  ;; turn it into an array of strings, where each string is
  ;; formed from one of the items.
  (do* ((st str 
            (unless (<= (length st) (length next-str))
              (subseq st (1+ (length next-str)))))
        (next-str (string-first st)
                  (string-first st))
        (res (make-empty-adjustable-array)))
       ((string= next-str "") res)
    (vector-push-extend next-str res)))

(defun base-name (file-str)
  (let ((last-slash (position #\/ file-str :from-end t)))
      (if last-slash
        (subseq file-str (1+ last-slash))
        file-str)))

(defun app-name-from-path (bundle-path)
  (let* ((str (first (last (pathname-directory bundle-path))))
         (dot-pos (position #\. str)))
    (if dot-pos
      (subseq str 0 dot-pos)
      str)))

(defun module-name (file-str)
  (let* ((base-name (base-name file-str))
         (last-dot (position #\. base-name :from-end t))
         (str (if last-dot
                (subseq base-name 0 last-dot)
                base-name)))
      (string-upcase str)))

(defun objc-class-name-from-class-string (cl-str)
  ;; cl-str is a string that includes a package qualifier
  (let ((pos (position #\: cl-str :from-end t)))
    (if pos
      (ccl::compute-objc-classname (string-upcase (subseq cl-str (1+ pos))))
      cl-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for plist files

#|
(defconstant *plist-keys*
  (list "CCLDelegateClass"                ;; The class of the app object's delegate (unless set when 
                                          ;;     NSPrincipalClass object is created)
        "CFBundleDevelopmentRegion"       ;; e.g. English
        "CFBundleDisplayName"             ;; localized app name usually in InfoPlist.strings files
                                          ;;    in language-specific resource dirs
        "CFBundleDocumentTypes"           ;; Array of dictionaries defining document types supported
        "CFBundleExecutable"              ;; Name of executable file in bundle
        "CFBundleHelpBookFolder"          ;; Directory where help book resides
        "CFBundleHelpBookName"            ;; Name of help book
        "CFBundleIconFile"                ;; name of single icons file
        "CFBundleIconFiles"               ;; array of strings that identifies icon files used by app
        "CFBundleIdentifier"              ;; e.g. com.clozure.appName, reverse-DNS UTI string
        "CFBundleInfoDictionaryVersion"   ;; e.g. 6.0 ??
        "CFBundleName"                    ;; short name of bundle, usually app name
        "CFBundlePackageType"             ;; "APPL" for app bundles
        "CFBundleShortVersionString"      ;; string with 3 period-separated integers - version of app
        "CFBundleSignature"               ;; Four-character bundle (Application) identifier
        "CFBundleURLTypes"                ;; array of dictionaries?
        "CFBundleVersion"                 ;; app-specific version string
        "LSMinimumSystemVersion"          ;; minimum version of Mac OSX required to run app; e.g. "10.6.4"
        "NSAppleScriptEnabled"            ;; Boolean value
        "NSHumanReadableCopyright"        ;; copyright notice; can be localized as for CFBundleDisplayName
        "NSMainNibFile"                   ;; base name (i.e. no .nib extension) of main nib file
        "NSPrincipalClass"                ;; some subclass of NSApplication; likely LispApplication or some subclass
        "UTExportedTypeDeclarations"      ;; 
        ))
|#

;; the following two functions are used to serialize fields that are hash tables
;; with the constraint that values are either embedded hash-tables that are
;; also serialized or values that can be printed to and read from strings.
;; Keys in the hash-tables are always strings.

(defun ht-to-assoc (ht)
  (when (hash-table-p ht)
    (let ((res nil))
      (maphash #'(lambda (k v)
                   (when (typep v 'hash-table)
                     (setf v (ht-to-assoc v)))
                   (setf res (acons k v res)))
               ht)
      res)))

(defun assoc-to-ht (alst)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair alst ht)
      (setf (gethash (car pair) ht)
            (if (listp (cdr pair))
              (assoc-to-ht (cdr pair))
              (cdr pair))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-doc-type
;; 
;; Contains information about a single document type supported by the application

(defclass lisp-doc-type ()
  ((dt-doc :accessor dt-doc
           :initarg :doc)
   (dt-doc-type :accessor dt-doc-type
                :initform "New Doc Type"
                :kvo "dtDocType"
                :undo "set document type")
   (dt-doc-class :accessor dt-doc-class
                 :initform ""
                 :kvo "dtDocClass"
                 :undo "set document class")
   (dt-file-ext :accessor dt-file-ext
                :initform ""
                :kvo "dtFileExt"
                :undo "set document extention")
   (dt-export-uti :accessor dt-export-uti
                  :initform nil
                  :kvo "dtExportUTI"
                  :undo "set Export UTI")
   (dt-uti :accessor dt-uti
           :initform ""
           :kvo "dtUTI"
           :undo "set document UTI")
   (dt-doc-role :accessor dt-doc-role
                :initform ""
                :kvo "dtDocRole"
                :undo "set application role for document")
   (dt-icon-full-path :accessor dt-icon-full-path
                      :initform nil)
   (dt-icon-file :accessor dt-icon-file
                 :initform ""
                 :kvo "dtIconFile")
   (dt-owner-for-doc :accessor dt-owner-for-doc
                     :initform nil
                     :kvo "dtOwnerForDoc"
                     :undo "change to App is Owner for Doc Type"))
  (:default-initargs
      :doc nil))

(defmethod undo-target ((self lisp-doc-type))
  (dt-doc self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-app-doc
;; 
;; Contains all the parameter values required to build a particular application.
;; Serves as a data source for bindings made from the application build window.

(defclass lisp-app-doc (lisp-document)
  ((app-name :accessor app-name
             :initform "My Application"
             :kvo "appName"
             :undo "set application name")
   (app-exec :accessor app-exec
             :initform "myapp"
             :kvo "appExec"
             :undo "set executable name")
   (app-bundle-id :accessor app-bundle-id
                  :initform "com.clozure.apps.myapplication"
                  :kvo "appBundleID"
                  :undo "set bundle ID")
   (app-bundle-sig :accessor app-bundle-sig
                  :initform "MyAp"
                  :kvo "appBundleSig"
                  :undo "set bundle signature")
   (app-version :accessor app-version
                :initform "1.0"
                :kvo "appVersion"
                :undo "set application version")
   (app-min-os :accessor app-min-os
                :initform "10.7"
                :kvo "appMinOS"
                :undo "set minimum OS")
   (app-directory :accessor app-directory
                  :initform nil
                  :kvo "appDirectory"
                  :undo "set application directory")
   (app-class :accessor app-class
              :initform ""
              :kvo "appClass"
              :undo "set application class")
   (app-delegate-class :accessor app-delegate-class
                       :initform ""
                       :kvo "appDelegateClass"
                       :undo "set app delegate class")
   (app-source-full-path :accessor app-source-full-path
                         :initform nil)
   (app-source-abbrev-path :accessor app-source-abbrev-path
                           :initform ""
                           :kvo "appAbbrevSrc")
   (app-source-module :accessor app-source-module
                      :initform nil)
   (app-icon-full-path :accessor app-icon-full-path
                       :initform nil)
   (app-icon-file :accessor app-icon-file
                  :initform ""
                  :kvo "appIconFile")
   (app-init-func :accessor app-init-func
                  :initform ""
                  :kvo "appInitFunc"
                  :undo "set app init function")
   (app-doc-types :accessor app-doc-types
                  :initform (make-empty-adjustable-array) 
                  :kvo "docTypes")
   (app-info-plist :accessor app-info-plist
                   :initform (make-hash-table :test #'equal))
   (app-info-plist-imported :accessor app-info-plist-imported
                            :initform nil)
   (app-info-plist-date :accessor app-info-plist-date
                        :initform 0)
   (app-bundle-path :accessor app-bundle-path
                    :initform nil
                    :kvo "appBundlePath"
                    :undo "set bundle path")
   ;; some lists of possible values for various fields in the window
   (app-classes :accessor app-classes
                :initform nil
                :kvo "appClasses")
   (app-delegate-classes :accessor app-delegate-classes
                         :initform nil
                         :kvo "appDelegateClasses")
   (doc-classes :accessor doc-classes
                :initform nil
                :kvo "appDocClasses")
   (doc-roles :accessor doc-roles
                :initform *doc-role-strings*
                :kvo "appDocRoles")
   (doc-controller :accessor doc-controller
                   :initform nil)
   (win-controller :accessor win-controller
                   :initform nil)
   (task-queue :accessor task-queue
               :initform (make-instance 'ts-queue))
   (task-process :accessor task-process
                 :initform nil)
   (update-lock :accessor update-lock :initform (make-lock)))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/close :void)
                ((self lisp-app-doc))
  (when (task-process self)
    ;; tell process to quit
    (queue-task self nil))
  (call-next-method))

;; Methods to lock when necessary

(defmethod (setf app-info-plist) :around (new-val (self lisp-app-doc))
  (declare (ignore new-val))
  (with-lock-grabbed ((update-lock self))
    (call-next-method)))

;; Methods to support doing some tasks in a separate thread

(defmethod queue-task ((self lisp-app-doc) task-func)
  ;; If task-func is not a function, the process will quit
  (with-slots (task-queue task-process) self
    (unless task-process
      (setf task-process (process-run-function "lisp app doc tasks" #'run-app-doc-tasks self)))
    (push-queue task-queue task-func)))

(defmethod run-app-doc-tasks ((self lisp-app-doc))
  (let ((q (task-queue self)))
    (do ((new-task (pop-queue q)
                   (pop-queue q)))
        ((not (functionp new-task)) nil)
      (funcall new-task))))
  
;; Notification functions

(defmethod bound-slot-modified ((self lisp-app-doc) (slot-name (eql 'app-bundle-sig)))
  (with-slots (app-bundle-sig) self
    (when (> (length app-bundle-sig) 4)
      (alert :text (format nil "Signature ~s should be 4 characters or less, only the first 4 will be used" app-bundle-sig))
      (setf app-bundle-sig (subseq app-bundle-sig 0 3)))))

(defmethod bound-slot-modified ((self lisp-app-doc) (slot-name (eql 'app-name)))
  ;; called when the app-name slot is modified. Check to see if we should
  ;; rename the bundle.
  (with-slots (app-bundle-path app-name) self
    (when (and (non-empty-string app-name) 
               app-bundle-path 
               (not (string= (app-name-from-path app-bundle-path) app-name))
               (probe-file app-bundle-path))
      (let ((new-app-bundle-path (make-pathname :directory (append (butlast (pathname-directory app-bundle-path))
                                                                   (list (concatenate 'string app-name ".app"))))))
        (unless (equal app-bundle-path new-app-bundle-path)
          (if (probe-file new-app-bundle-path)
            (let ((res (alert :text (format nil
                                            "~a already exists. Continuing rename will remove existing file."
                                            (pathname new-app-bundle-path))
                              :right "Cancel Rename"
                              :left "Continue with Rename")))
              (when (eq res :left)
                (rename-file app-bundle-path new-app-bundle-path :if-exists :overwrite)))
            (rename-file app-bundle-path new-app-bundle-path))
          (setf app-bundle-path new-app-bundle-path)
          (save-info-plist self))))))

;; Methods to manage windows

(defmethod window-build-funcs ((self lisp-app-doc))
  (list #'make-app-doc-window))

(defmethod document-window-controller-classes ((self lisp-app-doc))
  (list (find-class 'lisp-app-win-controller)))

;; lisp-app-doc Methods

(defmethod archive-slots ((self lisp-app-doc))
  ;; we want to save everything except for the doc-controller, win-controller, and update-lock slots
  ;; also don't save app-info-plist since we'll read if from the bundle if it exists. That way
  ;; it can be edited externally and those values will be preserved when the doc is opened.
  (let ((default-slots (call-next-method)))
    (set-difference default-slots '(doc-controller win-controller update-lock app-info-plist
                                    app-classes app-delegate-classes doc-classes doc-roles
                                    task-queue task-process))))

(defmethod init-bundle ((self lisp-app-doc))
  (setf (app-bundle-path self) nil)
  (setf (app-info-plist self) (make-hash-table :test #'equal))
  (setf (app-info-plist-imported self) nil)
  (reinit-bundle self))

(defmethod reinit-bundle ((self lisp-app-doc))
  (with-slots (app-name app-bundle-path app-creator-string app-icon-file app-icon-full-path
                        app-source-full-path app-bundle-sig app-info-plist app-doc-types
                        app-info-plist-imported app-delegate-class app-exec) self
    (let* ((bpath (and app-bundle-path (probe-file app-bundle-path)))
           (bundle-dir (unless bpath
                         (iu:open-panel :choose-dirs t
                                        :choose-files nil
                                        :prompt "Create App Bundle Here"))))
      ;; (set-defined-objc-classes self)
      (when bundle-dir
        ;; Create a new .app bundle
        (setf bpath (ccl::make-application-bundle :name app-name
                                                  :project-path bundle-dir))
        (setf app-bundle-path bpath)
        (#/updateChangeCount: self #$NSChangeDone))
      
      (when bpath
        ;; Create a pkginfo file
        ;; This isn't absolutely needed, but is a good idea
        (ccl::write-pkginfo (ccl::path app-bundle-path "Contents" "PkgInfo") "APPL" app-bundle-sig)

        ;; Copy any specified icon files into the bundle resource directory
        (let ((res-path (ccl::path bpath "Contents" "Resources" app-icon-file)))
          (when (and (non-empty-string app-icon-full-path)
                     (not (equal app-icon-full-path res-path)))
            (copy-file app-icon-full-path res-path :if-exists :overwrite)))
        (do-sequence (dt app-doc-types)
          (let ((res-path (ccl::path bpath "Contents" "Resources" (dt-icon-file dt))))
            (when (and (non-empty-string (dt-icon-full-path dt))
                       (not (equal (dt-icon-full-path dt) res-path)))
            (copy-file (dt-icon-full-path dt) res-path :if-exists :overwrite))))

        ;; Put Info.plist to the bundle
        (save-info-plist self)

        ;; Make sure the OS knows that the bundle was modified. Othewise it will use old
        ;; cached information from the bundle.
        (ccl::touch bpath)))))

(defmethod use-bundle ((self lisp-app-doc))
  ;; Associate an existing bundle with this document. Perhaps most commonly used when
  ;; a developer copies an existing bundle to use as a starting point and then creates
  ;; a new document and wants to associate the copied bundle with it.
  (let ((new-bundle-file (open-panel :prompt "Select bundle"
                                     :types '("bundle" "app" "bndl"))))
    (when new-bundle-file
      (with-slots (app-bundle-path) self
        ;; unfortunately what comes back isn't recognized as a directory so we have
        ;; to slightly patch the name
        (setf app-bundle-path (pathname (concatenate 'string new-bundle-file "/")))
        (read-info-plist self))
      (#/updateChangeCount: self #$NSChangeDone))))

(defmethod doc-type-string ((self lisp-app-doc))
  (with-slots (app-doc-types app-name) self
    (when (plusp (length app-doc-types))
      (let ((dt (or (find t app-doc-types :key #'dt-owner-for-doc)
                    (elt app-doc-types 0))))
        (if dt
          (or (non-empty-string (dt-doc-type dt))
              (non-empty-string (dt-file-ext dt)))
          (concatenate 'string app-name " Document"))))))

(defmethod primary-doc-class ((self lisp-app-doc))
  (with-slots (app-doc-types) self
    (when (plusp (length app-doc-types))
      (dt-doc-class (or (find t app-doc-types :key #'dt-owner-for-doc)
                        (elt app-doc-types 0))))))

(defmethod doc-file-extension ((self lisp-app-doc))
  ;; Either use the first extension specified in the document window or the default "LDOC"
  (with-slots (app-doc-types app-name) self
    (let ((dt (when (plusp (length app-doc-types))
                (or (find t app-doc-types :key #'dt-owner-for-doc)
                    (elt app-doc-types 0)))))
      (or (and dt (non-empty-string (dt-file-ext dt)))
          "LDOC"))))

(defmethod assure-source-loaded ((self lisp-app-doc))
  (with-slots (#|app-main-source-res |# app-source-full-path app-source-module #| app-source-files |#) self
    (when app-source-module
      (unless (member app-source-module *modules* :test #'string-equal)
        (with-errors-alerted
            (require app-source-module
                     (pathname app-source-full-path)))))))
      
(defmethod load-lisp-app ((self lisp-app-doc))
  (with-slots (app-bundle-path #| app-main-nib |# app-init-func #| app-doc-class app-include-source |#
               #| app-source-files app-doc-types |# doc-controller #| app-main-source-res |# app-delegate-class) self
    ;; first make sure the bundle is physically sync'ed with all the values in the lisp-app-doc
    (when doc-controller
      (#/release doc-controller)
      (setf doc-controller nil))
    (reinit-bundle self)
    (unless app-bundle-path
      ;; must have cancelled out of creating a new bundle, so just exit
      (return-from load-lisp-app nil))
    (let ((app-doc-class (primary-doc-class self))
          ;;(bndl (lisp-bundle-with-path app-bundle-path))
          (did-something nil))
      (setf did-something (assure-source-loaded self))

      ;; Next we do different things depending on the current state of the bundle:
      ;;
      ;; If the bundle has an application class defined for it, but does not have a main menu
      ;; then create an instance of lisp-doc-controller that creates and adds some standard
      ;; menu items for that class.
      ;;
      ;; If the bundle has an application class and also an app-init-func,
      ;; then create an instance of lisp-doc-controller that will create the main menu from
      ;; the function and also arrange to save the current menu items so that the user can 
      ;; toggle back and forth between the app menu-items and lisp menu-items
      (if (non-empty-string app-init-func)
        ;; Make an instance of lisp-doc-controller to be owner of any objects created by the app-init-func
        ;; It will act as if it were an NSApplication with respect to global menu items that are created.
        (progn 
          (setf doc-controller
                (make-doc-controller app-doc-class
                                     app-delegate-class
                                     (doc-type-string self)
                                     (doc-file-extension self)
                                     app-bundle-path))
          (show-dev-menu)
          (set-toggle-states)
          (setf did-something t))
        (when (non-empty-string app-doc-class)
          ;; otherwise make an instance of lisp-doc-controller that sets up pseudo menu-items
          ;; for this document
          (if (find-class (read-from-string app-doc-class) nil)
            (progn
              (setf doc-controller
                    (make-doc-controller app-doc-class
                                         app-delegate-class
                                         (doc-type-string self)
                                         (doc-file-extension self)
                                         app-bundle-path))
              (setf did-something t))
            (alert :text (format nil "Specified Document Class: ~a does not exist" app-doc-class)))))
      (unless did-something
        (alert
         :text "No source files to be loaded, no app initialization function specified, no Document Class specifed, so nothing was done.")))))

(defmethod unload-lisp-app ((self lisp-app-doc))
  ;; As much as possible, remove what was loaded
  ;; Open documents for this app are left alone, but menus on which they may depend will be gone.
  ;; They can still be saved and closed, but no new ones can be opened or created.

  ;; Release the doc-controller and set it to nil. Releasing will result in any menuitems that were 
  ;; added by the controller (either directly or via main-menu creation) being removed.
  (when (doc-controller self)
    (#/release (doc-controller self))
    (setf (doc-controller self) nil)))

(defmethod current-executable-path ()
  ;; finds the path (string) to the executable for the Lisp IDE currently running
  ;; This executable will be copied into the app bundle and renamed as needed.
  (first (coerce-obj (#/arguments (#/processInfo ns:ns-process-info)) 'list)))

(defmethod install-exec-sub-task ((self lisp-app-doc))
  (with-slots (app-exec app-class app-bundle-path app-include-source app-doc-types
                        app-source-full-path app-source-module) self
    ;; start a subordinate lisp to install the executable
    (with-ccl-stream (other-ccl)
      (with-stream-window (strm "Installation Progress")
        (let ((exec-path (namestring (ccl::path (ccl::bundle-executable-path app-bundle-path)
                                                (ccl::bundle-executable-name app-exec))))
              (remote-result nil))
          ;;(start-trace other-ccl)
          (format strm "Beginning to install executable in ~s at ~a" app-bundle-path (time-string (now)))
          (format strm "~%Starting Remote Lisp and requiring :cocoa-without-ide-init")
          (setf remote-result (in-subordinate-ccl (other-ccl) (require :cocoa-without-ide-init)))
          (if (eq :cocoa-without-ide-init remote-result)
            (progn
              (format strm "~%Starting Remote Lisp and requiring :cocoa-without-ide-init successful")
              (format strm "~%Requiring :install-executable in remote lisp")
              (setf remote-result (in-subordinate-ccl (other-ccl) (require :install-executable))))
            (progn
              (alert :text (format nil "Remote (require :cocoa-without-ide-init) failed. ~s returned" remote-result))
              (return-from install-exec-sub-task nil)))
          (if (eq :install-executable remote-result)
            (progn
              (format strm "~%Requiring :install-executable in remote lisp successful")
              (format strm "~%Requiring ~s in remote lisp" app-source-module)
              (setf remote-result (remote-let (other-ccl)
                                              ((require-source app-source-module)
                                               (require-path app-source-full-path))
                                              (when require-path
                                                (require require-source require-path))
                                              ;; a non-null result will indicate an error occurred
                                              nil)))
            (progn
              (alert :text (format nil "Remote (require :install-executable) failed. ~s returned" remote-result))
              (return-from install-exec-sub-task nil)))
          
          (cond ((stringp remote-result)
                 (alert :text remote-result))
                ((null remote-result)
                 (format strm "~%Requiring ~s in remote lisp successful" app-source-module)
                 (format strm "~%Last step: Saving app from remote lisp")
                 ;; we don't expect the following to return since the subordinate CCL will terminate
                 (remote-let (other-ccl)
                             ((bpath app-bundle-path)
                              (app-name (ccl::bundle-executable-name app-exec)))
                             (gui::save-app bpath app-name))
                 (shell-command (format nil "touch ~s" app-bundle-path))
                 (if (probe-file exec-path)
                   (format strm "~%App creation successful")
                   (alert :text (format nil "After saving, executable not found at ~s" exec-path))))
                (t
                 ;; something weird here
                 ;;(alert :text (format nil "Unknown error trying to make-default-type-method remotely: ~s returned" remote-result))
                 (alert :text (format nil "Unknown error trying to load app source remotely: ~s returned" remote-result))))
          #|(trace-output other-ccl)|#)))))

(defmethod install-executable ((self lisp-app-doc))
  (when (app-bundle-path self)
    (reinit-bundle self)
    ;; running the following as a sub-task permits event processing to continue so that progress can
    ;; be shown in a separate window.
    (queue-task self #'(lambda ()
                         (install-exec-sub-task self)))))

(defmethod run-standalone-app ((self lisp-app-doc))
  (run-program "open"
               (list (namestring (app-bundle-path self)))
               :wait nil))

(defmethod set-dt-icon-paths ((self lisp-doc-type) full-path)
  ;; set the dt-icon-full-path and dt-icon-file slots using the full-path
  (let ((current-val (dt-icon-full-path self)))
    (set-undo self
              #'(lambda ()
                  (set-dt-icon-paths self current-val))
              "set doc type icon file"))
  (setf (dt-icon-full-path self) full-path)
  (setf (dt-icon-file self) (base-name full-path)))

(defmethod select-icon-file ((self lisp-app-doc) (dt lisp-doc-type))
  ;; Use open-panel to allow user to select an icon file
  (let ((new-icon-file (open-panel :prompt "Select")))
    (when new-icon-file
      (set-dt-icon-paths dt new-icon-file))))

(defmethod bound-slot-modified ((self lisp-doc-type) (slot-name (eql 'dt-icon-file)))
  ;; User typed in a (hopefully) full pathname
  (unless (probe-file (dt-icon-file self))
    (alert :title "Warning" :text (format nil "File ~s does not currently exist." (dt-icon-file self))))
  (set-dt-icon-paths self (dt-icon-file self)))

(defmethod set-app-icon-paths ((self lisp-app-doc) full-path)
  ;; set the app-icon-full-path and app-icon-file slots using the full-path
  (let ((current-val (app-icon-full-path self)))
    (set-undo self
              #'(lambda ()
                  (set-app-icon-paths self current-val))
              "set app icon file"))
  (setf (app-icon-full-path self) full-path)
  (setf (app-icon-file self) (base-name full-path)))

(defmethod select-app-icon-file ((self lisp-app-doc))
  ;; Use open-panel to allow user to select an icon file
  (let ((new-icon-file (open-panel :prompt "Select")))
    (when new-icon-file
      (set-app-icon-paths self new-icon-file))))

(defmethod bound-slot-modified ((self lisp-app-doc) (slot-name (eql 'app-icon-file)))
  ;; User typed in a (hopefully) full pathname
  (unless (probe-file (app-icon-file self))
    (alert :title "Warning" :text (format nil "File ~s does not currently exist." (app-icon-file self))))
  (set-app-icon-paths self (app-icon-file self)))

(defmethod set-source-paths ((self lisp-app-doc) full-path)
  ;; set the app-source-full-path, app-source-abbrev-path, and app-source-module slots using the full-path
  (let ((current-val (app-source-full-path self)))
    (set-undo self
              #'(lambda ()
                  (set-source-paths self current-val))
              "set required source file"))
  (setf (app-source-full-path self) full-path)
  (setf (app-source-abbrev-path self) (base-name full-path))
  (setf (app-source-module self) (module-name full-path)))

(defmethod select-src-file ((self lisp-app-doc))
  ;; Use open-panel to allow user to select a source file
  (let ((new-src-file (open-panel :prompt "Select")))
    (when new-src-file
      (set-source-paths self new-src-file))))

(defmethod bound-slot-modified ((self lisp-app-doc) (slot-name (eql 'app-source-abbrev-path)))
  ;; User typed in a (hopefully) full pathname
  (unless (probe-file (app-source-abbrev-path self))
    (alert :title "Warning" :text (format nil "File ~s does not currently exist." (app-source-abbrev-path self))))
  (set-source-paths self (app-source-abbrev-path self)))

(defmethod doc-type-added ((self lisp-app-doc) controller root parent new-child)
  (declare (ignore controller root parent))
  ;; make sure the new doc type has its dt-doc slot set so that undo works
  (setf (dt-doc new-child) self))

(defmethod update-available-classes ((self lisp-app-doc))
  (setf (app-classes self) (find-app-classes))
  (setf (app-delegate-classes self) (find-app-delegate-classes))
  ;; (setf (ctrl-classes self) (find-ctrl-classes))
  (setf (doc-classes self) (find-document-classes)))

(defmethod document-did-open ((self lisp-app-doc))
  ;; Reload the Info.plist from whatever is in the bundle, if it exists
  (read-info-plist self))

(defmethod window-will-close ((self lisp-app-doc))
  (with-slots (doc-controller) self
    (when doc-controller
      (when (open-documents doc-controller)
        (if (and (#/isRunning #$NSApp) 
                 (eql :left (alert :text (format nil "Close open ~a documents?" (doc-type-string self))
                                   :left "YES"
                                   :right "NO")))
          (close-open-documents doc-controller)))
      (#/release doc-controller)
      (setf doc-controller nil))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resource functions

(defmethod add-resource ((self lisp-app-doc) resource-path)
  (let* ((bpath (app-bundle-path self))
         (res-path (and bpath (ccl::path bpath "Contents" "Resources"))))
    (when (probe-file resource-path)
      (copy-file resource-path (ccl::path res-path (base-name resource-path))))))

(defmethod remove-resource ((self lisp-app-doc) resource-base-name)
  (unless (string= resource-base-name "")
    (let* ((bpath (app-bundle-path self))
           (res-path (and bpath (ccl::path bpath "Contents" "Resources" resource-base-name))))
      (when (probe-file res-path)
        (delete-file res-path)))))

(defmethod has-resource ((self lisp-app-doc) resource-base-name)
  (let* ((bpath (app-bundle-path self))
         (res-path (and bpath (ccl::path bpath "Contents" "Resources" resource-base-name))))
    (probe-file res-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Info.plist functions

(defmethod merge-vals-into-plist ((self lisp-app-doc))
  ;; Take values from the fields in the lisp-app window set by the user and make sure
  ;; anything that needs to be reflected in the info.plist is
  (with-slots (app-info-plist app-name app-version app-min-os app-directory app-class app-main-source
                              app-delegate-class app-doc-types #| app-doc-role
                              app-doc-class app-file-ext app-uti |# app-exec app-export-uti
                              app-icon-file #| app-owner-for-doc app-main-nib |# app-bundle-id
                              app-bundle-sig app-include-source #| app-source-classes |#
                              app-main-source-res app-init-func) self
    (setf (gethash "CFBundleName" app-info-plist) app-name)
    (setf (gethash "CFBundleIdentifier" app-info-plist) app-bundle-id)
    (setf (gethash "CFBundleVersion" app-info-plist) app-version)
    (setf (gethash "LSMinimumSystemVersion" app-info-plist) app-min-os)
    (setf (gethash "CFBundleSignature" app-info-plist) app-bundle-sig)
    (setf (gethash "CFBundleExecutable" app-info-plist) app-exec)
    (if app-init-func
      (setf (gethash "CLMainFunc" app-info-plist) app-init-func)
      (remhash "CLMainFunc" app-info-plist))
    (setf (gethash "NSPrincipalClass" app-info-plist)
          (if (non-empty-string app-class)
            (objc-class-name-from-class-string app-class)
            "LispApplication"))
    (setf (gethash "CFBundlePackageType" app-info-plist) "APPL")
    (if (non-empty-string app-icon-file)
      (setf (gethash "CFBundleIconFile" app-info-plist)
            app-icon-file)
      (remhash "CFBundlePackageType" app-info-plist))
    (if (non-empty-string app-delegate-class)
      (setf (gethash "CCLDelegateClass" app-info-plist)
            (objc-class-name-from-class-string app-delegate-class))
      (remhash "CCLDelegateClass" app-info-plist))

    ;; If user has specified info for one or more application documents, make sure 
    ;; that there are correponding entries in the CFBundleDocumentTypes list
    ;; for that info. If you are not also including ide resources, then there
    ;; should also be an entry in the UTExportedTypeDeclarations list.
    (let* ((doc-type-list (coerce (gethash "CFBundleDocumentTypes" app-info-plist) 'list))
           (export-list (coerce (gethash "UTExportedTypeDeclarations" app-info-plist) 'list)))
      (do-sequence (dt app-doc-types)
        (with-slots (dt-doc-type dt-doc-class dt-file-ext dt-doc-role dt-icon-file dt-uti dt-export-uti dt-owner-for-doc) dt
          (when (or (non-empty-string dt-doc-type)
                    (non-empty-string dt-doc-class)
                    (non-empty-string dt-file-ext)
                    (non-empty-string dt-doc-role)
                    (non-empty-string dt-icon-file)
                    (non-empty-string dt-uti))
            (let ((dt-ht (find-if #'(lambda (ht)
                                       (string= dt-doc-type (gethash "CFBundleTypeName" ht)))
                                   doc-type-list))
                  (export-ht (find-if #'(lambda (ht)
                                          (string= dt-uti (gethash "UTTypeIdentifier" ht)))
                                      export-list)))
              (unless dt-ht
                (setf dt-ht (make-hash-table :test #'equal))
                (setf doc-type-list (nconc doc-type-list (list dt-ht))))
              (if (non-empty-string dt-doc-type)
                (setf (gethash "CFBundleTypeName" dt-ht) dt-doc-type)
                (remhash "CFBundleTypeName" dt-ht))
              (if (non-empty-string dt-icon-file)
                (setf (gethash "CFBundleTypeIconFile" dt-ht) dt-icon-file)
                (remhash "CFBundleTypeIconFile" dt-ht))
              (when (string= dt-doc-role "")
                (setf dt-doc-role "Editor"))
              (when (non-empty-string dt-doc-role)
                (setf (gethash "CFBundleTypeRole" dt-ht) dt-doc-role))
              (if (non-empty-string dt-file-ext)
                (setf (gethash "CFBundleTypeExtensions" dt-ht) (make-string-array dt-file-ext))
                (remhash "CFBundleTypeExtensions" dt-ht))
              (if (non-empty-string dt-uti)
                (progn
                  ;; specify what UTIs can be used to read in to represent this type of document
                  (setf (gethash "LSItemContentTypes" dt-ht) (make-string-array dt-uti))
                  ;; specify what UTIs can be used to write out this type of document
                  (setf (gethash  "NSExportableTypes" dt-ht) (make-string-array dt-uti)))
                (progn
                  (remhash "LSItemContentTypes" dt-ht)
                  (remhash "NSExportableTypes" dt-ht)))
              (setf (gethash "LSHandlerRank" dt-ht) 
                    (if dt-owner-for-doc "Owner" "Alternate"))
              (setf (gethash "NSDocumentClass" dt-ht)
                    (if (non-empty-string dt-doc-class)
                      (objc-class-name-from-class-string dt-doc-class)
                      "LispDocument"))
              (when (non-empty-string dt-uti)
                (if dt-export-uti
                  (progn
                    (unless export-ht
                      ;; create an entry for our uti
                      (setf export-ht (make-hash-table :test #'equal))
                      (setf export-list (cons export-ht export-list))
                      (setf (gethash "UTExportedTypeDeclarations" app-info-plist) (coerce export-list 'simple-vector)))
                    ;; identify what UTIs this bundle is exporting to the world
                    (setf (gethash "UTTypeIdentifier" export-ht) dt-uti)
                    ;; bit of a kludge below, but if user really wants to set this they can edit info.plist
                    (setf (gethash "UTTypeConformsTo" export-ht) (coerce (list #$kUTTypeData #$kUTTypeContent) 'simple-vector))
                    (when (non-empty-string dt-doc-type)
                      (setf (gethash "UTTypeDescription" export-ht) dt-doc-type))
                    (let ((tts-hash (gethash "UTTypeTagSpecification" export-ht)))
                      (unless tts-hash
                        (setf tts-hash (make-hash-table :test #'equal))
                        (setf (gethash "UTTypeTagSpecification" export-ht) tts-hash))
                      (setf (gethash "public.filename-extension" tts-hash) (coerce (list dt-file-ext) 'simple-vector))))
                  (when export-ht
                    ;; remove the UTI export entry if it already exists
                    (setf export-list (delete export-ht export-list))
                    (if (null export-list)
                      (remhash "UTExportedTypeDeclarations" app-info-plist)
                      (setf (gethash "UTExportedTypeDeclarations" app-info-plist) (coerce export-list 'simple-vector))))))))))
      (if doc-type-list
        (setf (gethash "CFBundleDocumentTypes" app-info-plist) (coerce doc-type-list 'simple-vector))
        (remhash "CFBundleDocumentTypes" app-info-plist)))))

(defmethod info-plist-to-doc-vals ((self lisp-app-doc))
  ;; Take values from the info-plist and move them into appropriate fields in the document so that
  ;; they will be accurately displayed in the window. Typically this will be done after the user
  ;; edits the plist (by using the "Edit Info.plist" menu command which opens the document's
  ;; info-plist in Apple's Property List Editor) and subsequently saves it.
  (with-slots (app-name app-bundle-id app-version app-min-os app-bundle-sig app-exec app-bundle-path app-icon-file
                        app-icon-full-path app-init-func app-doc-types
                        app-info-plist #| app-main-nib app-doc-class |# app-class app-delegate-class #|app-uti|# ) self
    (setf app-name (gethash "CFBundleName" app-info-plist ""))
    (setf app-bundle-id (gethash "CFBundleIdentifier" app-info-plist ""))
    (setf app-version (gethash "CFBundleVersion" app-info-plist "1.0"))
    (setf app-min-os (gethash "LSMinimumSystemVersion" app-info-plist "10.7"))
    (setf app-bundle-sig (gethash "CFBundleSignature" app-info-plist ""))
    (setf app-exec (gethash "CFBundleExecutable" app-info-plist ""))
    (setf app-init-func (gethash "CLMainFunc" app-info-plist ""))
    (let* ((new-icon-file (gethash "CFBundleIconFile" app-info-plist ""))
           (bundle-path (has-resource self new-icon-file)))
      (if bundle-path
        (unless (string= new-icon-file app-icon-file)
          (remove-resource self app-icon-file)
          (setf app-icon-file new-icon-file)
          (setf app-icon-full-path bundle-path))
        (alert :text (format nil
                             "Ignoring new info.plist value for CFBundleIconFile because ~s is not a bundle resource"
                             new-icon-file))))
    (setf app-class (string-downcase (ns-to-lisp-classname (gethash "NSPrincipalClass" app-info-plist nil) app-class)))
    (setf app-delegate-class (string-downcase (ns-to-lisp-classname (gethash "CCLDelegateClass" app-info-plist nil) app-delegate-class)))
    
    ;; Process all the doc types in the info.plist
    (let* ((doc-types (coerce-obj (gethash "CFBundleDocumentTypes" app-info-plist nil) 'list))
           (export-list (coerce (gethash "UTExportedTypeDeclarations" app-info-plist) 'list)))
      (dolist (dt-ht doc-types)
        (let* ((type-name  (gethash "CFBundleTypeName" dt-ht ""))
               (old-dt (find type-name app-doc-types :key #'dt-doc-type :test #'string=))
               (dt (or old-dt (make-instance 'lisp-doc-type :doc self))))
          (unless old-dt
            (vector-push-extend dt app-doc-types))
          (with-slots (dt-doc-type dt-doc-class dt-file-ext dt-doc-role dt-icon-file 
                       dt-icon-full-path dt-uti dt-export-uti dt-owner-for-doc) dt
            ;; we leave existing values in window/document if there isn't anything in the Info.plist that overrides it
            (setf dt-doc-type (gethash "CFBundleTypeName" dt-ht ""))
            (let* ((new-icon-file (gethash "CFBundleTypeIconFile" dt-ht ""))
                   (bundle-path (has-resource self new-icon-file)))
              (if bundle-path
                (unless (string= new-icon-file dt-icon-file)
                  (remove-resource self dt-icon-file)
                  (setf dt-icon-file new-icon-file)
                  (setf dt-icon-full-path bundle-path))
                (alert :text (format nil
                                     "Ignoring new info.plist value for CFBundleTypeIconFile because ~s is not a bundle resource"
                                     new-icon-file))))
            (setf dt-doc-role (gethash "CFBundleTypeRole" dt-ht ""))
            (let ((types (gethash "LSItemContentTypes" dt-ht nil)))
              (when types
                (setf dt-uti (array-to-string types))))
            (let ((exts (gethash "CFBundleTypeExtensions" dt-ht nil)))
              (when exts
                (setf dt-file-ext (array-to-string exts))))
            (setf dt-owner-for-doc (if (string= (gethash "LSHandlerRank" dt-ht "") "Owner") t nil))
            (setf dt-doc-class (string-downcase (ns-to-lisp-classname (gethash "NSDocumentClass" dt-ht nil) dt-doc-class)))
    
            ;; check to see if UTI was exported
            (if (find-if #'(lambda (ht)
                             (string= dt-uti (gethash "UTTypeIdentifier" ht)))
                         export-list)
              (setf dt-export-uti t)
              (setf dt-export-uti nil))))))))

(defmethod save-info-plist ((self lisp-app-doc))
  (with-slots (app-bundle-path) self
    (let* ((bpath (and app-bundle-path (probe-file app-bundle-path)))
           (ip-path (and bpath (namestring (ccl::path bpath "Contents" "Info.plist")))))
      (flet ((save-it ()
               (merge-vals-into-plist self)
               ;; Don't want to detect this as an external modification of the Info.plist and 
               ;; re-merge the values, so we'll suspend any monitoring of the file that might exist.
               ;; It wouldn't hurt anything, but is a waste of time and would show up in UNDO.
               (suspend-monitoring ip-path)
               (unless (#/writeToFile:atomically: (lisp-to-ns-plist-dict (app-info-plist self))
                                                  (lisp-to-temp-nsstring ip-path)
                                                  #$YES)
                 (ns-log (format nil
                                 "Unknown error while trying to write ~s"
                                 (namestring (ccl::path bpath "Contents" "Info.plist")))))
               (setf (app-info-plist-date self) (file-write-date (ccl::path bpath "Contents" "Info.plist")))
               (resume-monitoring ip-path)))
        (if bpath
          (progn
            (if (and (probe-file ip-path) (> (file-write-date ip-path) (app-info-plist-date self)))
              (case (alert :right "Cancel"
                           :left "Overwrite"
                           :middle "Import"
                           :text "Info.plist changed on disk by another application since last written.")
                (:right (return-from save-info-plist nil))
                (:middle (read-info-plist self))
                (:left (save-it)))
              (save-it)))
          (reinit-bundle self))))))

(defmethod read-info-plist ((self lisp-app-doc) &key (undo nil))
  (let ((bpath (when (app-bundle-path self)
                 (or (probe-file (app-bundle-path self))
                     (setf (app-bundle-path self) nil))))
        (aip (app-info-plist self)))
    (when bpath
      (when undo
        (set-undo self
                  #'(lambda ()
                      (setf (app-info-plist self) aip)
                      (info-plist-to-doc-vals self))
                  "set values from edited Info.plist"))
      (set-info-plist-from-file self (ccl::path bpath "Contents" "Info.plist"))
      (info-plist-to-doc-vals self))))

(defmethod set-info-plist-from-file ((self lisp-app-doc) path)
  ;; While this sets the app-info-plist value it does not set any individual variables
  ;; so that later when we update the app-info-plist from those variables we may augment
  ;; and/or replace values that are there. Effectively this makes what we are reading
  ;; here a default set of values that may be overridden by what the user enters in
  ;; the application window.
  (let* ((pl-path (lisp-to-temp-nsstring (namestring path)))
         (ns-plist (#/dictionaryWithContentsOfFile: ns:ns-mutable-dictionary pl-path)))
    (setf (app-info-plist self)
          (ns-to-lisp-hash-table ns-plist :test 'equal))
    (setf (app-info-plist-date self) (file-write-date path))))
    
(defmethod edit-plist ((self lisp-app-doc))
  ;; Edit plist with Apple's Property List Editor application.
  ;; When user does a "save" there, the plist will automatically be reloaded and re-displayed.
  (with-slots (app-bundle-path) self
    (save-info-plist self)
    ;; set up a file monitor that will restore the plist when it is modified
    ;; check for updates once per second
    (monitor-file (ccl::path app-bundle-path "Contents" "Info.plist")
                  #'(lambda (plist-path)
                      (declare (ignore plist-path))
                      (read-info-plist self :undo t))
                  :wait-secs 1)
    ;; open the plist in the default app, which is normally the Property List Editor
    (run-program "open"
                 (list (namestring (ccl::path app-bundle-path
                                              "Contents"
                                              "Info.plist")))
                 :wait nil)))

(defun install-lisp-app-tools ()
  ;; This is meant to be executed in the lisp listener, so we make sure that everything is
  ;; done on the main thread.
  (on-main-thread
   (let* ((bundle-path (probe-file (first (directory "cocoa-pk:**;lisp-app-doc.bundle"
                                                     :directories t
                                                     :files nil))))
          (bundle-namestring (and bundle-path (namestring bundle-path))))
     (make-doc-controller 'lisp-app-doc
                          nil
                          "Lisp Application"
                          "lapp"
                          bundle-namestring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a delegate for lisp-doc objects.
;; This object will end up as the delegate of the lisp-app-controller object that handles
;; lisp-app-doc instances.

(defclass lisp-doc-app-delegate (lisp-IDE-app-delegate)
  ((lisp-doc-controller :accessor lisp-doc-controller)
   ;; (objc-classes :accessor objc-classes :initform nil)
   (ccl-menuitem :accessor ccl-menuitem
                 :initarg :ccl-menuitem)
   (app-menuitem :accessor app-menuitem
                 :initarg :app-menuitem)
   (dev-menu :accessor dev-menu
             :initarg :dev-menu)
   (menu-key :accessor menu-key :initform (gensym)))
  (:default-initargs
    :dev-menu (%null-ptr)
    :ccl-menuitem (%null-ptr)
    :app-menuitem (%null-ptr))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/applicationWillFinishLaunching: :void)
                ((self lisp-doc-app-delegate) notification)
  (declare (ignore notification))
  ;; In this method do things that need to be done before the event loop
  ;; of the app is started. These things are also done if the app is
  ;; loaded under the IDE. Here we explicitly call application-will-finish-launching.
 (application-will-finish-launching self))

(let ((*dev-menu-key* nil)
      (*ccl-menuitem* nil)
      (*app-menuitem* nil))

  (defun set-toggle-states ()
    ;; we need a way to set the state of the menu-toggle items in the DEV menu
    ;; after we load a new mainmenu from an application bundle. This will be 
    ;; called to do that.
    (when *ccl-menuitem*
      (#/setState: *ccl-menuitem* #$NSOffState))
    (when *app-menuitem*
      (#/setState: *app-menuitem* #$NSOnState)))

  (defun show-dev-menu ()
    (when *dev-menu-key*
      (add-to-main-menu *dev-menu-key*)))

  (defmethod application-will-finish-launching ((self lisp-doc-app-delegate))
    ;; Add the Dev menu to the existing set of menus in the menubar
    ;; But do so in such a way that it never goes away as CCL and app
    ;; menus are toggled on and off.
    (with-slots (dev-menu menu-key ccl-menuitem app-menuitem) self
      (when (not (eql dev-menu (%null-ptr)))
        ;; the Dev menu is a disembodied (i.e. not Main) menu so we need to create a
        ;;  menuitem and make it the submenu. Then add it to the saved menu hash table
        ;;  and finally add it to the currently displayed main menu at a specified location.
        (let ((new-mi (make-instance ns:ns-menu-item
                        :submenu dev-menu
                        :title (#/title dev-menu))))
          (save-mi-list-with-key (list new-mi) menu-key)
          (setf *ccl-menuitem* ccl-menuitem)
          (setf *app-menuitem* app-menuitem)
          (setf *dev-menu-key* menu-key)
          (show-dev-menu)))))
)

(objc:defmethod (#/toggleCCLMenus: :void)
                ((self lisp-doc-app-delegate) sender)
  (if (eql #$NSOnState (#/state sender))
    (progn
      (remove-from-main-menu (starting-menu))
      (#/setState: sender #$NSOffState))
    (progn
      (add-to-main-menu (starting-menu) 0)
      (#/setState: sender #$NSOnState))))

(objc:defmethod (#/toggleAppMenus: :void) 
                ((self lisp-doc-app-delegate) (sender :id))
  (if (eql #$NSOnState (#/state sender))
    (progn
      (remove-from-main-menu (app-menu))
      (#/setState: sender #$NSOffState))
    (progn
      (add-to-main-menu (app-menu) "Dev")
      (#/setState: sender #$NSOnState))))

(objc:defmethod (#/validateMenuItem: #>BOOL) 
                ((self lisp-doc-app-delegate) (item :id))
  (let* ((action (#/action item)))
    (if (eql action (ccl::@selector "toggleAppMenus:"))
      (if (app-menu)
        #$YES
        #$NO)
      (call-next-method item))))

(defun make-dev-app (app-object)
  ;; This is called when the lisp-app-doc bundle is loaded by the lisp-doc-controller object created when
  ;; install-lisp-app-tools is called. The info.plist in that bundle contains "ad::make-dev-app" as the 
  ;; value of the CLMainFunc key. The app-object will be the lisp-doc-controller instance. For stand-alone
  ;; apps the app-object argument would be the ns-application object for the app.
  (let* ((mi1 (make-instance ns:ns-menu-item
                :title "New Bundle"
                :action "initBundle:"
                :key-equivalent "b"
                :key-equivalent-modifier-mask :option))
         (mi2 (make-instance ns:ns-menu-item
                :title "Initialize Bundle"
                :action "reinitBundle:"
                :key-equivalent "i"
                :key-equivalent-modifier-mask :option))
         (mi3 (make-instance ns:ns-menu-item
                :title "Use Bundle…"
                :action "useBundle:"
                :key-equivalent "u"
                :key-equivalent-modifier-mask :option))
         (mi4 (make-instance ns:ns-menu-item
                :title "Edit Info.plist…"
                :action "editPlist:"
                :key-equivalent "p"
                :key-equivalent-modifier-mask :option))
         (mi5 (make-instance ns:ns-menu-item
                :title "Install Executable"
                :action "installExec:"
                :key-equivalent "e"
                :key-equivalent-modifier-mask :option))
         (mi6 (menu-item-for-key :sep))
         (mi7 (make-instance ns:ns-menu-item
                :title "Load App Under IDE"
                :action "loadLispApp:"
                :key-equivalent "l"
                :key-equivalent-modifier-mask :option))
         (mi8 (make-instance ns:ns-menu-item
                :title "Unload App from IDE"
                :action "unloadLispApp:"
                :key-equivalent "L"
                :key-equivalent-modifier-mask :option))
         (mi9 (make-instance ns:ns-menu-item
                :title "Run App Stand-Alone"
                :action "runStandAlone:"
                :key-equivalent "r"
                :key-equivalent-modifier-mask :option))
         (mi10 (menu-item-for-key :sep))
         (mi11 (make-instance ns:ns-menu-item
                 :title "CCL Menus"
                 :action "toggleCCLMenus:"
                 :key-equivalent "c"
                 :state #$NSOnState
                 :key-equivalent-modifier-mask :option))
         (mi12 (make-instance ns:ns-menu-item
                 :title "App Menus"
                 :action "toggleAppMenus:"
                 :state #$NSOffState
                 :key-equivalent "a"
                 :key-equivalent-modifier-mask :option))
         (dev-menu (make-instance ns:ns-menu
                     :title "Dev"
                     :menu-items (list mi1 mi2 mi3 mi4 mi5 mi6 mi7 mi8 mi9 mi10 mi11 mi12)))
         (del (make-instance 'lisp-doc-app-delegate
                :dev-menu dev-menu
                :ccl-menuitem mi11
                :app-menuitem mi12)))
    ;; Menu toggling is a function of the lisp-doc-controller delegate (lisp-doc-app-delegate instance)
    ;; so set the target for those menu items to the delegate.
    (#/setTarget: mi11 del)
    (#/setTarget: mi12 del)

    (#/setDelegate: app-object del)
    (list dev-menu del)))

(provide :lisp-app-doc)

