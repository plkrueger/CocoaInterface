;; lisp-bundle.lisp


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

;; Defines code to manipulate dynamically loadable bundles in CCL
;; The idea is to progressively refine a bundle from something that can be loaded under the IDE
;; to something that can run standalone (i.e. a bundle with a .app extension).
;; If we do everything correctly we may be able to run an app either stand-alone or under the IDE
;; using a .app bundle defined using these methods.
;; Normal bundles are loaded once into an application, so even if you change things like the Info.plist
;; in a bundle, you can't retrieve new values using normal bundle functions like #/infoDictionary, so
;; we redefine that function to go retrieve the actual dictionary from the bundle each time it is requested.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes)
  (require :builder-utilities)
  (require :ns-object-utils)
  (require :ns-string-utils)
  (require :alert))

(in-package :iu)

(defconstant *lisp-type* (ccl::%make-nsstring (subseq (namestring ccl::*.lisp-pathname*) 1)))

(defconstant *fasl-type* (ccl::%make-nsstring (subseq (namestring ccl::*.fasl-pathname*) 1)))

(defvar *lisp-bundles* nil)

(defvar *loaded-lisp-classes* (make-hash-table))

;; Utility macros/functions

(defun bundle-for-class (cls)
  (or (gethash cls *loaded-lisp-classes*)
      (obj-if-not-null (#/bundleForClass: ns:ns-bundle cls))
      (#/mainBundle ns:ns-bundle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-bundle class

#|
(defclass lisp-bundle (ns:ns-bundle)
  ((bundle-loaded :accessor bundle-loaded :initform nil)
   (lisp-path :accessor lisp-path :initform nil)
   (bundle-logical-host :accessor bundle-logical-host :initform nil))
  (:metaclass ns:+ns-object))
|#

(defun lisp-bundle-with-path (path)
  (let* ((bpath (namestring (truename path)))
         (bundle (#/initWithPath: (#/alloc (find-class 'lisp-bundle))
                                  (lisp-to-temp-nsstring bpath))))
    (setf (lisp-path bundle) bpath)
    (pushnew bundle *lisp-bundles*)
    ;; set up a logical-pathname for the bundle and a translation that permits access 
    ;; to its contents (e.g. use of load, require, or probe-file) 
    (let* ((full-pname (first (last (pathname-directory bpath))))
           (dot-pos (position #\. full-pname :test #'char=))
           (pname (if dot-pos (subseq full-pname 0 dot-pos) full-pname))
           (search-path (concatenate 'string pname ":**;")))
      (setf (bundle-logical-host bundle) (logical-pathname (concatenate 'string pname ":")))
      (pushnew (logical-pathname search-path) *module-search-path* :test #'equal)
      (setf (logical-pathname-translations pname)
            (list (list "**;*.*"
                        (concatenate 'string bpath "/**/*.*")))))
    bundle))

(objc:defmethod (#/classNamed: #>Class)
                ((self lisp-bundle) (cl-name :id))
  ;; Find the class for any class provided by the bundle
  (let ((cl-obj (#_NSClassFromString cl-name)))
    (if (eq (bundle-for-class cl-obj) self)
      cl-obj
      (%null-ptr))))

(objc:defmethod (#/infoDictionary :id)
                ((self lisp-bundle))
  ;; go read the info.plist from the actual bundle instead of using a cached copy
  (#/dictionaryWithContentsOfFile: ns:ns-mutable-dictionary 
                                   (lisp-to-temp-nsstring 
                                    (namestring (ccl::path (lisp-path self) "Contents" "Info.plist")))))

(objc:defmethod (#/pathForResource:ofType: :id)
                ((self lisp-bundle) (res :id) (type :id))
  ;; This is used when apps are loaded under the CCL IDE and modified dynamically. Instead of 
  ;; using cached information about the bundle as the normal version of this method does, it
  ;; will look into the bundle to find the result (unless the default version gives us something).
  ;; This differs from the default method in that it does not search first in language independent
  ;; directory and later in language-specific. This could be modified relatively easily to do that
  ;; if it becomes important for someone. It also will recursively search all subdirectories in
  ;; the bundle which the default method does not do.
  (let ((def-path (call-next-method res type)))
    (if (and (eql def-path (%null-ptr)) (bundle-logical-host self))
      (let* ((ext (ns-to-lisp-string type))
             (res-str (ns-to-lisp-string res))
             (path (first (directory (merge-pathnames (concatenate 'string
                                                                   res-str 
                                                                   (if (non-empty-string ext)
                                                                     (concatenate 'string "." ext)
                                                                     ""))
                                                      (concatenate 'string
                                                                   (namestring (bundle-logical-host self)) 
                                                                   "**;"))
                                     :directories t
                                     :files t))))
        (if path
          (lisp-to-temp-nsstring (namestring path))
          (%null-ptr)))
      def-path)))

(objc:defmethod (#/load #>BOOL)
                ((self lisp-bundle))
  ;; Load whatever name was specified by the "LoadName" key in the info.plist in the bundle
  ;; It can then load whatever other files it wants.
  ;; When the bundle was created we set up a logicial translation for the logical host: 
  ;; _<bundle>_ where <bundle> is the name of the bundle file. We also set up a module search
  ;; path for that host. So to load "x.lisp" from a bundle named "MyBundle.bundle" a developer
  ;; could include the form (load "_MyBundle_:x.lisp"). Or to require that file the form
  ;; (require :x) or (require "x") would be sufficient since require will search directories 
  ;; specified in *module-search-path*.
  ;; Load for normal bundles only happens once; subsequent calls do nothing. This load,
  ;; on the other hand, is intended for use in an environment where the bundle may be 
  ;; modified dynamically. Therefore we go ahead and reload everything, including the
  ;; Info.plist (aka infoDictionary).
  (let* ((idict (#/infoDictionary self))
         (class-name (#/objectForKey: idict #@"NSPrincipalClass"))
         (classes (#/objectForKey: idict #@"CLProvidedClasses"))
         (load-module (#/objectForKey: idict #@"CLLoadName"))
         (loaded-classes (if (eql classes (%null-ptr))
                           (and (not (eql class-name (%null-ptr)))
                                (list class-name))
                           (ns-to-lisp-list classes :element-class ns:ns-object)))
         (dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary 2))
         (arr (lisp-to-ns-array loaded-classes)))
    (unless (eql load-module (%null-ptr))
      (with-errors-alerted
          (require (ns-to-lisp-string load-module))))
    (#/setValue:forKey: dict arr #$NSLoadedClasses)
    (dolist (cl loaded-classes)
      (let ((cls (#_NSClassFromString cl)))
        (setf (gethash cls *loaded-lisp-classes*) self)
        (#/setValue:forKey: dict cls cl)))
    (#/postNotificationName:object:userInfo: (#/defaultCenter ns:ns-notification-center)
                                             #$NSBundleDidLoadNotification
                                             self
                                             dict))
  (setf (bundle-loaded self) t)
  #$YES)

(defmethod load-bundle ((self lisp-bundle))
  (#/load self))

(objc:defmethod (#/loadAndReturnError: #>BOOL)
                ((self lisp-bundle) (err (:* :id)))
  (declare (ignore err))
  (#/load self))

(objc:defmethod (#/isLoaded #>BOOL)
                ((self lisp-bundle))
  (if (bundle-loaded self)
    #$YES
    #$NO))

 