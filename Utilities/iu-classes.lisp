;; iu-classes.lisp

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

;; Defines all the classes used within the interface-utilities (iu) package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :iu)

;; A stream to a separate application
(defclass app-stream (two-way-stream)
  ((app-stream-proc :accessor app-stream-proc)
   (app-stream-app :accessor app-stream-app :initarg :app)
   (app-stream-trace :accessor app-stream-trace :initarg :trace)
   (app-stream-trace-lines :accessor app-stream-trace-lines :initform nil)
   (app-stream-args :accessor app-stream-args :initarg :args))
  (:default-initargs
    :app "tcsh"
    :args nil
    :trace nil))

;; A class that implements an n-dimensional associative (sparse) array
(defclass assoc-array ()
  ((arr-rank :accessor arr-rank :initarg :rank)
   (index1-ht :accessor index1-ht)
   (index-tests :accessor index-tests :initarg :tests)
   (index-weaks :accessor index-weaks :initarg :weaks)
   (default-value :accessor default-value :initarg :default)
   (default-form :accessor default-form :initarg :initform))
  (:default-initargs
    :rank 2
    :tests nil
    :weaks nil
    :default nil
    :initform nil))

;; lisp version of an NSAttributedString
(defclass attributed-string (fundamental-character-output-stream)
  ((att-ns-str :accessor att-ns-str)
   (ns-mut-str :accessor ns-mut-str)
   (as-view :accessor as-view
            :initarg :view)
   (format-attributes :accessor format-attributes
                      :initform nil))
  (:default-initargs
      :view nil))

;; Serves as a source and delegate for pop-up menus
(defclass combo-box-source (ns:ns-object)
  ((source-items :accessor source-items
                 :initform nil))
  (:metaclass ns:+ns-object))

;; Info about a file being watched in a separate thread
(defclass file-monitor ()
  ((file :accessor file :initarg :file)
   (notify-func :accessor notify-func :initarg :notify-func)
   (last-change-date :accessor last-change-date :initform nil)
   (suspended :accessor suspended :initform nil)))

;; Observes some KVO compliant slot
(defclass kvo-observer (ns:ns-object)
  ((observed-instance :accessor observed-instance
                      :initarg :obj)
   (observe-path :accessor observe-path
                 :initarg :path)
   (when-observed-func :accessor when-observed-func
                       :initarg :func)
   (convert-type :accessor convert-type
                 :initarg :convert-type))
  (:default-initargs
    :obj nil
    :path nil
    :func nil
    :convert-type nil)
  (:metaclass ns:+ns-object))

;; NSBundle interface for Lisp
(defclass lisp-bundle (ns:ns-bundle)
  ((bundle-loaded :accessor bundle-loaded :initform nil)
   (lisp-path :accessor lisp-path :initform nil)
   (bundle-logical-host :accessor bundle-logical-host :initform nil))
  (:metaclass ns:+ns-object))

;; This class does some of the same things that the shared NSDocumentController instance
;; does for stand-alone application programs.
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
   ;;(ldc-open-pnl :accessor ldc-open-pnl
    ;;             :initform nil)
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

;; Lisp subclass of NSDocument
(defclass lisp-document (ns:ns-document)
  ()
  (:metaclass ns:+ns-object))

;; A class defining objects that can be targets of menuitem actions and which
;; then invoke some lisp function
(defclass lisp-menu-target (ns:ns-object)
  ((lisp-func :accessor lisp-func :initarg :lisp-func))
  (:metaclass ns:+ns-object))

;; A class that encapsulates references to other instances so that we can have circular
;; references.
(defclass lisp-object-reference (ns:ns-object)
  ((obj-dict :accessor obj-dict :initarg :obj-dict))
  (:default-initargs :obj-dict nil)
  (:metaclass ns:+ns-object))

;; An objective-C class that can be used as a proxy for any lisp object when binding
(defclass lisp-ptr-wrapper (ns:ns-object)
  ((lpw-lisp-ptr :accessor lpw-lisp-ptr)
   (lpw-controller :accessor lpw-controller)
   (lpw-depth :accessor lpw-depth)
   (lpw-parent :accessor lpw-parent))
  (:metaclass ns:+ns-object))

;; A NSWindowController subclass that controls windows created dynamically in Lisp
(defclass lisp-window-controller (ns:ns-window-controller)
  ((window :accessor window
           :initform nil)
   (window-build-connect-method :accessor window-build-connect-method
                                :initarg :build-method)
   (instantiated-objects :accessor instantiated-objects
                         :initform nil)
   (is-loaded :accessor is-loaded
              :initform nil))
  (:default-initargs
    :build-method nil)
  (:metaclass ns:+ns-object))

;; Used to create symbolic links from inside packages to nib files outside
(defclass nib-link ()
  ((nib-path :accessor nib-path :initarg :path)
   (bundle-path :accessor bundle-path :initform nil))
  (:default-initargs
      :path nil))

;; class used to handle notifications
(defclass notification-handler (ns:ns-object)
  ((notify-func :accessor notify-func
                :initarg :func)
   (notify-target :accessor notify-target
                  :initarg :target))
  (:default-initargs
    :func nil
    :target nil)
  (:metaclass ns:+ns-object))

;; class used to encode lisp functions
(defclass ns-func (ns:ns-object)
  ((func :accessor func :initarg :func)
   (ns-str :accessor ns-str)
   (func-name :accessor func-name)
   (func-package :accessor func-package))
  (:metaclass ns:+ns-object))

;; Class used to encode an arbitrary lisp object
(defclass ns-misc (ns:ns-object)
  ((obj :accessor obj :initarg :obj)
   (obj-type :accessor obj-type)
   (obj-str :accessor obj-str)
   (ns-str :accessor ns-str))
  (:metaclass ns:+ns-object))

;; Class used to encode lisp symbols
(defclass ns-sym (ns:ns-object)
  ((sym :accessor sym :initarg :sym)
   (ns-str :accessor ns-str)
   (sym-name :accessor sym-name)
   (sym-package :accessor sym-package))
  (:metaclass ns:+ns-object))

;; Thread-safe queue
;; Class defintions for ts-queue, and qnode are currently in ts-queue.lisp 

;; or-semaphore
;; Class definition for or-semaphore currently in or-semaphore.lisp

(provide :iu-classes)