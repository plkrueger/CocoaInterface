;; window-controller.lisp

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

#|

Implements the lisp-window-controller class which inherits from NSWindowController. It always creates
a new window (or multiple windows) using a method (or methods) provided when the controller is initialized.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes)
  (require :ns-string-utils)
  (require :ns-object-utils)
  (require :nslog-utils))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-window-controller
;;
;; A NSWindowController subclass that controls windows created dynamically in Lisp
;;
;; The :build-method should take one argument which is a lisp-window-controller for
;; the window it creates. That method should return two values: the window created
;; and a list of instantiated objects that the lisp-window-controller will own.
;; That list should contain both Objective-C and Lisp objects. The latter is necessary
;; to assure that a reference to things like lisp views or data objects that only
;; serve to support window functionality always have a reference from another lisp
;; object and are therefore not reclaimed by garbage collection. 
;; Objective-C objects should already have been #/retained by the window build method.
;; When the window is closed they will be released by the lisp-window-controller.

#|
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
|#

(objc:defmethod (#/dealloc :void)
                ((self lisp-window-controller))
  (on-main-thread
   #|(let ((win (window self)))
     (when (and (is-loaded self)
                (%null-ptr-p (#/document self))
                (not (%null-ptr-p win)))
       (ns-log-format "Closing window ~s because the window-controller is being dealloc'ed" win)
       (#/close win)))|#
   (dolist (obj (instantiated-objects self))
     (when (and (typep obj 'ns:ns-object) (not (eql obj (%null-ptr))))
       (#/release obj)))
   (setf (instantiated-objects self) nil)
   (call-next-method)
   (objc:remove-lisp-slots self)))
  
(defmethod load-window ((self lisp-window-controller))
  (#/loadWindow self))

(defmethod show-window ((self lisp-window-controller))
 (on-main-thread 
   (#/showWindow: self (%null-ptr))))

(defmethod close-window ((self lisp-window-controller))
 (on-main-thread
  (#/performClose: (window self) self)))

(objc:defmethod (#/loadWindow :void) 
                ((self lisp-window-controller))
  ;; Initializes the controller's window using the window-build-connect-method
  (on-main-thread
   (multiple-value-bind (window instantiated-objs) 
                        (funcall (window-build-connect-method self) self)
     (when (typep window 'ns:ns-window)
       ;; need to check whether to release first because setting the windoow-controller for the
       ;; window causes this to be set to nil
       (#/setWindow: self window)
       ;; For reasons mysterious to me, calling #/window on a window controller increases the
       ;; window's reference count. To avoid worrying about that, we'll save the window pointer
       ;; and use that instead.
       (setf (window self) window)
       ;; all instantiated objects are now owned by this controller 
       ;; Objective-C objects should have a ref count of 1 already since they will be
       ;; released by this controller when it goes away. Lisp objects are kept here too so
       ;; they won't be gc'ed until the window is closed.
       (setf (instantiated-objects self) instantiated-objs)
       (awake-objects-from-nib (list self))
       (awake-objects-from-nib instantiated-objs)
       (awake-objects-from-nib (list window))
       (setf (is-loaded self) t)))))

(defmethod awake-objects-from-nib ((obj-list list))
  (dolist (obj obj-list)
    (when (and (typep obj 'ns:ns-object)
               (#/respondsToSelector: obj (ccl::@selector "awakeFromNib")))
      (#/awakeFromNib obj))))

(defmethod update-window ((self lisp-window-controller))
  ;; Most things in windows will update automatically because they have values bound to KVO compliant
  ;; objects. But sometimes when the document being displayed is modified by changing objects internally
  ;; you need to nudge a lisp-controller to reload itself. When called, this method will find all
  ;; lisp-controller objects among its instantiated objects and tell them something has changed.
  (dolist (lc (remove-if-not #'(lambda (obj)
                                 (typep obj 'lc::lisp-controller))
                             (instantiated-objects self)))
    (lc::lisp-controller-changed lc)))

(objc:defmethod (#/isWindowLoaded #>BOOL)
                ((self lisp-window-controller))
  (is-loaded self))
  
(provide :lisp-window-controller)

