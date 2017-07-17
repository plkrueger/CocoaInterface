;; notification.lisp

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

;; This is just a simple lisp layer on top of NSNotifications

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize)
  (require :selector-utils))

(in-package :iu)

(defmethod received-notification (target notif-name notif-obj notif-info)
  (declare (ignore notif-info))
  ;; default method that just logs that a notification was received
  (ns-log-format "~s received ~a from ~s" target notif-name notif-obj))

#|
(defclass notification-handler (ns:ns-object)
  ((notify-func :accessor notify-func
                :initarg :func)
   (notify-target :accessor notify-target
                  :initarg :target))
  (:default-initargs
    :func nil
    :target nil)
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self notification-handler)
                                       &key
                                       (from nil)
                                       (notifications nil)
                                       (func nil)
                                       &allow-other-keys)
  (unless func
    (setf (notify-func self) #'received-notification))
  (when from
    (dolist (notif (if (listp notifications) notifications (list notifications)))
      (#/addObserver:selector:name:object: 
       (#/defaultCenter ns:ns-notification-center)
       self
       (ccl::@selector "lispHandleNotification:")
       (if (typep notif 'ns:ns-object)
         notif
         (eval (read-from-string (concatenate 'string "#$" (string notif)))))
       from))))

(objc:defmethod (#/dealloc :void)
                ((self notification-handler))
  (#/removeObserver: (#/defaultCenter ns:ns-notification-center) self)
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc:defmethod (#/lispHandleNotification: :void) 
                ((self notification-handler)
                 (notif :id))
  (funcall (notify-func self)
           (notify-target self)
           (coerce-obj (#/name notif) 'string)
           (#/object notif)
           (coerce-obj (#/userInfo notif) t)))
           

(provide :notification)