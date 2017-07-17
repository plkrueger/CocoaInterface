;; tagged-dates.lisp

;;; Sets things up so CCL recognized the new tagged form of NSDate instances as real class instances

(in-package :ccl)

(defun setup-class-info-for-tagged-instance (instance)
 ;; "instance" is a pointer known to be an ObjC instance that isn't recognized as an ObjC
 ;; object, presumably because its address has some of its low 4 bits set.
 (let* ((tag (tagged-objc-instance-p instance)))
   (when (and tag (not (objc-tagged-instance-class-index tag)))
     ;; We should only bother with this if tag isn't already on the alist
     (let* ((class (objc-message-send instance "class")))
       (unless (%null-ptr-p class)
        (install-foreign-objc-class class nil)
        (push (cons tag (objc-class-or-private-class-id class)) *tagged-instance-class-indices*)))
     ;; return a copy of the pointer that will be interpreted as an ns-object if this was successful
     (%inc-ptr instance 0))))

(provide :tagged-dates)