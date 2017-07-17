;; lisp-notifications.lisp

#|
The MIT license.

Copyright (c) 2016 Paul L. Krueger

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

This implements a lisp equivalent to Apple's notification manager. It allows a lisp object to post
event notifications that might be of interest to other lisp objects. Notification identifiers can be any
symbol or case-insensitive string. A notification poster may optionally include additional information
related to the event. A callback function registered by an observer must have a function signature in
the form: (receiver poster notification-id notification-info) where notification-info is optional. 
It must be included if and only if the notifier provides additional notification-info. The funtion
should not take a third argument if the notifier never includes extra information. The notification-info
argument can also be optional if the poster sometimes, but not always, provides extra information. An
observer may register for any number of different notifications from the same sender to be handled by
the same function with a single add-observer call.

If you need to interact with Apple's event notification system, see notifications.lisp. 

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes))

(in-package :iu)

;; the use of :weaks in the make-instance call for the underlying 'assoc-array
;; means that if an observed object is gc'able (except for a reference to it in
;; this assoc-array), then whenever a gc occurs, that entry will be removed and
;; then the observed object can be gc'ed.
;; We get a comparable effect for the observor by using an alist form of population
;; object to contain the (observor.function) pairs. See the CCL documentation for
;; the use of population objects to contain weak references.
;; A given observer can register only once for a given notification from a particular
;; observed object. A second registration will replace the function previously 
;; registered with a new one.
(let ((notify-hash (make-instance 'assoc-array
                     :default-value nil 
                     :tests (list #'eq #'equalp)
                     :weaks (list :key nil))))

  (defun notify-ht ()
    notify-hash)

  (defmethod add-observer (observer (func function) notif observed)
    (dolist (n (if (consp notif) notif (list notif)))
      (dolist (obs (if (consp observed) observed (list observed)))
        (let* ((pop (or (assoc-aref notify-hash obs n)
                        (setf (assoc-aref notify-hash obs n)
                              (make-population :type :alist))))
               (current (assoc observer (population-contents pop))))
          (if current
              (setf (cdr current) func)
              (push (cons observer func) (population-contents pop)))))))

  (defmethod remove-observer (observer notif observed)
    (dolist (n (if (consp notif) notif (list notif)))
      (dolist (obs (if (consp observed) observed (list observed)))
        (let* ((pop (assoc-aref notify-hash obs n))
               (contents (and pop (population-contents pop))))
          (when contents
            (setf (population-contents pop)
                  (delete (assoc observer contents) contents)))))))

  (defmethod post-notification (poster (notif string) &optional (user-info nil))
    (let* ((pop (assoc-aref notify-hash poster notif))
           (observer-alist (and pop (population-contents pop))))
      (dolist (obs observer-alist)
          (let ((obs (car obs))
                (func (cdr obs)))
            (apply func obs poster notif user-info))))))

(provide :lisp-notification)
           