;; or-semaphore.lisp

#|
The MIT license.

Copyright (c) 2017 Paul L. Krueger

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
  (require :iu-classes))

(in-package :iu)

(defstatic *debug-or-semaphore* nil)

(defclass or-semaphore ()
  ((sub-sems :accessor sub-sems
             :initarg :sems)
   (sub-procs :accessor sub-procs
              :initform nil)
   (signalers :accessor signalers
              :initform nil)
   (signal-lock :accessor signal-lock
                :initform (make-lock))
   (or-sem :accessor or-sem
           :initform (make-semaphore))))

(defmethod push-signaler ((self or-semaphore))
  (let* ((pos (position *current-process* (sub-procs self)))
         (signaler-sem (if pos
                           ;; a sub-semaphore was signaled
                           (nth pos (sub-sems self))
                           ;; the or-semaphore was directly signaled 
                           (or-sem self))))                    
    (setf (signalers self)
          (nconc (signalers self)
               (list signaler-sem)))))

(defmethod lock-and-signal ((self or-semaphore))
  (when *debug-or-semaphore*
    (iu::ns-log-format "~s lock-and-signal ~s" *current-process* self))
  (with-lock-grabbed ((signal-lock self))
    (push-signaler self)
    (signal-semaphore (or-sem self))))

(defmethod lock-and-pop ((self or-semaphore))
  (when *debug-or-semaphore*
    (iu::ns-log-format "~s lock-and-pop ~s" *current-process* self))
  (with-lock-grabbed ((signal-lock self))
    (pop (signalers self))))

(defmethod wait-semaphore-proc ((or-sem or-semaphore) (wait-sem semaphore))
 (loop 
    do
    (when *debug-or-semaphore*
      (iu::ns-log-format "~s about to wait on ~s" *current-process* wait-sem))
    (wait-on-semaphore wait-sem)
    (when *debug-or-semaphore*
      (iu::ns-log-format "~s wait on ~s satisfied" *current-process* wait-sem))
    (lock-and-signal or-sem)))

(defmethod initialize-or-semaphore ((self or-semaphore))
  (with-slots (or-sem sub-sems sub-procs) self
    (dolist (sub-sem sub-sems)
      (setf sub-procs
            (nconc sub-procs
                   (list (process-run-function "semaphore waiter"
                           #'wait-semaphore-proc
                           self
                           sub-sem)))))))

(defmethod initialize-instance :after ((self or-semaphore)
                                       &key
                                       &allow-other-keys)
  (ccl:terminate-when-unreachable self)
  (initialize-or-semaphore self))

(defmethod ccl:terminate ((self or-semaphore))
  (stop-or-semaphore self))

(defmethod stop-or-semaphore ((self or-semaphore))
  ;; can be called when finished with an or-semaphore to keep it
  ;; from continuing to wait for the sub-semaphores which were
  ;; used to create it. To re-use it after calling this method
  ;; first call reinitialize-or-semaphore.
  (dolist (proc (sub-procs self))
    (process-abort proc))
  (setf (sub-procs self) nil))

(defmethod signal-or-semaphore ((self or-semaphore))
  (lock-and-signal self))

(defmethod wait-on-or-semaphore ((self or-semaphore) &rest args)
  ;; returns the semaphore that was signaled
  (apply #'wait-on-semaphore (or-sem self) args)
  (lock-and-pop self))

(defmethod timed-wait-on-or-semaphore ((self or-semaphore) &rest args)
  ;; returns the semaphore that was signaled or nil if timeout occurred
  (when (apply #'timed-wait-on-semaphore (or-sem self) args)
    (lock-and-pop self)))

(defmethod reinitialize-or-semaphore ((self or-semaphore))
  (dolist (proc (sub-procs self))
    (process-abort proc))
  (setf (or-sem self) (make-semaphore))
  (initialize-or-semaphore self))

;;;;;;;;;; Test methods
#|
(defstatic *or-sem-test-result* nil)
(defstatic *test-or-sem* nil)
(defglobal *sem-1* (make-semaphore))
(defglobal *sem-2* (make-semaphore))
(defglobal *sem-3* (make-semaphore))

(defun or-wait (sems)
  (let ((or-sem (make-instance 'or-semaphore :sems sems)))
    (setf *test-or-sem* or-sem)
    (setf *or-sem-test-result*
          (wait-on-or-semaphore or-sem))
    (setf *or-sem-test-result*
          (wait-on-or-semaphore or-sem))
    (setf *or-sem-test-result*
          (timed-wait-on-or-semaphore or-sem 15))
    (stop-or-semaphore or-sem)))

(defun or-test ()
  (setf *or-sem-test-result* nil)
  (process-run-function "or-semaphore wait"
    #'or-wait
    (list *sem-1* *sem-2* *sem-3*)))
|#
  
(provide :or-semaphore)