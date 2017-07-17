;; file-monitor.lisp

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


;; Set up a process in a thread that periodically checks for changes to a file and calls
;; a specified function when the file has been changed.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes))

(in-package :iu)

(defstatic *file-monitor-thread* nil)
(defstatic *file-monitor-lock* (make-lock))
(defstatic *file-monitor-wait-secs* 3)
(defstatic *file-monitor-files* nil)

#|
(defclass file-monitor ()
  ((file :accessor file :initarg :file)
   (notify-func :accessor notify-func :initarg :notify-func)
   (last-change-date :accessor last-change-date :initform nil)
   (suspended :accessor suspended :initform nil)))
|#

(defun monitor-file-changes ()
  (loop
    (sleep *file-monitor-wait-secs*)
    (with-lock-grabbed (*file-monitor-lock*)
      (dolist (fm *file-monitor-files*)
        (unless (suspended fm)
          (if (probe-file (file fm))
            (let ((last-changed (file-write-date (file fm))))
              (if (last-change-date fm)
                (when (> last-changed (last-change-date fm))
                  (setf (last-change-date fm) last-changed)
                  (on-main-thread
                   ;; log any error that might occur, but otherwise ignore it
                   (handler-case (funcall (notify-func fm) (file fm))
                     (condition (c)
                                (ns-log (format nil
                                                "Error calling: ~s, condition: ~s"
                                                (notify-func fm)
                                                c))))))
                (setf (last-change-date fm) last-changed)))
            ;; file disappeared on us, stop monitoring it
            (remove-monitored-file (file fm))))))))

(defun monitor-file (path notify-func &key (wait-secs 3) (single-monitor t))
  (when (< wait-secs *file-monitor-wait-secs*)
    (setf *file-monitor-wait-secs* wait-secs))
  (when (probe-file path)
    (with-lock-grabbed (*file-monitor-lock*)
      (let ((current-monitor (and single-monitor
                                  (find path *file-monitor-files* :key #'file :test #'equal))))
        (if current-monitor
          ;; only allow one monitor function per file
          (progn
            (setf (notify-func current-monitor) notify-func)
            ;; Update the last-change-date or we'll trigger an immediate call of the update
            ;; function. Since monitor-file was just called we likely just changed the file.
            (setf (last-change-date current-monitor)
                  (file-write-date (file current-monitor))))
          (setf *file-monitor-files* (cons (make-instance 'file-monitor
                                             :file path
                                             :notify-func notify-func)
                                           *file-monitor-files*)))))
    (if *file-monitor-thread*
      (when (string= (process-whostate *file-monitor-thread*) "Exhausted")
        (process-enable *file-monitor-thread*))
      (setf *file-monitor-thread*
            (process-run-function "file-monitor-thread"
                                  #'monitor-file-changes)))))

(defun remove-monitored-file (path)
  ;; *file-monitor-lock* already acquired by caller
  (setf *file-monitor-files* (delete path *file-monitor-files* :key #'file :test #'equal))
  (unless *file-monitor-files*
    (process-kill *file-monitor-thread*)))

(defun unmonitor-file (path)
  (when *file-monitor-files*
    (with-lock-grabbed (*file-monitor-lock*)
      (setf *file-monitor-files* (delete path *file-monitor-files* :key #'file :test #'equal))
      (remove-monitored-file path))))

(defun suspend-monitoring (path)
  ;; If path is being monitored, stop it temporarily
  (when *file-monitor-files*
    (with-lock-grabbed (*file-monitor-lock*)
      (let ((fm (find path *file-monitor-files* :key #'file :test #'equal)))
        (when fm
          (setf (suspended fm) t))))))

(defun resume-monitoring (path)
  ;; If path is being monitored, resume active reaction to changes
  (when *file-monitor-files*
    (with-lock-grabbed (*file-monitor-lock*)
      (let ((fm (find path *file-monitor-files* :key #'file :test #'equal)))
        (when fm
          (setf (last-change-date fm)
                (file-write-date path))
          (setf (suspended fm) nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test functions

#|

(require :open-panel)

(defun fn (file)
    (hi::with-output-to-listener 
        (format t "File ~s was changed~%" file)))

(defvar *test-file* (iu::open-panel))

(iu::monitor-file *test-file* #'fn)

(iu::unmonitor-file *test-file*)

|#

(provide :file-monitor)
  