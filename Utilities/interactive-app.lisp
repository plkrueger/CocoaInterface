;; interactive-app.lisp
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
  (require :ns-string-utils))

(in-package :iu)

(defun process-close (proc)
  (when (eq (external-process-status proc) :running)
    (signal-external-process proc 9)))

(defun shell-command (command &optional (shell "tcsh"))
  (let ((in-strm (make-string-input-stream (format nil "~a~c" command #\newline))))
    (with-output-to-string (out-str)
      (run-program shell nil :wait t :input in-strm :output out-str))))

#|
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
|#

(defmethod initialize-instance :after ((self app-stream) &key app args &allow-other-keys)
  (with-slots (app-stream-proc app-stream-app input-stream output-stream) self
    (setf app-stream-proc
          (run-program app args :wait nil :input :stream :output :stream))
    (setf input-stream
          (external-process-output-stream app-stream-proc))
    (setf output-stream
          (external-process-input-stream app-stream-proc))))

(defmethod start-trace ((self app-stream))
  (setf (app-stream-trace-lines self) nil)
  (setf (app-stream-trace self) t))

(defmethod stop-trace ((self app-stream))
  (setf (app-stream-trace self) nil))

(defmethod trace-output ((self app-stream))
  (format t "~{~%~a~%~}" (reverse (app-stream-trace-lines self))))
        
(defmethod write-line-to-app ((self app-stream) line)
  (when (app-stream-trace self)
    (push line (app-stream-trace-lines self)))
  (write-line line self)
  (force-output self))

(defmethod print-from-app ((self app-stream))
  (do ((next-char (read-char-no-hang self nil :eof)
                  (read-char-no-hang self nil :eof)))
      ((or (eq next-char nil) (eq next-char :eof)) next-char)
    (write-char next-char t)))

(defmethod close :after ((self app-stream) &key &allow-other-keys)
  (process-close (app-stream-proc self)))

(defun make-ccl-stream ()
  ;; Makes an app-stream opened to the correct ccl executable
  (let ((exec-path (namestring (truename (merge-pathnames (ccl::standard-kernel-name) "ccl:")))))
    (when exec-path
      (let ((strm (make-instance 'app-stream :app exec-path)))
        (values strm (ccl-result strm))))))

(defmacro with-ccl-stream ((stream-name) &body forms)
  `(let ((,stream-name (make-ccl-stream)))
     (unwind-protect
         (progn ,@forms)
       (close ,stream-name))))

(defmethod pop-error ((self app-stream))
  (write-line-to-app self ":pop")
  (ccl-result self))

(defmethod ccl-result ((self app-stream) &optional (timeout 20))
  ;; Assumes that you've done make-ccl-stream or equivalent
  ;; Returns everything printed prior to "#/newline?" as one big string
  ;; If an error occurred, returns a string with error and :pops the error
  ;; in the remote CCL. Two values are returned. The first is the result
  ;; as just described and the second is one of: :eof, :timeout, :normal,
  ;; or :error to indicate what sort of result is contained in the first
  ;; value.
  (do* ((fifth-prev-char #\space
                         (if next-char fourth-prev-char fifth-prev-char))
        (fourth-prev-char #\space
                          (if next-char third-prev-char fourth-prev-char))
        (third-prev-char #\space
                         (if next-char prev-char third-prev-char))
        (prev-char #\space
                   (if next-char next-char prev-char))
        (next-char (read-char-no-hang self nil :eof)
                   (read-char-no-hang self nil :eof))
        (loop-time (get-universal-time)
                   (get-universal-time))
        (last-char-time loop-time
                        (if next-char loop-time last-char-time))
        (time-since-last-char (- loop-time last-char-time)
                              (- loop-time last-char-time))
        (normal-complete (and (eq prev-char #\?) (eq third-prev-char #\newline) (eq next-char #\space))
                         (and (eq prev-char #\?) (eq third-prev-char #\newline) (eq next-char #\space)))
        (error-complete (and (eq prev-char #\>) (digit-char-p fourth-prev-char)
                             (eq fifth-prev-char #\newline) (eq next-char #\space))
                        (and (eq prev-char #\>) (digit-char-p fourth-prev-char)
                             (eq fifth-prev-char #\newline) (eq next-char #\space)))
        (res (make-array '(1) :element-type 'standard-char :adjustable t :fill-pointer 0 )))
      ((or (> time-since-last-char timeout)
           (eq next-char :eof)
           normal-complete
           error-complete)
       (progn
         (when (app-stream-trace self)
           (push res (app-stream-trace-lines self)))
         (cond ((eq next-char :eof)
                (values (coerce res 'string)
                        :eof))
               ((> time-since-last-char timeout)
                (values (coerce res 'string)
                        :timeout))
               (normal-complete
                (values (coerce (subseq res 0 (- (length res) 2)) 'string)
                        :normal))
               (error-complete
                (pop-error self)
                (values (coerce (subseq res 0 (- (length res) 4)) 'string)
                        :error)))))
    (when next-char
      (setf last-char-time loop-time)
      (vector-push-extend next-char res))))

(defmacro in-subordinate-ccl ((app-strm) &body forms)
  ;; fix to work with multiple return values and unreadable values
  `(progn
     (write-line-to-app ,app-strm (format nil "(progn ~{~s~})" '(,@forms)))
     (multiple-value-bind (res res-type) (ccl-result ,app-strm)
       (if (eq res-type :normal)
         (if (search "#<" res)
           res
           (read-from-string res nil :eof))
         res))))

(defmacro eval-in-subordinate-ccl ((app-strm) string)
  ;; use this if the forms you want to execute remotely should not be read locally
  ;; as they would if you used in-subordinate-ccl. Things like #/<objc-func> might
  ;; fail becuase the reader macro for #/ has side-effects that need to be executed
  ;; remotely.
  `(progn
     (write-line-to-app ,app-strm ,string)
     (multiple-value-bind (res res-type) (ccl-result ,app-strm)
       (if (eq res-type :normal)
         (if (search "#<" res)
           res
           (read-from-string res nil :eof))
         res))))

(defmacro remote-let ((app-strm) var-init-forms &body forms)
  ;; This is intended to be used with an open ccl-stream.
  ;; Be aware that all variable and function definitions will have the 
  ;; same package qualification on the remote side that they would have in
  ;; the calling environment. Therefore you should either explicitly provide
  ;; whatever package you want in all referencence within your remote-let 
  ;; form or arrange to be in the correct package on the remote side before
  ;; making the remote-let call.
  ;; Initialization values for vars are determined in local environment
  ;; and results should be literal objects that can be read from
  ;; their printed representation (i.e acceptable to read-from-string).
  ;; Then the forms are executed in the remote environment of app-strm
  ;; with the variables initialized to those locally-determined values.
  ;; If the returned value is readable from a string, that value is 
  ;; returned. Otherwise a string with the printed representation of the
  ;; unreadable value is returned.
  (loop
    for (var val-form) in var-init-forms
    collect var into vars
    collect val-form into val-forms
    finally (return `(progn
                       (write-line-to-app ,app-strm 
                                          (format nil
                                                  "(let (~:{(~s ~s)~}) ~{~s~})"
                                                  (mapcar #'list
                                                          ',vars
                                                          (list ,@val-forms))
                                                  ',forms))
                       (let ((res (ccl-result ,app-strm)))
                         (if (search "#<" res)
                           res
                           (read-from-string res nil :eof)))))))

#|  Tests

(defun interactive-shell ()
  ;; Returns a two-way stream that can be used to write and read shell commands.
  ;; User must remember to supply #\newlines at the end of each line written and
  ;; also call (force-output <strm>).
  (let* ((proc (run-program *shell* nil :wait nil :input :stream :output :stream))
         (in-str (and proc (external-process-output-stream proc)))
         (out-str (and proc (external-process-input-stream proc))))
    (values (and in-str out-str
                 (make-two-way-stream in-str out-str))
            proc)))

(defun basic-test ()
  (let ((ls-process (run-program "/bin/ls" '() 
                                 :wait nil 
                                 :output :stream))) 
    (unwind-protect 
        (with-open-stream (s (external-process-output-stream ls-process))
          (do ((line (read-line s nil :eof)
                     (read-line s nil :eof)))
              ((eq line :eof))
            (format t "~%~a" line)))
      (process-close ls-process))))

(defun piping-test () 
  ;; derived from code found at 
  ;; http://stackoverflow.com/questions/2353353/how-to-process-input-and-output-streams-in-steel-bank-common-lisp
  (let ((ls-process (run-program "/bin/ls" '() 
                                 :wait nil 
                                 :output :stream))) 
    (unwind-protect 
        (with-open-stream (s (external-process-output-stream ls-process)) 
          (let ((grep-process (run-program "/usr/bin/grep" '("System")
                                          :input s 
                                          :output :stream))) 
            (when grep-process 
              (unwind-protect 
                  (with-open-stream (o (external-process-output-stream grep-process)) 
                    (loop 
                       :for line := (read-line o nil nil) 
                       :while line 
                       :collect line))
                (when grep-process (process-close grep-process))))))
      (when ls-process (process-close ls-process)))))

|#

(provide :interactive-app)