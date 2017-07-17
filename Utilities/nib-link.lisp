;; nib-link.lisp


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

;; This provides a facility that creates a symbolic link from an appropriate point within the CCL applications 
;; application bundle to a specified .nib file in some other directory. This permits the use of more standard
;; ways to load nib files even when the nib file isn't actually located within the bundle. The link remains
;; for the duration of the nib-link object and is then removed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes))

(in-package :iu)

(defvar *shell* "tcsh")

(defvar *nib-links* nil)

#|
(defclass nib-link ()
  ((nib-path :accessor nib-path :initarg :path)
   (bundle-path :accessor bundle-path :initform nil))
  (:default-initargs
      :path nil))
|#

(defmethod initialize-instance :after ((self nib-link)
                                       &key &allow-other-keys)
  (ccl:terminate-when-unreachable self)
  (with-slots (nib-path bundle-path) self
    (let ((path-name (namestring (truename nib-path))))
      ;; since nibs are actually directories, truename will end with "/" which we 
      ;; need to strip off to make the link and unlink work correctly
      (setf nib-path (subseq path-name 0 (1- (length path-name)))))
    (when (probe-file nib-path)
      (setf bundle-path (truename
                         (concatenate 'string
                                      ccl::*cocoa-application-path*
                                      "Contents;Resources;English.lproj"))))
    (when (and bundle-path (probe-file bundle-path))
      ;; add a symbolic link to the nib file from the application bundle
      (let* ((str (concatenate 'string
                               "cd \""
                               (namestring (truename bundle-path))
                               "\""
                               (string #\newline)
                               "ln -s \""
                               nib-path
                               "\" ."
                               (string #\newline)))
             (in-strm (make-string-input-stream str)))
        (with-output-to-string (out-str)
          (run-program *shell* nil :wait t :input in-strm :output nil))
        (push self *nib-links*)))))

(defmethod unlink ((self nib-link))
  (with-slots (nib-path bundle-path) self
    (when (and bundle-path (probe-file bundle-path))
      ;; remove the symbolic link to the nib file
      (let* ((str (concatenate 'string
                               "cd \""
                               (namestring (truename bundle-path))
                               "\""
                               (string #\newline)
                               "rm \""
                               (pathname-name nib-path)
                               ".nib\""
                               (string #\newline)))
             (in-strm (make-string-input-stream str)))
        (with-output-to-string (out-str)
          (run-program *shell* nil :wait t :input in-strm :output nil)))
      (setf *nib-links* (delete self *nib-links*)))))

(defmethod ccl:terminate ((self nib-link))
  (unlink self))

(defmethod is-link-to ((self nib-link) nib-path)
  (let ((path-name (namestring (truename nib-path))))
    ;; since nibs are actually directories, truename will end with "/" which we 
    ;; need to strip off to make the link and unlink work correctly
    (string= (nib-path self) (subseq path-name 0 (1- (length path-name))))))

(defun find-link-to (nib-path)
  (find-if #'(lambda(nl)
               (is-link-to nl nib-path))
           *nib-links*))

(defun find-nib (nib-name)
  (probe-file (truename
               (concatenate 'string
                            ccl::*cocoa-application-path*
                            "Contents;Resources;English.lproj;"
                            nib-name))))

(provide :nib-link)
        