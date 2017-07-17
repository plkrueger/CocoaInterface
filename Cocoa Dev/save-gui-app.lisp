;; save-gui-app.lisp

#|
The MIT license.

Copyright (c) 2010-2013 Paul L. Krueger

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

(in-package "GUI")

(defun save-app (bundle-path app-name)
  ;; adapted from build-ide in start.lisp
  ;; some shortcuts taken because the legwork was already done in the calling Lisp process

  #-cocotron                          ;needs conditionalization
  (require :easygui)

  (ccl::maybe-map-objc-classes t)
  (let* ((missing ()))
    (ccl::do-interface-dirs (d)
      (ccl::cdb-enumerate-keys
       (ccl::db-objc-classes d)
       (lambda (name)
         (let* ((class (ccl::lookup-objc-class name nil)))
           (unless (ccl::objc-class-id  class) (push name missing))))))
    (when missing
      (break "ObjC classes ~{~&~a~} are declared but not defined." missing)))

  #-cocotron
  (ccl::touch bundle-path)
  (let ((kernel-file (make-pathname :name app-name 
                                    :type nil 
                                    :version nil 
                                    :defaults (merge-pathnames 
                                               #+darwin-target
					       ";Contents;MacOS;"
					       #+cocotron
					       ";Contents;Windows;"
                                               bundle-path)))
        (image-file (make-pathname :name app-name
                                   :type "image"
                                   :version nil
                                   :defaults (merge-pathnames
                                              ";Contents;Resources;ccl;"
                                              bundle-path))))
    (ensure-directories-exist image-file)
    (ccl:copy-file (ccl::kernel-path) kernel-file :if-exists :supersede 
                   :preserve-attributes t)
    (save-application image-file
                      #|
                      :toplevel-function (let ((tlf (read-from-string top-func nil nil)))
                                           (cond ((null tlf)
                                                  nil)
                                                 ((typep tlf 'function-name)
                                                  (and (fboundp tlf) (symbol-function tlf)))
                                                 (t
                                                  nil)))
                      |#
                      :application-class 'cocoa-ide
                      #+windows-target #+windows-target
                      :application-type :gui)))
