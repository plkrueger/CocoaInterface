;; ns-string-utils.lisp
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
  (require :iu-classes))

(in-package :iu)

(defun ns-to-lisp-string (ns-str)
  (if (and (not (eql (%null-ptr) ns-str)) (plusp (#/length ns-str)))
    (ccl::%get-utf-8-cstring (#/UTF8String ns-str))
    ""))

(defun ns-attrib-to-lisp-string (att-str)
  (ns-to-lisp-string (#/string att-str)))

(defun non-empty-string (str)
  (and (stringp str) (not (zerop (length str))) str))

(defun lisp-str-to-ns-data (lisp-str)
  (with-cstrs ((str lisp-str))
    (#/dataWithBytes:length: ns:ns-data str (1+ (length lisp-str)))))

(defun ns-data-to-lisp-str (nsdata)
  (%get-cstring (#/bytes nsdata)))

(defun lisp-object-to-ns-data (obj)
  (lisp-str-to-ns-data (format nil "~s" obj)))

(defun ns-data-to-lisp-object (nsdata)
  (read-from-string (ns-data-to-lisp-str nsdata)))

(defun lisp-to-temp-nsstring (string)
  ;; creates a string that is not owned by caller
  ;; so no release is necessary
  (with-encoded-cstrs :utf-8 ((s string))
    (#/stringWithUTF8String: ns:ns-string s)))

(defun nsstring-to-class (ns-str)
  (let ((lisp-obj (read-from-string (ns-to-lisp-string ns-str) nil nil))
        (classes nil))
    (if (consp lisp-obj)
      (dolist (obj lisp-obj (nreverse classes))
        (push (find-class obj nil) classes))
      (find-class lisp-obj nil))))

(defun find-func (func-str)
  (let* ((sym (read-from-string func-str nil nil)))
    (cond ((and (typep sym 'function-name) (fboundp sym))
           (symbol-function sym))
          ((and (consp sym) (eq (first sym) 'function))
           (let ((fsym (second sym)))
             (and (typep fsym 'function-name) 
                  (fboundp fsym)
                  (symbol-function fsym)))))))

(defun nsstring-to-func (ns-str)
  (find-func (ns-to-lisp-string ns-str)))

(defun nsstring-to-sym (ns-str)
  (let ((sym (read-from-string (ns-to-lisp-string ns-str) nil nil)))
    (if (symbolp sym) sym nil)))

(defun read-from-nsstring (ns-str)
  (read-from-string (ns-to-lisp-string ns-str) nil nil))

(defun find-substring (substr str &key (test #'string=))
  (let* ((first-char (elt substr 0))
         (count (length substr))
         (search-end (1+ (- (length str) count))))
    (do* ((begin (position first-char str :end search-end)
                 (position first-char str :start (1+ begin) :end search-end)))
         ((null begin) nil)
      (if (funcall test substr (subseq str begin (+ begin count)))
        (return-from find-substring t)))))

(defun internable-string-p (str)
  (when (non-empty-string str)
    (let ((pkg-end (position #\: str)))
      (if pkg-end
        (find-package (read-from-string (subseq str 0 pkg-end)))
        t))))

(defun make-mutable-nsstring (string)
  (with-encoded-cstrs :utf-8 ((s string))
    (#/initWithUTF8String: (#/alloc ns:ns-mutable-string) s)))



(provide :ns-string-utils)