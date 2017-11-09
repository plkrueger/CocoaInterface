;; path-trans.lisp

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
  (require :nslog-utils)
  (require :ns-string-utils)
  (require :assoc-array))

(in-package :iu)

;; debugging facility
;; Used primarily in binding-utils.lisp, which is dependent on this file

(defvar *log-bindings* nil)

(defun log-bindings (&optional (on-off t))
  (setf *log-bindings* on-off))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lisp-ptr-wrapper
;;;
;;; This is a class that encapsulates a pointer to a lisp object so we can pass this
;;; off to an Objective-C view and know what it points to when we get it back later.
;;; Added the ability to handle bindings.

#|
(defclass lisp-ptr-wrapper (ns:ns-object)
  ((lpw-lisp-ptr :accessor lpw-lisp-ptr)
   (lpw-controller :accessor lpw-controller)
   (lpw-depth :accessor lpw-depth)
   (lpw-parent :accessor lpw-parent))
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/copyWithZone: :id)
                ((self lisp-ptr-wrapper) (zone (* #>NSZone)))
  (when *log-bindings*
    (ns-log-format "Copying wrapper for ~s" (lpw-lisp-ptr self)))
  self)

(let ((kvc-observed (make-instance 'assoc-array :rank 2 :tests (list #'eql #'equal)))
      (obj-wrappers (make-instance 'assoc-array :rank 2)))
  ;; this assoc-array keeps track of paths that are being observed and the
  ;; corresponding lisp-ptr-wrapper object that is ostensibly being observed.

  (defun make-ptr-wrapper (ptr &key (depth 1) (parent nil) (controller nil))
    (when *log-bindings*
      (ns-log-format "Making wrapper for ~s" ptr))
    (let ((lpw (make-instance 'lisp-ptr-wrapper)))
      (setf (lpw-lisp-ptr lpw) ptr)
      (setf (lpw-depth lpw) depth)
      (setf (lpw-parent lpw) parent)
      (setf (lpw-controller lpw) controller)
      (setf (assoc-aref obj-wrappers controller ptr) lpw)
      lpw))

  (defmethod wrapper-for (lisp-obj &key (controller nil) (depth 0) (parent nil))
    (or (assoc-aref obj-wrappers controller lisp-obj)
        (setf (assoc-aref obj-wrappers controller lisp-obj)
              (make-ptr-wrapper lisp-obj 
                                :depth depth
                                :parent parent
                                :controller controller))))

  (defmethod note-kvc-observed ((self lisp-ptr-wrapper) lisp-obj path)
    (when *log-bindings*
      (ns-log-format "Observing ~s for ~s" path lisp-obj))
    (pushnew self (assoc-aref kvc-observed lisp-obj path)))

  (defmethod will-change-value-for-key (owner key)
    ;; called from a lisp object to tell us that a value will be changed.
    ;; We find the lisp-ptr-wrapper instances that have been used to access
    ;; the owner via the specified key and call the appropriate
    ;; method to let KVC know what is going on.
    ;; Could also be called for a :kvo lisp slot in an objective-c instance
    ;; and if so we should call the willChange... method for this instance.
    (when *log-bindings*
      (ns-log-format "Will change ~s for ~s" key owner))
    (let ((owner-lpws (assoc-aref kvc-observed owner key))
          (objc-key (lisp-to-temp-nsstring (if (stringp key)
                                             key
                                             (lisp-to-objc-keypathname key)))))
      (if (typep owner 'ns:ns-object)
        (#/willChangeValueForKey: owner objc-key))
      (dolist (lpw owner-lpws)
        (when *log-bindings*
          (ns-log-format "#/willChangeValueForKey: ~s ~s" lpw objc-key))
        (#/willChangeValueForKey: lpw objc-key))))

  (defmethod did-change-value-for-key (owner key)
    ;; called from a lisp object to tell us that a value changed.
    ;; We find the lisp-ptr-wrapper instances that have been used to access
    ;; the owner via the specified key and call the appropriate
    ;; method to lets KVC know what is going on.
    ;; Could also be called for a :kvo lisp slot in an objective-c instance.
    ;; If so, call the didChange... method for this instance.
    (when *log-bindings*
      (ns-log-format "Did change ~s for ~s" key owner))
    (let ((owner-lpws (assoc-aref kvc-observed owner key))
          (objc-key (lisp-to-temp-nsstring (if (stringp key)
                                             key
                                             (lisp-to-objc-keypathname key)))))
      (if (typep owner 'ns:ns-object)
        (#/didChangeValueForKey: owner objc-key))
      (dolist (lpw owner-lpws)
        (when *log-bindings*
          (ns-log-format "#/didChangeValueForKey: ~s ~s" lpw objc-key))
        (#/didChangeValueForKey: lpw objc-key))))

  (defun kvc-observed ()
    kvc-observed)
)


(defmacro objc-method-for (meth-name lisp-meth)
  (let ((meth-sym (gentemp))
        (lisp-fun (gentemp)))
    `(let ((,meth-sym  (intern (symbol-name ,meth-name) (find-package :nextstep-functions)))
           (,lisp-fun ,lisp-meth))
       (eval `(objc:defmethod (,,meth-sym :id)
                              ((self ns:ns-object))
                ;; ns:ns-object is always passed, never a null pointer, but if a null object
                ;; needs to be passed it will encoded as a lisp-point-wrapper pointinG to nil. 
                ;; So lisp functions must always be prepared to get nil as an argument.
                ;; Return value is always a wrapper so that when converted back to lisp we
                ;; get what was intended.
                (wrapper-for (funcall ,,lisp-fun (coerce-obj self t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods to support binding to Lisp slots and value translation between bound objects

(let ((key-returns (make-hash-table :test #'equal)))
  ;; key-returns tells us whether the value for a given key should be returned as a lisp-ptr-wrapper
  ;; or an Objective-C value converted from the the lisp value retrieved using the key. The legal
  ;; hash values are :path and :convert
  
  (defun set-key-return (key-str role)
    (setf (gethash key-str key-returns) role))

  (defun key-return (key-str)
    (gethash key-str key-returns nil))

)

(let ((path-trans (make-instance 'assoc-array :rank 2 :tests '(eql equal))))
  
  (defun objc-to-lisp-keypathname (name-str)
    ;; translate a name from Objective-C to Lisp
    ;; Use standard translation for function/slot names except the use of 
    ;; underscore (#\_) is used initially to delimit a package specifier
    (let* ((package-end (if (char= (elt name-str 0) #\_)
                          (position #\_ name-str :start 1)))
           (pkg-string (string-upcase (subseq name-str 1 package-end)))
           (path-string (if package-end 
                          (subseq name-str (1+ package-end))
                          name-str))
           (pkg (or (and package-end
                         (find-package pkg-string))
                    (find-package :cl-user))))
      ;; cache the string we are translating so that we can reverse
      ;; translate to exactly the same string used by the developer.
      (setf (assoc-aref path-trans pkg path-string) name-str)
      (ccl::compute-lisp-name path-string pkg)))
  
  (defun lisp-to-objc-keypathname (name)
    ;; translate name (symbol or string or function) from Lisp to Objective-C
    ;; If we previously cached a string that came from Objective-C and
    ;; was translated to this name, then translate back to it. This
    ;; prevents problems caused by alternative package names/nicknames
    ;; that the developer might have used in Interface Builder.
    ;; Use standard translation for function/slot names except prefix
    ;; package name and underscore if package is not cl-user
   
    (if (functionp name)
      (find-or-make-func-keypath name)
      (let ((pkg (if (symbolp name)
                   (symbol-package name)
                   (find-package :cl-user)))
            (name-str (string name))
            (name-segments nil))
        (or (assoc-aref path-trans pkg name-str)
            (progn
              (do* ((start 0 (1+ pos))
                    (pos (position #\- name-str)
                         (position #\- name-str :start start)))
                   ((null pos) (setf name-segments (nreverse (cons (subseq name-str start) name-segments))))
                (setf name-segments (cons (subseq name-str start pos) name-segments)))
              (setf name-segments (cons (string-downcase (first name-segments))
                                        (mapcar #'string-capitalize (rest name-segments))))
              (unless (eq pkg (find-package :cl-user))
                (setf name-segments (cons (concatenate 'string
                                                       "_"
                                                       (lisp-to-objc-keypathname (or (first (package-nicknames pkg))
                                                                                     (package-name pkg)))
                                                       "_")
                                          name-segments)))
              (apply #'concatenate 'string name-segments))))))

  (defun find-or-make-func-keypath (lisp-func)
    ;; find a previously made symbol corresponding to the lisp object if it exists, or create one
    ;; Cache so we can easity translate in both directions.
    ;; Create an objc method that converts an objc argument to lisp, applies the method, and
    ;; converts it back to objc. This lets lisp functions be used as intermediate path elements
    ;; for bindings.
    (or (assoc-aref path-trans lisp-func nil)
        (let* ((sym-package (find-package :cl-user))
               (sym (gentemp "Trans" sym-package))
               (sym-name (symbol-name sym)))
          (setf (assoc-aref path-trans sym-package sym-name) lisp-func)
          (setf (assoc-aref path-trans lisp-func nil) sym-name)
          (objc-method-for sym lisp-func)
          sym-name)))

  (defun func-for-keypath (keypath)
    ;; find the function represented by the keypath if it exists
    (let ((func (assoc-aref path-trans (find-package :cl-user) keypath)))
      (when (functionp func) func)))

  (defun convert-path-element (pe)
    (cond ((stringp pe)
           pe)
          ((functionp pe)
           (find-or-make-func-keypath pe))))
  
  (defun convert-path-list (key-path)
    ;; key-path        ::= <path-object> | ( <path-object>* )
    ;; path-object     ::= <path-string> | <lisp-accessor-function>
    ;; path-string     ::= "<path-elt-string>{.<path-string>}*
    ;; path-elt-string ::= any legal string for Objective-C keypaths
    (let ((path-strs  (if (listp key-path)
                        (mapcar #'convert-path-element key-path)
                        (list (convert-path-element key-path)))))
      (dolist (ps (butlast path-strs))
        (set-key-return ps :path))
      (set-key-return (first (last path-strs)) :convert)
      (lisp-to-temp-nsstring (format nil "~{~a~^.~}" path-strs))))
  
)


(provide :path-trans)