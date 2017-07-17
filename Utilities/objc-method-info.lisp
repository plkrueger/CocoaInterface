;; objc-method-info.lisp
#|
The MIT license.

Copyright (c) 2013 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subjec to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

#|

Provides information about objective-c methods. Used mainly in tools to aid developers.

|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :iu)
  
(defvar *foreign-types* 
  '(#>NSAppleEventManagerSuspensionID
    #>NSComparator
    #>NSDecimal
    #>NSHashEnumerator
    ;; #>NSHashTable
    #>NSHashTableCallBacks
    #>NSHashTableOptions
    #>NSUInteger
    #>NSMapEnumerator
    ;; #>NSMapTable
    #>NSMapTableKeyCallBacks
    #>NSMapTableOptions
    #>NSMapTableValueCallBacks
    #>NSPoint
    #>NSPointArray
    #>NSPointPointer
    #>NSRange
    #>NSRangePointer
    #>NSRect
    #>NSRectArray
    #>NSRectPointer
    #>NSSize
    #>NSSizeArray
    #>NSSizePointer
    #>NSSocketNativeHandle
    #>NSStringEncoding
    #>NSSwappedDouble
    #>NSSwappedFloat
    #>NSTimeInterval
    #>NSUInteger
    #>NSUncaughtExceptionHandler
    #>NSZone
    #>BOOL
    #>CGFloat
    #>SEL
    :id
    :void
    :short
    :int
    :float
    :double))

(defun keyword-to-name (keyw)
  (let ((state :lower)
        (ch nil)
        (keyw-str (symbol-name keyw)))
    (with-output-to-string (os)
      (with-input-from-string (is keyw-str)
        (dotimes (i (length keyw-str) os)
          (setf ch (read-char is))
          (cond ((char= ch #\<)
                 (setf state :upper))
                ((char= ch #\>)
                 (if (eq state :upper)
                   (setf state :lower)
                   (write-char ch os)))
                (t
                 (write-char (if (eq state :lower)
                               (char-downcase ch)
                               ch)
                             os))))))))

(defun foreign-ptr-for-hashing (frt)
  ;; only converts foreign-pointer-type objects, leaves everything else alone
  (if (typep frt 'ccl::foreign-pointer-type)
    (list :pointer (ccl::foreign-pointer-type-to frt))
    frt))
       

(let ((*type-ht* (make-hash-table :test #'equal)))
  
  (defun populate-type-ht ()
    (dolist (ft *foreign-types*)
      (setf (gethash (foreign-ptr-for-hashing (ccl::parse-foreign-type ft)) *type-ht*)
            (keyword-to-name ft))))

  (defun cached-foreign-type-string (foreign-type)
    (gethash (foreign-ptr-for-hashing foreign-type) *type-ht*)))

(populate-type-ht)

;; return the reference count for an Objective-C object
(defmethod ref-count ((self ns:ns-object))
  (#_CFGetRetainCount self))

;; call this with something like: (method-for "func" obj-that-can-handle-func)
;; This isn't all that useful because the returned arg types are very high level.
;; For example, all ns-objects are of a type "@".
(defmethod method-for (method-name obj)
  (let ((method-sig (#/methodSignatureForSelector: obj (iu::get-selector method-name))))
    (values
     method-name
     obj
     (%get-cstring (#/methodReturnType method-sig))
     (let ((num-args (#/numberOfArguments method-sig))
           (args nil))
       (dotimes (i num-args (nreverse args))
         (push (%get-cstring (#/getArgumentTypeAtIndex: method-sig i))
               args))))))

(defun method-info-list (message-name)
  (let* ((message-info (ccl::get-objc-message-info message-name))
         (methods (and message-info (ccl::objc-message-info-methods message-info)))
         (ambiguous (and message-info (ccl::objc-message-info-ambiguous-methods message-info))))
    (values
     methods
     ambiguous)))

;; find the signature of an Objective-C method name as a list
;; with class first, return value next, then all args.
(defun method-sigs (message-name)
  (let* ((mi-list (method-info-list message-name))
         (class-sigs nil))
    (dolist (m mi-list class-sigs)
      (push (list* (ccl::objc-method-info-class-name m)
                   (ccl::ensure-foreign-type (ccl::objc-method-info-result-type m))
                   (mapcar #'(lambda (arg)
                               (ccl::ensure-foreign-type arg))
                           (ccl::objc-method-info-arglist m)))
            class-sigs))))

(defmethod method-sig-for-class-method ((class objc::objc-class) message-name)
  (find (ccl::lisp-to-objc-classname (class-name class))
        (method-sigs message-name)
        :key #'first
        :test #'string=))

(defun foreign-type-string (ft)
  (or  (cached-foreign-type-string ft)
       (typecase ft
         (ccl::foreign-pointer-type
          (foreign-type-string (ccl::foreign-pointer-type-to ft)))
         (ccl::foreign-record-type
          (keyword-to-name (ccl::foreign-record-type-name ft)))
         (t
          (format nil "~s" ft)))))

(defmethod method-info ((class objc::objc-class) message-name)
  (let* ((meth-sig (method-sig-for-class-method class message-name))
         (ret (foreign-type-string (second meth-sig)))
         (args (mapcar #'foreign-type-string (cddr meth-sig))))
    (when meth-sig (list* ret args))))

(defmethod method-info (class message-name)
  (declare (ignore class))
  (let* ((meth-sig (first (method-sigs message-name)))
         (ret (foreign-type-string (second meth-sig)))
         (args (mapcar #'foreign-type-string (cddr meth-sig))))
    (when meth-sig (list* ret args))))

(defun print-method-sigs (message-name)
  (format t 
          "~:{~%Class: ~s~%Return: ~s~%Args:  ~@{~s~^~%       ~}~%~}"
         (method-sigs message-name))
  (values))

(defun parse-foreign-dec (decl)
  (ccl::parse-foreign-type decl))

(provide :objc-method-info)
