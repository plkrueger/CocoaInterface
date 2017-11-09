;; undo.lisp

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


;; Provides a lisp-friendly interface to the undo manager
;; To specify how to undo some action call 
;;   (set-undo <target> <undo-closure> <undo-string>)
;; where <target> is the instance being modified
;; <undo-closure is a function of 0 arguments that when funcalled will undo the change
;; being made, and
;; <undo-string> is a string that names the action that will be undone or redone
;; Typically the undo-string should name the action. So in a method that is adding 
;; something to a view, the undo-string should be something like "add". When in the process
;; of "undoing" the set-undo method will ignore this sting and the underlying Objective-C
;; method will take the action name for the redo menu item from the undo menu item. 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :path-trans))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for UNDO / REDO

(defmethod undo-target (target)
  ;; undo supported for subclasses of NSDocument or any object that specifies
  ;; an undo-target that is such a subclass by implementing this method.
  (when (typep target 'ns:ns-document)
    target))

(defmacro set-undo (target undo-closure &optional (undo-name nil))
  (let ((undo (gensym))
        (undo-target (gensym)))
    `(let* ((,undo-target (undo-target ,target))
            (,undo (and ,undo-target (#/undoManager ,undo-target))))
       (when ,undo
         (#/lispUndo: (#/prepareWithInvocationTarget: ,undo ,undo-target)
                      (make-ptr-wrapper ,undo-closure))
         (when (and ,undo-name (not (#/isUndoing ,undo)))
           (#/setActionName: ,undo (lisp-to-temp-nsstring ,undo-name)))))))

(defun update-with-undo (accessor-name instance new-value &key (undo-name nil) (test #'equal))
  ;; Convenience function that updates an instance slot only if the value is different from
  ;; whatever was there previously (using test specified). Does appropriate set-undo call if
  ;; value was changed. The instance should be a subclass of lisp-document.
  (let* ((reader (symbol-function accessor-name))
         (writer (symbol-function (list 'setf accessor-name)))
         (old-slot-val (funcall reader instance)))
    (unless (funcall test old-slot-val new-value)
      (set-undo instance
                #'(lambda ()
                    (update-with-undo accessor-name instance old-slot-val
                                      :undo-name undo-name
                                      :test test))
                undo-name)
      (funcall writer new-value instance))))

(objc:defmethod (#/lispUndo: :void)
                ((target ns:ns-document) (closure-lpw :id))
  (funcall (lpw-lisp-ptr closure-lpw)))


(provide :undo)