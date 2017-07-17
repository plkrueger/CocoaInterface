;; kvo-slot.lisp

#|
The MIT license.

Copyright (c) 2010 Pagetul L. Krueger

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
  (require :undo)
  (require :ns-object-utils)
  (require :binding-utils))

(in-package :ccl)
(export 
 '(defnotification
   has-kvo-slots-p
   kvo-classes
   kvo-object
   kvo-slot-for
   new-kvo-classes
   slot-with-name
   value-for-kvo-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and macros

(defun slot-with-name (class slot-name)
  (find slot-name (class-slots class) :key #'ccl::slot-definition-name))

(defmacro defnotification ((instance-arg slot-arg new-value-arg)
                           &rest function-forms)
  ;; Creates a notification function that is called after setting a new value for
  ;; the specified slot-name in the specified class. This differs from normal accessor
  ;; methods in that it is called even if the slot was directly set using (setf slot-value).
  ;; Therefore it can be used to get notification when a KVO slot was modified via a binding
  ;; from a user interface element in Interface Builder.
  ;; A call might look like:
  ;; (defnotification ((self my-class) (slot my-slot-name) (new-value <new-value-qualifier>))
  ;;    (do-something-about self slot new-value)
  ;;    ... )
  ;; Without a name qualifier for the slot, the notification will be called for every slot in the object.
  (let* ((class-name (if (listp instance-arg)
                       (second instance-arg)
                       t))
         (class (find-class class-name))
         (instance (if (listp instance-arg)
                     (first instance-arg)
                     instance-arg))
         (slot-var (if (listp slot-arg)
                     (first slot-arg)
                     slot-arg))
         (slot-name (if (listp slot-arg)
                      (second slot-arg)))
         (slot (slot-with-name class slot-name))
         (slot-qualifier (if slot
                           `(eql (slot-with-name ,class ',slot-name))
                           t))
         (class-arg (gensym)))   
    `(defmethod (setf slot-value-using-class) :after 
       (,new-value-arg
        (,class-arg (eql ,class))
        (,instance ,class-name)
        (,slot-var ,slot-qualifier))
       ,@function-forms)))

(defun canonical-kvo-key (dslot)
  (let ((declared-key (kvo-slot-definition-kvo dslot)))
    (if (symbolp declared-key)
      (iu::lisp-to-objc-keypathname declared-key)
      declared-key)))

(defmethod corresponding-direct-slots ((class slots-class) slot-name)
  ;; Find an ordered list of any direct slots with slot-name in class 
  ;; hierarchy of class
  (do* ((top-class (find-class t))
        (dslots nil)
        (classes (list class) new-classes)
        (new-classes nil))
       ((null classes) (nreverse dslots))
    (setf new-classes nil)
    (dolist (cl classes)
      (unless (eq cl top-class)
        (setf new-classes (append (class-direct-superclasses cl) new-classes))
        (let ((ds (find slot-name
                        (%class-direct-slots cl)
                        :key #'slot-definition-name)))
          (when ds
            (push ds dslots)))))))

(defmethod corresponding-kvo-direct-slots ((class slots-class) slot-name)
  (let ((kvo-slot-classes (list (find-class 'kvo-direct-slot-definition)
                                (find-class 'kvo-foreign-direct-slot-definition))))
    (remove-if-not #'(lambda (ds)
                       (member (class-of ds) kvo-slot-classes))
                   (corresponding-direct-slots class slot-name))))

(let ((kvo-classes nil)
      (new-kvo-classes nil))

  (defmethod add-kvo-class ((self class))
    (pushnew self kvo-classes)
    (pushnew self new-kvo-classes))

  (defun kvo-classes ()
    kvo-classes)

  (defun new-kvo-classes ()
    (prog1
      new-kvo-classes
      (setf new-kvo-classes nil)))

 ) ;; end of functions using kvo-classes
            
(let ((kvo-hash (make-hash-table)))

  (defmethod invalidate-kvo-hash ((self class))
    (setf (gethash self kvo-hash) nil))
  
  (defmethod class-kvo-slots (class)
    (or (gethash class kvo-hash)
        (setf (gethash class kvo-hash)
              (remove-if-not #'(lambda (slot)
                                 (or (typep slot 
                                            'kvo-foreign-effective-slot-definition)
                                     (typep slot 
                                            'kvo-effective-slot-definition)))
                             (class-slots class)))))

  (defmethod kvo-slots ((self standard-object))
    (class-kvo-slots (class-of self)))

  (defmethod kvo-slots (obj)
    ;; needed so that (typep obj 'ccl::kvo-obj) works on anything
    (declare (ignore obj))
    nil)

)

(defmethod kvo-slot-for ((self standard-object) key-string)
  (find key-string 
        (kvo-slots self)
        :key #'kvo-slot-definition-kvo
        :test #'(lambda (key-string slot-keys) 
                  (member key-string slot-keys :test #'string=))))

(defmethod value-for-kvo-key ((self standard-object) key-string)
  ;; find a slot where the key-string is a member of the list that is the
  ;; value of its kvo slot and return the value of that slot
  (let ((slot (kvo-slot-for self key-string)))
    (when slot
      (slot-value self (slot-definition-name slot)))))

(defmethod (setf value-for-kvo-key) (new-value (self standard-object) key-string)
  (let ((slot (kvo-slot-for self key-string)))
    (if slot
      (let* ((undo-string (kvo-slot-definition-undo slot))
             (slot-name (slot-definition-name slot))
             (current-boundp (slot-boundp self slot-name))
             (current-value (and current-boundp (slot-value self slot-name))))
        (values (prog1 (setf (slot-value self slot-name) new-value)
                  (when undo-string
                    (iu::set-undo self
                                  (if current-boundp
                                    #'(lambda ()
                                        (setf (value-for-kvo-key self key-string) current-value))
                                    #'(lambda ()
                                        (slot-makunbound self slot-name)))
                                  (if (stringp undo-string)
                                    undo-string
                                    "set value"))))
                slot))
      (values nil nil))))

(deftype kvo-object ()
  '(satisfies kvo-slots))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct slot classes

(defclass kvo-direct-slot-definition (standard-direct-slot-definition)
  ((kvo :initarg :kvo :initform nil :accessor kvo-slot-definition-kvo)
   (undo :initarg :undo :initform nil :accessor kvo-slot-definition-undo)))

(defclass kvo-foreign-direct-slot-definition (foreign-direct-slot-definition)
  ((kvo :initarg :kvo :initform nil :accessor kvo-slot-definition-kvo)
   (undo :initarg :undo :initform nil :accessor kvo-slot-definition-undo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOP methods to select which direct-slot class to use.
;; Use the kvo slot metaclasses if the slot has a :kvo attribute
;; Use a different metaclass for :foreign-type and non :foreign-type classes
;; because we want to do different things to make them kvo compliant.

(defmethod direct-slot-definition-class :around ((class objc:objc-class-object)
                                                 &rest initargs)
  (let ((base-class (call-next-method)))
    (if (or (getf initargs :kvo) (getf initargs :undo))
      (if (eq base-class (find-class 'foreign-direct-slot-definition))
        (find-class 'kvo-foreign-direct-slot-definition)
        (find-class 'kvo-direct-slot-definition))
      base-class)))

(defmethod direct-slot-definition-class :around ((class standard-class)
                                                 &rest initargs)
  (if (or (getf initargs :kvo) (getf initargs :undo))
    (find-class 'kvo-direct-slot-definition)
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kvo-effective-slot-definition class

(defclass kvo-effective-slot-definition (standard-effective-slot-definition)
  ((kvo :accessor kvo-slot-definition-kvo)
   (undo :accessor kvo-slot-definition-undo)))

(defmethod initialize-instance :after ((self kvo-effective-slot-definition)
                                       &key &allow-other-keys)
  (let* ((class (standard-effective-slot-definition.class self))
         (slot-name (standard-effective-slot-definition.name self))
         (dslots (corresponding-kvo-direct-slots class slot-name)))
    (add-kvo-class class)
    (invalidate-kvo-hash class)
    (setf (kvo-slot-definition-kvo self)
          (delete-duplicates (mapcar #'canonical-kvo-key dslots) :test #'string=))
    (setf (kvo-slot-definition-undo self) 
          (find-if #'identity (mapcar #'kvo-slot-definition-undo dslots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kvo-foreign-effective-slot-definition class

(defclass kvo-foreign-effective-slot-definition (foreign-effective-slot-definition)
  ((kvo :accessor kvo-slot-definition-kvo)
   (undo :accessor kvo-slot-definition-undo)))

(defmethod initialize-instance :after ((self kvo-foreign-effective-slot-definition)
                                       &key &allow-other-keys)
  (let* ((class (standard-effective-slot-definition.class self))
         (slot-name (standard-effective-slot-definition.name self))
         (dslots (corresponding-kvo-direct-slots class slot-name)))
    (add-kvo-class class)
    (invalidate-kvo-hash class)
    (setf (kvo-slot-definition-kvo self)
          (delete-duplicates (mapcar #'canonical-kvo-key dslots) :test #'string=))
    (setf (kvo-slot-definition-undo self) 
          (when (ccl::subclassp class ns:ns-document)
            ;; automatic undo is only supported for slots in some subclass of NSDocument
            (find-if #'identity (mapcar #'kvo-slot-definition-undo dslots))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOP methods to select which effective-slot class to use.

(defmethod effective-slot-definition-class :around ((class slots-class)
					    &rest initargs)
  ;; basically we want to select the effective-slot-definition-class based on the
  ;; class of the corresponding direct-slot. So we find a direct slot of the same
  ;; name if it exists and go from there. If there isn't one just call-next-method.
  (let* ((slot-name (getf initargs :name))
         (dslots (corresponding-direct-slots class slot-name))
         (dslot-classes (mapcar #'class-of dslots))
         (base-class (call-next-method)))
    (if dslots
      (cond ((find (find-class 'kvo-foreign-direct-slot-definition) dslot-classes)
             (find-class 'kvo-foreign-effective-slot-definition))
            ((find (find-class 'kvo-direct-slot-definition) dslot-classes)
             (find-class 'kvo-effective-slot-definition))
            (t
             base-class))
      base-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOP methods to set slot values for instance of a specified class and 
;; effective slots of a particular class. This is where we enforce the KVO
;; notification protocol.

(defmethod (setf slot-value-using-class)
           :around (value
                    class
                    instance
                    (slotd kvo-effective-slot-definition))
  (declare (ignore value class))
  ;; Calls to #/willChange... and #/didChange... must be nested so the order
  ;; of calls to these two is reversed 
  (flet ((objc-key (key)
           (iu:lisp-to-temp-nsstring key))
         (will-change (key)
           (iu:will-change-value-for-key instance key))
         (did-change (key)
           (iu:did-change-value-for-key instance key)))
    (mapcar #'will-change (kvo-slot-definition-kvo slotd))
    (prog1
      (call-next-method)
      (mapcar #'did-change (reverse (kvo-slot-definition-kvo slotd))))))

(provide :kvo-slot)

#|

Testing

(require :kvo-slot)

(defclass c1 ()
  ((s1 :accessor s1)
   (s2 :accessor s2 :kvo s2)))

(class-slots (find-class 'c1))
(class-direct-slots (find-class 'c1))

(setf i1 (make-instance 'c1))

(setf (s1 i1) 1)
(setf (s2 i1) 2)

(defclass c2 (ns:ns-object)
  ((s1 :accessor s1)
   (s2 :accessor s2 :kvo s2)
   (s3 :accessor s3 :foreign-type :id :kvo s3))
  (:metaclass ns:+ns-object))

(class-slots (find-class 'c2))
(class-direct-slots (find-class 'c2))

(setf i2 (make-instance 'c2))

(setf (s1 i2) 1)
(setf (s2 i2) 2)
(setf (s3 i2) #@"testobject")

(defclass c3 (c2)
  ((s4 :accessor s4)
   (s5 :accessor s5 :kvo s5))
  (:metaclass ns:+ns-object))

|#

