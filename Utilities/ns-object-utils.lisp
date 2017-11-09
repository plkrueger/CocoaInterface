;; ns-object-utils.lisp
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
  (require :ns-string-utils)
  (require :binding-utils)
  (require :nslog-utils)
  (require :date)
  (require :alert)
  (require :decimal))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variable

(defvar *debug-convert* nil)
(defvar *minimal-hash-table-encoding* nil)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Special types

(deftype lisp-slot-object ()
  '(satisfies ccl::has-lisp-slot-vector))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

(defmethod class-conforms-to-protocol ((cl objc:objc-class-object) prot)
  (let ((pr (#_NSProtocolFromString (coerce-obj prot 'ns:ns-string))))
    (unless (eql pr (%null-ptr))
      (#/conformsToProtocol: cl pr))))

(defmethod class-conforms-to-protocol (cl prot)
  (declare (ignore cl prot))
  nil)

(defun print-ns-object (ns-obj)
  ;; default print methods for objects truncate strings at 1024 characters for some reason
  ;; this function doesn't
  (if (ccl::objc-object-p ns-obj)
    (format t "~a" (ns-to-lisp-string (#/description ns-obj)))
    (format t "~s" ns-obj)))

(defun obj-if-not-null (ns-obj)
  (if (eql ns-obj (%null-ptr))
    nil
    ns-obj))

(defun retain-if-necessary (obj)
  (declare (special *unconverting*))
  (when (and *unconverting* (ccl::objc-object-p obj)
             (#/retain obj)))
  obj)

(defun unreadable-object-string-p (str)
  ;; returns t if str contains a "#<" substring
  (search "#<" str))

(defun string-to-interned-symbol (str)
  ;; str can be in form a, a:b, or a::b
  (let* ((c1-pos (or (position #\: str) 0))
         (c2-pos (or (position #\: str :from-end t) -1))
         (pkg-str (string-upcase (subseq str 0 c1-pos)))
         (name-str (string-upcase (subseq str (1+ c2-pos)))))
    (if (string= pkg-str "")
      (unless (string= name-str "")
        (intern name-str))
      (when (find-package pkg-str)
        (intern name-str pkg-str)))))

(defun recursive-map (func obj)
  ;; apply func to obj and then to the results
  ;; returned from that application recursively
  ;; func can return either a list of objects or a single object
  (let ((res (funcall func obj)))
    (setf res (if (listp res) (copy-list res) (list res)))
    (nconc res (mapcan #'(lambda (sub-obj)
                           (recursive-map func sub-obj))
                       res))))

(defun class-name-string (class)
  ;; we want a fully-qualified string (including package designation) for the class
  ;; so symbol-name won't cut it.
  (format nil "~s" (class-name class)))

(defun ns-to-lisp-classname (classname-str &optional (previous-str ""))
  ;; unlike ccl::objc-to-lisp-classname which does a syntactic translation
  ;; and therefore fails to provide a package-qualified classname, this
  ;; function finds the actual class and returns its name.
  (unless (non-empty-string classname-str)
    (return-from ns-to-lisp-classname ""))
  (let ((cls (#_NSClassFromString (iu::lisp-to-temp-nsstring classname-str))))
    (if (eql cls (%null-ptr))
      ;; There isn't a known class with this name.
      ;; If the lexical conversion of classname-str is identical to the part 
      ;; of the non-package portion of the previous string then just use the 
      ;; previous string. 
      (let* ((lisp-name (ccl::objc-to-lisp-classname classname-str))
             (col-pos (position #\: previous-str :test #'char= :from-end t))
             (prev-base (if col-pos
                          (subseq previous-str (1+ col-pos))
                          previous-str)))
        (if (string-equal lisp-name prev-base)
          previous-str
          lisp-name))
      (class-name-string cls))))

(defun find-ns-classes (&key (if-pkg nil) (if-not-pkg nil))
  ;; finds all subclasses of ns:ns-object which are explicitly in the non-null if-pkg
  ;; or finds all that are explicitly not in the non-null if-not-pkg
  ;; either keyword argument may contain a single package or package-name or a list
  ;; of packages or package names
  (let ((if-pkgs (mapcar #'find-package 
                         (and if-pkg (if (consp if-pkg) if-pkg (list if-pkg)))))
        (if-not-pkgs (mapcar #'find-package 
                             (and if-not-pkg (if (consp if-not-pkg) if-not-pkg (list if-not-pkg)))))
        (ns-classes (mapcar #'class-name (recursive-map #'class-direct-subclasses ns:ns-object))))
    (when if-pkgs
      (setf ns-classes (delete-if-not #'(lambda (class-name)
                                          (member (find-package (symbol-package class-name)) if-pkgs))
                                      ns-classes)))
    (when if-not-pkgs
      (setf ns-classes (delete-if #'(lambda (class-name)
                                      (member (find-package (symbol-package class-name)) if-not-pkgs))
                                  ns-classes)))
    ns-classes))

(defun ns-to-lisp-object (ns-obj &key (lisp-class nil) (ns-format nil))
  ;; convert an arbitrary NSObject object to an appropriate lisp object.
  ;; Often done so that it can replace the old-lisp-obj when edited
  ;; An empty string @"" returns nil if old-lisp-obj is not a string
  (when *debug-convert*
    (ns-log-format "Converting ~s to lisp~@[ lisp-class = ~s~]~@[ ns-format = ~s~]" ns-obj lisp-class ns-format))
  (retain-if-necessary
   (cond ((ccl::subclassp (class-of ns-obj) lisp-class)
          ;; ns-obj is a subclass of the target lisp class so just return the new value
          ns-obj)
         ((or (eql ns-obj (%null-ptr))
              (eql ns-obj #$NSNoSelectionMarker)
              (eql ns-obj #$NSNullPlaceholderBindingOption)
              (eql ns-obj #$NSNotApplicablePlaceholderBindingOption))
          (if (ccl::subclassp lisp-class (find-class 'string))
              ""
              nil))
         ((typep ns-obj 'lisp-ptr-wrapper)
          ;; just strip the wrapper and return the original object
          (lpw-lisp-ptr ns-obj))
         ((typep ns-obj 'ns-sym)
          (sym ns-obj))
         ((typep ns-obj 'ns-misc)
          (obj ns-obj))
         ((typep ns-obj 'ns-func)
          (func ns-obj))
         ((typep ns-obj 'ns:ns-decimal-number)
          (cond ((ccl::subclassp lisp-class (find-class 'double-float))
                 ;; convert the decimal to a double
                 (#/doubleValue ns-obj))
                ((ccl::subclassp lisp-class (find-class 'float))
                 ;; convert the decimal to a float
                 (#/floatValue ns-obj))
                (t
                 ;; otherwise convert it to an appropriate lisp integer with assumed
                 ;; decimals (see ip;Utilities;decimal.lisp)
                 (if (and (listp ns-format) (eq (first ns-format) :decimal))
                     (lisp-from-ns-decimal ns-obj :decimals (second ns-format))
                     (lisp-from-ns-decimal ns-obj)))))
         ((typep ns-obj 'ns:ns-url)
          (ns-to-lisp-string (if (#/isFileURL ns-obj)
                                 (#/path ns-obj)
                                 (#/absoluteString ns-obj))))
         ((typep ns-obj 'ns:ns-number)
          (cond ((and (listp ns-format) (eq (first ns-format) :decimal))
                 (round (* (expt 10 (second ns-format)) (#/floatValue ns-obj))))
                ((eq lisp-class (find-class 'symbol))
                 ;; how can a number be of class 'symbol? Simple, it is nil if number
                 ;; is 0 or t otherwise
                 (not (= (#/intValue ns-obj) 0)))
                ((ccl::subclassp lisp-class (find-class 'double-float))
                 ;; convert the number to a double
                 (#/doubleValue ns-obj))
                ((ccl::subclassp lisp-class (find-class 'float))
                 ;; convert the number to a float
                 (#/floatValue ns-obj))
                ((ccl::subclassp lisp-class (find-class 'integer))
                 ;; convert the number to an integer
                 (#/longLongValue ns-obj))
                ((ccl::subclassp lisp-class (find-class 'ratio))
                 ;; convert the number to an integer
                 (#/floatValue ns-obj))
                ((eql ns-obj (#/numberWithBool: ns:ns-number #$YES))
                 ;; the number is the constant for #$YES, convert to t
                 ;; This does NOT convert all numbers with the value 1 to t
                 t)
                ((eql ns-obj (#/numberWithBool: ns:ns-number #$NO))
                 ;; the number is the constant for #$NO, convert to nil
                 ;; This does NOT convert all numbers with the value 0 to nil
                 nil)
                (t
                 ;; no specific target, so just read from the string representation
                 (read-from-string (ns-to-lisp-string
                                    (#/descriptionWithLocale: ns-obj (%null-ptr)))
                                   nil nil))))
         ((typep ns-obj 'ns:ns-date)
          (ns-to-lisp-date ns-obj))
         ((typep ns-obj 'lisp-object-reference)
          (objc-to-std-instance ns-obj))
         ((typep ns-obj 'ns:ns-dictionary)
          (cond ((ccl::subclassp lisp-class (find-class 'list))
                 (ns-to-lisp-assoc ns-obj))
                (t
                 (ns-to-lisp-hash-table ns-obj))))
         ((typep ns-obj 'ns:ns-array)
          (if (or (ccl::subclassp lisp-class (find-class 'list))
                  (eq lisp-class (find-class 'null))) ;; assume they want a list
              (ns-to-lisp-list ns-obj)
              (ns-to-lisp-array ns-obj)))
         ((typep ns-obj 'ns:ns-attributed-string)
          (cond ((ccl::subclassp lisp-class (find-class 'string))
                 (ns-attrib-to-lisp-string ns-obj))
                (t
                 (make-instance 'attributed-string :ns-str ns-obj))))
         ((typep ns-obj 'ns:ns-string)
          (let ((lisp-str (ns-to-lisp-string ns-obj)))
            (cond ((ccl::subclassp lisp-class (find-class 'string))
                   lisp-str)
                  ((ccl::subclassp lisp-class (find-class 'symbol))
                   (string-to-interned-symbol lisp-str))
                  ((ccl::subclassp lisp-class (find-class 'number))
                   (let ((num (read-from-string lisp-str :nil 0)))
                     (if (numberp num)
                         num
                         lisp-str)))
                  (t
                   lisp-str))))
         ((typep ns-obj 'ns:ns-null)
          nil)
         (t
          ;; can't convert so just return ns-obj
          ns-obj))))

(defun lisp-to-ns-object (lisp-obj &optional (ns-format nil))
  ;; convert an arbitrary lisp object to an appropriate NSObject so
  ;; that it can be displayed someplace
  (when *debug-convert*
    (ns-log-format "Converting ~s to ns~@[ ns-format = ~s~]" lisp-obj ns-format))
  (cond ((and (eq ns-format :archive)
              (typep lisp-obj 'ns:ns-object))
         (if (class-conforms-to-protocol (class-of lisp-obj) "NSCoding")
           ;; Any object that conforms to NSCoding can just be returned.
           ;; A lisp class that inherits from an Objective-C class which is NSCoding compliant
           ;; must itself implement appropriate encoding and decoding methods if it wants to 
           ;; add additional information to the archived object (and it should call-next-method
           ;; as the first thing it does).
           lisp-obj
           ;; otherwise convert to a lisp-object-reference
           (std-instance-to-objc lisp-obj)))
        ((ccl::objc-object-p lisp-obj)
         ;; it's already an NSObject so just return it
         lisp-obj)
        ((member (type-of lisp-obj) (list 'ns:ns-rect 'ns:ns-size 'ns:ns-point))
         ;; These types are let through even if they aren't exactly objc "objects"
         (if (eq ns-format :archive)
           ;; But if we're archiving them we need to encapsulate them
           (lisp-to-ns-misc lisp-obj)
           lisp-obj))
        ((eq ns-format :wrapper)
         (wrapper-for lisp-obj))
        ((eq ns-format :date)
         (if (stringp lisp-obj)
           (string-to-ns-date lisp-obj)
           ;; assume lisp-obj is an integer representing a lisp date
           (lisp-to-ns-date lisp-obj)))
        ((typep lisp-obj 'attributed-string)
         (cond ((eq ns-format :text)
                ;; The binding object doesn't accept NSAttributedStrings so
                ;; pass it an NSString
                (#/string (att-ns-str lisp-obj)))
               ((eq ns-format :archive)
                ;; We're archiving this to disk, so save it as we would
                ;; any other Lisp instance so that eq-ness is preserved when it is restored.
                (std-instance-to-objc lisp-obj))
               (t
                ;; Either null ns-format or :rich-text
                ;; Return the NSMutableAttributedString
                (att-ns-str lisp-obj))))
        ((and (consp ns-format) (eq (first ns-format) :decimal))
         (cond ((typep lisp-obj 'fixnum)
                (lisp-to-ns-decimal lisp-obj :decimals (second ns-format)))
               ((typep lisp-obj 'number)
                (lisp-to-ns-decimal (round (* (expt 10 (second ns-format)) lisp-obj))
                                    :decimals (second ns-format)))
               (t
                (lisp-to-ns-decimal 0 :decimals (second ns-format)))))
        ((typep lisp-obj '(signed-byte 16))
         (#/numberWithShort: ns:ns-number lisp-obj))
        ((typep lisp-obj '(signed-byte 32))
         (#/numberWithLong: ns:ns-number lisp-obj))
        ((typep lisp-obj '(signed-byte 64))
         (#/numberWithLongLong: ns:ns-number lisp-obj))
        ((typep lisp-obj 'double-float)
         (#/numberWithDouble: ns:ns-number lisp-obj))
        ((floatp lisp-obj)
         (#/numberWithFloat: ns:ns-number lisp-obj))
        ((ratiop lisp-obj)
         (#/numberWithFloat: ns:ns-number (float lisp-obj)))
        ((floatp lisp-obj)
         ;; some other type of floating number
         (#/numberWithFloat: ns:ns-number (float lisp-obj)))
        ((integerp lisp-obj)
         ;; some other type of integer number
         (#/numberWithLongLong: ns:ns-number (coerce lisp-obj '(signed-byte 64))))
        ((complexp lisp-obj)
         ;; no Objective-C counterpart so just return the realpart of the complex
         (#/numberWithFloat: ns:ns-number (float (realpart lisp-obj))))
        ((null lisp-obj)
         (#/numberWithBool: ns:ns-number #$NO))
        ((eq lisp-obj t)
         (#/numberWithBool: ns:ns-number #$YES))
        ((symbolp lisp-obj)
         (lisp-to-ns-sym lisp-obj))
        ((stringp lisp-obj)
         (let ((ns-str (lisp-to-temp-nsstring lisp-obj)))
           (if (eq ns-format :rich-text)
             (#/autorelease (#/initWithString: (#/alloc ns:ns-attributed-string) ns-str))
             ns-str)))
        ((hash-table-p lisp-obj)
         (lisp-to-ns-dict lisp-obj))
        ((or (vectorp lisp-obj) (consp lisp-obj))
         (lisp-to-ns-array lisp-obj))
        ((or (typep lisp-obj 'standard-object)
             (typep lisp-obj 'structure-object))
         (std-instance-to-objc lisp-obj))
        ((typep lisp-obj 'function)
         (lisp-to-ns-func lisp-obj))
        (t
         (lisp-to-ns-misc lisp-obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with standard-object instances

(defvar *converting* nil)
(defvar *unconverting* nil)

;; The *ns-object-hash* keeps track of instances that have been converted to objc objects and
;; only converts an instance one time.
(defvar *ns-object-hash* nil)

;; Similarly, *lisp-object-assoc* keeps track of objc objects that have been turned into lisp objects
;; and always returns the same lisp obj for the multiple invocations of objc-to-std-instance
;; that pass in the same objc object and are looking for the same lisp target class.
(defvar *lisp-object-assoc* nil)

(defun vals ()
  (declare (special *converting* *ns-object-hash*))
  (values *converting* *ns-object-hash*))

(defmacro while-converting (&rest body)
  `(let ((*converting* t)
         (*ns-object-hash* (make-hash-table)))
     (prog1 (progn ,@body)
       (maphash #'(lambda (key val)
                    (declare (ignore key))
                    (#/release val))
                *ns-object-hash*))))

(defmacro while-unconverting (&rest body)
  `(let ((*unconverting* t)
         (*lisp-object-assoc* (make-instance 'assoc-array :rank 2 :tests (list #'eql #'eq))))
     (prog1 (progn ,@body)
       ;; release all the objects we retained in note-unconverted-object
       (map-assoc-array #'(lambda (obj x)
                            (declare (ignore x))
                            (#/release obj))
                        *lisp-object-assoc*))))

(defun note-converted-object (lisp-obj ns-obj)
  (declare (special *converting* *ns-object-hash*))
  (when *converting*
    (#/retain ns-obj)
    (setf (gethash lisp-obj *ns-object-hash*) ns-obj)))

(defun note-unconverted-object (ns-obj lisp-target-class lisp-obj)
  (declare (special *unconverting* *lisp-object-assoc*))
  (when *unconverting*
    (#/retain ns-obj)
    (setf (assoc-aref *lisp-object-assoc* ns-obj lisp-target-class) lisp-obj)))

(defun converted-object (lisp-obj)
  (declare (special *converting* *ns-object-hash*))
  (and *converting*
       (gethash lisp-obj *ns-object-hash* nil)))

(defun unconverted-object (lisp-obj lisp-target-class)
  (declare (special *unconverting* *lisp-object-assoc*))
  (and *unconverting*
       (assoc-aref *lisp-object-assoc* lisp-obj lisp-target-class)))

;; lisp-object-reference: A class that encapsulates references to other instances so that we can have circular
;; references.

#|
(defclass lisp-object-reference (ns:ns-object)
  ((obj-dict :accessor obj-dict :initarg :obj-dict))
  (:default-initargs :obj-dict nil)
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self lisp-object-reference) &key obj-dict &allow-other-keys)
  (when *debug-convert*
    (ns-log-format "Initializing lisp-object-reference for ~s" self))
  (when (and obj-dict (ccl::objc-object-p obj-dict))
    (#/retain obj-dict)))

(objc:defmethod (#/dealloc :void)
                ((self lisp-object-reference))
  (with-slots (obj-dict) self
    (when (and obj-dict (ccl::objc-object-p obj-dict))
    (#/release obj-dict)))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc:defmethod (#/initWithCoder: :id)
                ((self lisp-object-reference) (decoder :id))
  (setf (obj-dict self) (#/retain (#/decodeObjectForKey: decoder #@"obj-dict")))
  self)

(objc:defmethod (#/encodeWithCoder: :void)
                ((self lisp-object-reference) (coder :id))
  (#/encodeObject:forKey: coder
                          (obj-dict self)
                          #@"obj-dict"))

(defun instance-to-ref (obj)
  (when *debug-convert*
    (ns-log-format "Finding or creating lisp-object-reference for ~s" obj))
  (or (converted-object obj)
      (let* ((slots (archive-slots obj))
             (obj-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary (1+ (* 2 (list-length slots)))))
             (obj-ref (make-instance 'lisp-object-reference :obj-dict obj-dict)))
        (note-converted-object obj obj-ref)
        (#/setObject:forKey: obj-dict
                             (lisp-to-ns-object (class-name (class-of obj)))
                             #@"__instance-class__")
        (dolist (slot slots)
          (let ((slot-str (string slot))
                (slot-val (slot-value obj slot)))
            (#/setObject:forKey: obj-dict
                                 (lisp-to-ns-object (class-name (class-of slot-val)))
                                 (lisp-to-temp-nsstring (concatenate 'string 
                                                                     slot-str
                                                                     "__class__")))
            (#/setObject:forKey: obj-dict
                                 (lisp-to-ns-object slot-val :archive)
                                 (lisp-to-temp-nsstring slot-str))))
        (#/autorelease obj-ref))))

(defmethod std-instance-to-objc ((obj standard-object))
  ;; Default method for converting an object instance to an Objective-C dictionary object.
  ;; This is used to create objects that can be converted and restored by normal Objective-C 
  ;; methods such as encodeWithCoder: and initWithCoder:.
  ;; Classes can override or specialize this in any way they want as long as some Objective-C
  ;; instance is returned.
  (instance-to-ref obj))

(defmethod std-instance-to-objc ((obj structure-object))
  ;; Default method for converting an structure instance to an Objective-C dictionary object.
  ;; This is used to create objects that can be converted and restored by normal Objective-C 
  ;; methods such as encodeWithCoder: and initWithCoder:.
  ;; Classes can override or specialize this in any way they want as long as some Objective-C
  ;; instance is returned.
  (instance-to-ref obj))

(defmethod objc-to-std-instance ((ref lisp-object-reference) &optional new-instance)
  (when *debug-convert*
    (ns-log-format "Converting ~s to std-instance~@[ (using existing ~s)~]" ref new-instance))
  (or (unconverted-object ref nil)
      (let* ((obj-dict (obj-dict ref))
             (class (find-class (ns-to-lisp-object
                                 (#/objectForKey: obj-dict #@"__instance-class__"))
                                nil))
             (inst (and class
                        (if (ccl::subclassp (class-of new-instance) class)
                          ;; if new-instance is specified and is of a compatible type, 
                          ;; then just set the values in its slots from the NSDictionary
                          ;; object. Otherwise create a new instance.
                          new-instance
                          (make-instance class)))))
        (unless class
          (ns-log (format nil
                          "Bad dictionary found in objc-to-std-instance: ~s" obj-dict))
          (alert :text 
           "In objc-to-std-instance, no instance-class found in NSDictionary, returned object will be nil. See console log for more information"))
        (when inst
          (note-unconverted-object ref nil inst)
          (dolist (slot (archive-slots inst) inst)
            (let* ((slot-str (string slot))
                   (slot-class-key-str (lisp-to-temp-nsstring (concatenate 'string
                                                                           slot-str
                                                                           "__class__")))
                   (slot-class-str (ns-to-lisp-object (#/objectForKey: obj-dict 
                                                                       slot-class-key-str)))
                   (slot-class (find-class slot-class-str nil))
                   (objc-slot-val (#/objectForKey: obj-dict (lisp-to-temp-nsstring slot-str)))
                   (slot-val (ns-to-lisp-object 
                              objc-slot-val
                              :lisp-class slot-class)))
              (cond ((eq slot-val :none)
                     (ns-log (format nil
                                     "Saved value for ~s slot cannot be converted to Lisp. Value: ~s"
                                     slot
                                     objc-slot-val)))
                    ((not (eql objc-slot-val (%null-ptr)))
                     ;; We found an archived value for the slot
                     (setf (slot-value inst slot) slot-val))
                    (t
                     ;; There is now a slot in the object that wasn't archived in the saved version of
                     ;; this instance. We just let the default initial value take care of it and do
                     ;; nothing here.
                     nil))))))))

(defun instance-hash-table-p (ns-dict)
  ;; ns-dict must be an NSDictionary object
  ;; check to see if it is an encoded standard-instance
  (not (eql (%null-ptr) (#/objectForKey: ns-dict #@"__instance-class__"))))

(defun arch-slots (obj)
  ;; by default return a list of all non-foreign slots
  ;; object classes that do not want to archive all such slots may 
  ;; override this function for their class
  (mapcar #'ccl::slot-definition-name
          (remove-if #'(lambda (eslot)
                         (subtypep (type-of eslot) 'ccl::foreign-effective-slot-definition))
                     (class-slots (class-of obj)))))

(defmethod archive-slots ((obj standard-object))
  ;; by default return a list of all non-foreign slots
  ;; object classes that do not want to archive all such slots may 
  ;; override this function for their class
  (arch-slots obj))

(defmethod archive-slots ((obj structure-object))
  ;; by default return a list of all non-foreign slots
  ;; object classes that do not want to archive all such slots may 
  ;; override this function for their class
  (arch-slots obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with NSArray objects

(defmacro do-objc-array ((arr-elt arr &optional (return t)) &rest body)
  (let ((arr-indx (gensym)))
    `(dotimes (,arr-indx (#/count ,arr) ,return)
       (let ((,arr-elt (#/objectAtIndex: ,arr ,arr-indx)))
         ,@body))))

(defmethod ns-to-lisp-array ((ns-arr ns:ns-array) &key (element-class nil))
  (let ((new-arr (make-array (list 0) :adjustable t :fill-pointer t)))
    (do-objc-array (elt ns-arr new-arr)
       (vector-push-extend (if (ccl::subclassp element-class ns:ns-object)
                             elt
                             (ns-to-lisp-object elt
                                                :lisp-class element-class))
                           new-arr))))

(defmethod ns-to-lisp-list ((ns-arr ns:ns-array) &key (element-class nil))
  (let ((new-list nil))
    (do-objc-array (elt ns-arr (nreverse new-list))
       (setf new-list 
              (cons (if (ccl::subclassp element-class ns:ns-object)
                      elt
                      (ns-to-lisp-object elt
                                         :lisp-class element-class))
                    new-list)))))

(defmethod lisp-to-ns-array ((lst list))
  (let ((new-arr (#/arrayWithCapacity: ns:ns-mutable-array (list-length lst)))
        (count -1))
    (dolist (item lst new-arr)
      (#/insertObject:atIndex: new-arr
                               (lisp-to-ns-object item)
                               (incf count)))))

(defmethod lisp-to-ns-array ((arr array))
  (let* ((max-count (if (array-has-fill-pointer-p arr)
                     (fill-pointer arr)
                     (length arr)))
         (new-arr (#/arrayWithCapacity: ns:ns-mutable-array max-count)))
    (do* ((count 0 (1+ count)))
         ((>= count max-count) new-arr)
      (#/insertObject:atIndex: new-arr
                               (lisp-to-ns-object (aref arr count))
                               count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with NSDictionary objects

(defmethod ns-to-lisp-hash-table ((dict ns:ns-dictionary) &key (element-class nil) (test nil))
  (let* ((dict-keys (ns-to-lisp-list (#/allKeys dict)
                                                     :element-class ns:ns-object))
         (ts (#/objectForKey: dict (lisp-to-ns-object "hash-table-size")))
         (tab-size (if (%null-ptr-p ts) 60 (ns-to-lisp-object ts)))
         (tt (#/objectForKey: dict (lisp-to-ns-object "hash-table-test")))
         (tab-test (or test (if (%null-ptr-p tt) 'eql (ns-to-lisp-object tt))))
         (trs (#/objectForKey: dict (lisp-to-ns-object "hash-table-rehash-size")))
         (tab-rehash-size (if (%null-ptr-p trs) 1.5 (ns-to-lisp-object trs)))
         (trt (#/objectForKey: dict (lisp-to-ns-object "hash-table-rehash-threshold")))
         (tab-rehash-threshold  (if (%null-ptr-p trt) 0.85 (ns-to-lisp-object trt)))
         (ht (make-hash-table :test tab-test :size tab-size :rehash-size tab-rehash-size :rehash-threshold tab-rehash-threshold)))
    (dolist (key dict-keys ht)
      (let ((lisp-key (ns-to-lisp-object key)))
        (unless (and (stringp lisp-key) (member lisp-key
                                                (list "hash-table-size" 
                                                      "hash-table-test"
                                                      "hash-table-rehash-size"
                                                      "hash-table-rehash-threshold")
                                                :test #'string=))
          (setf (gethash lisp-key ht)
                (if (ccl::subclassp element-class ns:ns-object)
                  (#/objectForKey: dict key)
                  (ns-to-lisp-object (#/objectForKey: dict key)
                                     :lisp-class element-class))))))))

(defmethod ns-to-lisp-assoc ((dict ns:ns-dictionary) &key (element-class nil))
  (let ((assoc-lst nil)
        (dict-keys (ns-to-lisp-list (#/allKeys dict)
                                    :element-class ns:ns-object)))
    (dolist (key dict-keys assoc-lst)
      (let ((lisp-key (ns-to-lisp-object key)))
        (unless (and (stringp lisp-key) (member lisp-key
                                                (list "hash-table-size" 
                                                      "hash-table-test"
                                                      "hash-table-rehash-size"
                                                      "hash-table-rehash-threshold")
                                                :test #'string=))
          (setf assoc-lst
            (acons lisp-key
                   (if (ccl::subclassp element-class ns:ns-object)
                     (#/objectForKey: dict key)
                     (ns-to-lisp-object (#/objectForKey: dict key)
                                        :lisp-class element-class))
                   assoc-lst)))))))

(defmethod lisp-to-ns-plist-dict ((ht hash-table))
  ;; arrays put in info.plist files have become picky about allowing any fields that
  ;; are not expected, so we can't add extraneous fields to support hash table attributes
  ;; because the write to the info.plist file will fail. So this routing does a vanilla
  ;; encoding of any hash-table that we used in lisp that will end up as part of an info
  ;; plist file. That includes any sub-fields.
  (let* ((count (hash-table-count ht))
         (new-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary count))
         (*minimal-hash-table-encoding* t))
    (maphash #'(lambda (key val)
                 (#/setObject:forKey: new-dict 
                                      (lisp-to-ns-object val)
                                      (lisp-to-ns-object key)))
             ht)
    new-dict))

(defmethod lisp-to-ns-dict ((alist list))
  ;; alist must be in the form of an association list
  (let* ((count (list-length alist))
         (new-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary count)))
    (dolist (pair alist new-dict)
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object (cdr pair))
                           (lisp-to-ns-object (car pair))))))

(defmethod lisp-to-ns-dict ((ht hash-table))
  (let* ((count (hash-table-count ht))
         (new-dict (#/dictionaryWithCapacity: ns:ns-mutable-dictionary (+ count 4))))
    (maphash #'(lambda (key val)
                 (#/setObject:forKey: new-dict 
                                      (lisp-to-ns-object val)
                                      (lisp-to-ns-object key)))
             ht)
    (unless *minimal-hash-table-encoding*
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object (hash-table-size ht))
                           (lisp-to-ns-object "hash-table-size"))
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object (hash-table-test ht))
                           (lisp-to-ns-object "hash-table-test"))
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object (hash-table-rehash-size ht))
                           (lisp-to-ns-object "hash-table-rehash-size"))
      (#/setObject:forKey: new-dict 
                           (lisp-to-ns-object (hash-table-rehash-threshold ht))
                           (lisp-to-ns-object "hash-table-rehash-threshold")))
    new-dict))

(deftype objc-displayable () 
  '(or string
       (and atom 
            (not sequence)
            (not hash-table)
            (not package) 
            (not pathname)
            (not random-state)
            (not readtable)
            (not array)
            (not stream)
            (not class)
            (not structure-object)
            (not standard-object)
            (not macptr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with conversion of lisp symbols
;;
;; Initially I implemented this as a concrete subclass of NSString, but it turned out that
;; my #/initWithCoder: and #/encodeWithCoder: functions were never called when one of these
;; was converted. It just encoded them as NSMutableStrings. So there is more than one way to 
;; skin a cat ...

#|
(defclass ns-sym (ns:ns-object)
  ((sym :accessor sym :initarg :sym)
   (ns-str :accessor ns-str)
   (sym-name :accessor sym-name)
   (sym-package :accessor sym-package))
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self ns-sym) &key sym &allow-other-keys)
  (setf (sym-name self) (symbol-name sym))
  (setf (sym-package self) (symbol-package sym))
  (setf (ns-str self) (ccl::%make-nsstring (format nil
                                                   "~a:~a"
                                                   (package-name (sym-package self))
                                                   (sym-name self)))))

(defmethod print-object ((self ns-sym) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "~s" (sym self))))

(objc:defmethod (#/dealloc :void)
                ((self ns-sym))
  (#/release (ns-str self))
  (call-next-method)
  (objc:remove-lisp-slots self))

;; This method suffices to make an ns-sym act like an NSString if the runtime is 10.6 or higher
(objc:defmethod (#/forwardingTargetForSelector: :id)
                ((self ns-sym) (sel #>SEL))
  (ns-str self))

;; Otherwise the following two methods are needed

(objc:defmethod (#/methodSignatureForSelector: :id)
                ((self ns-sym) (sel #>SEL))
  (#/methodSignatureForSelector: (ns-str self) sel))

(objc:defmethod (#/forwardInvocation: :void)
                ((self ns-sym) (inv :id))
  (#/invokeWithTarget: inv (ns-str self)))

(objc:defmethod (#/initWithCoder: :id)
                ((self ns-sym) (decoder :id))
  (let* ((sym-name (#/decodeObjectForKey: decoder #@"symName"))
         (sym-pkg (#/decodeObjectForKey: decoder #@"symPkg"))
         (pkg-str (ns-to-lisp-string sym-pkg)))
    (setf (sym-name self) (ns-to-lisp-string sym-name))
    (setf (sym-package self) (or (find-package pkg-str)
                                 (make-package pkg-str)))
    (setf (sym self) (intern (sym-name self) (sym-package self)))
    (setf (ns-str self) (ccl::%make-nsstring (format nil
                                                     "~@[~a:~]~a"
                                                     (package-name (sym-package self))
                                                     (sym-name self))))
    self))

(objc:defmethod (#/encodeWithCoder: :void)
                ((self ns-sym) (coder :id))
  (#/encodeObject:forKey: coder
                          (lisp-to-temp-nsstring (sym-name self))
                          #@"symName")
  (#/encodeObject:forKey: coder 
                          (lisp-to-temp-nsstring (package-name (sym-package self)))
                          #@"symPkg"))

(defmethod lisp-to-ns-sym ((sym symbol))
  (#/autorelease (make-instance 'ns-sym :sym sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with conversion of lisp functions
;;
;; We save the function name so that it can be reconstituted when loaded back

#|
(defclass ns-func (ns:ns-object)
  ((func :accessor func :initarg :func)
   (ns-str :accessor ns-str)
   (func-name :accessor func-name)
   (func-package :accessor func-package))
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self ns-func) &key func &allow-other-keys)
  (setf (func-name self) (function-name func))
  (setf (func-package self) (symbol-package (func-name self)))
  (setf (ns-str self) (ccl::%make-nsstring (print-object self nil))))

(defmethod print-object ((self ns-func) strm)
  (format strm "(function ~a::~a)" (package-name (func-package self)) (func-name self)))

(objc:defmethod (#/dealloc :void)
                ((self ns-func))
  (#/release (ns-str self))
  (call-next-method)
  (objc:remove-lisp-slots self))

;; This method suffices to make an ns-func act like an NSString if the runtime is 10.6 or higher
(objc:defmethod (#/forwardingTargetForSelector: :id)
                ((self ns-func) (sel #>SEL))
  (ns-str self))

;; Otherwise the following two methods are needed

(objc:defmethod (#/methodSignatureForSelector: :id)
                ((self ns-func) (sel #>SEL))
  (#/methodSignatureForSelector: (ns-str self) sel))

(objc:defmethod (#/forwardInvocation: :void)
                ((self ns-func) (inv :id))
  (#/invokeWithTarget: inv (ns-str self)))

(objc:defmethod (#/initWithCoder: :id)
                ((self ns-func) (decoder :id))
  (let* ((func-name (#/decodeObjectForKey: decoder #@"funcName"))
         (func-pkg (#/decodeObjectForKey: decoder #@"funcPkg"))
         (pkg-str (ns-to-lisp-string func-pkg)))
    (setf (func-name self) (ns-to-lisp-string func-name))
    (setf (func-package self) (or (find-package pkg-str)
                                  (make-package pkg-str)))
    (setf (func self) (symbol-function (intern (func-name self) (func-package self))))
    (setf (ns-str self) (ccl::%make-nsstring (print-object self nil)))
    self))

(objc:defmethod (#/encodeWithCoder: :void)
                ((self ns-func) (coder :id))
  (#/encodeObject:forKey: coder
                          (lisp-to-temp-nsstring (func-name self))
                          #@"funcName")
  (#/encodeObject:forKey: coder 
                          (lisp-to-temp-nsstring (package-name (func-package self)))
                          #@"funcPkg"))

(defmethod lisp-to-ns-func ((func function))
  (#/autorelease (make-instance 'ns-func :func func)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods dealing with conversion of miscellaneous lisp values that hopefully can
;; be printed to and read from strings

#|
(defclass ns-misc (ns:ns-object)
  ((obj :accessor obj :initarg :obj)
   (obj-type :accessor obj-type)
   (obj-str :accessor obj-str)
   (ns-str :accessor ns-str))
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self ns-misc) &key obj &allow-other-keys)
  (setf (obj-str self) (format nil "~s" obj))
  (setf (obj-type self) (type-of obj))
  (setf (ns-str self) (ccl::%make-nsstring (obj-str self))))

(defmethod print-object ((self ns-misc) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "~s" (obj-str self))))

(objc:defmethod (#/dealloc :void)
                ((self ns-misc))
  (#/release (ns-str self))
  (call-next-method)
  (objc:remove-lisp-slots self))

;; This method suffices to make an ns-misc act like an NSString if the runtime is 10.6 or higher
(objc:defmethod (#/forwardingTargetForSelector: :id)
                ((self ns-misc) (sel #>SEL))
  (ns-str self))

;; Otherwise the following two methods are needed

(objc:defmethod (#/methodSignatureForSelector: :id)
                ((self ns-misc) (sel #>SEL))
  (#/methodSignatureForSelector: (ns-str self) sel))

(objc:defmethod (#/forwardInvocation: :void)
                ((self ns-misc) (inv :id))
  (#/invokeWithTarget: inv (ns-str self)))

(objc:defmethod (#/initWithCoder: :id)
                ((self ns-misc) (decoder :id))
  (let* ((obj-type (coerce-obj (#/decodeObjectForKey: decoder #@"objType") t))
         (obj-str (#/decodeObjectForKey: decoder #@"objString"))
         (obj (when (member obj-type (list 'ns:ns-rect 'ns:ns-size 'ns:ns-point))
                (coerce-obj (coerce-obj (#/decodeObjectForKey: decoder #@"objAsList") 'list) obj-type))))
    (setf (obj-str self) (ns-to-lisp-string obj-str))
    (setf (obj self) (or obj
                         (if (unreadable-object-string-p (obj-str self))
                           nil
                           (read-from-string (obj-str self) nil nil))))
    (setf (ns-str self) (#/retain obj-str))
    self))

(objc:defmethod (#/encodeWithCoder: :void)
                ((self ns-misc) (coder :id))
  (let ((typ (obj-type self)))
    (#/encodeObject:forKey: coder
                            (lisp-to-ns-sym typ)
                            #@"objType")
    (#/encodeObject:forKey: coder
                            (ns-str self)
                            #@"objString")
    (when (member typ (list 'ns:ns-rect 'ns:ns-size 'ns:ns-point))
      (#/encodeObject:forKey: coder
                              (coerce-obj (coerce-obj (obj self) 'list) 'ns:ns-array)
                              #@"objAsList"))))

(defmethod lisp-to-ns-misc (obj)
  (make-instance 'ns-misc :obj obj))

;; methods dealing with NSSize and NSRect

(defmethod equal-size-p ((sz1 ns:ns-size) (sz2 ns:ns-size))
  (and (eql (ns:ns-size-width sz1) (ns:ns-size-width sz2))
       (eql (ns:ns-size-height sz1) (ns:ns-size-height sz2))))

(defmethod equal-size-p ((sz1 ns:ns-rect) (sz2 ns:ns-rect))
  (and (eql (ns:ns-rect-width sz1) (ns:ns-rect-width sz2))
       (eql (ns:ns-rect-height sz1) (ns:ns-rect-height sz2))))

;; macro to force actions to happen on the main thread

(defmacro on-main-thread (&rest actions)
  `(ccl::call-in-event-process
     #'(lambda ()
         ,@actions)))

(provide :ns-object-utils)