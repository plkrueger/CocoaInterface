;; coerce-obj

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

Implements a variety of coerce-obj methods that provide an easier-to-use interface to various
conversion functions that involve Objective-C objects.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :ns-object-utils))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assoc-list type definition

(defun all-dotted-pairs (lst)
  (and (listp lst)
       (every #'consp lst)))

(deftype assoc-list () '(and list (satisfies all-dotted-pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic coerce-obj method

(defgeneric coerce-obj (obj type &key ns-format &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error free coerce-obj method

(defmacro on-error-return ((err-return) &rest forms)
  `(handler-case (progn ,@forms)
     (error (c) ,err-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special purpose methods

(defmethod coerce-obj ((obj ccl::macptr) (type (eql 'ns:ns-object)) &key (ns-format nil) &allow-other-keys)
  (declare (ignore type))
  (iu::lisp-to-ns-object obj ns-format))

(defmethod coerce-obj ((obj ccl::macptr) type &key &allow-other-keys)
  (declare (ignore type))
  (cond ((eql obj (ccl::%null-ptr))
         nil)
        ((typep obj 'ns:ns-object)
         (iu::ns-to-lisp-object obj))
        (t
         obj)))

(defmethod coerce-obj ((str string) (sel (eql :SEL)) &key &allow-other-keys)
  (declare (ignore sel))
  ;; create a selector from the string
  (ccl::%get-selector (ccl::load-objc-selector str)))

(defmethod coerce-obj ((obj ns:ns-object) type &key (ns-format nil))
  ;; This could get called with type being a lisp-type for which there is no explicit coerce-obj method, 
  ;; in which case we'll create a default lisp object.
  ;; Or it could be called to convert an ns-object to another ns-object (or perhaps to the same type.
  (if (and  (subtypep type 'ns:ns-object)
            (subtypep (type-of obj) type))
      obj
      (let ((converted-obj (iu::ns-to-lisp-object obj
                                                  :lisp-class (if (eq type t) nil (find-class type nil))
                                                  :ns-format ns-format)))
        (cond ((typep converted-obj type)
               converted-obj)
              ((eq converted-obj obj)
               ;; ns-to-lisp-object just returned what we sent
               (error "~s cannot be coerced to ~s" obj type))
              (t
               (coerce-obj converted-obj type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods that do additional forms of coercion between normal lisp objects

(defmethod coerce-obj (obj type &key (ns-format nil) &allow-other-keys)
  ;; If no other specialized type, just call coerce
  (if (subtypep type 'ns:ns-object)
    (let ((converted-obj (iu::lisp-to-ns-object obj ns-format)))
      (if (typep converted-obj type)
        converted-obj
        (error "~s cannot be coerced to ~s" obj type)))
    (coerce obj type)))

(defmethod coerce-obj ((str string) (sym (eql 'symbol)) &key &allow-other-keys)
  (iu::string-to-interned-symbol str))

(defmethod coerce-obj ((str string) (cl (eql 'class)) &key &allow-other-keys)
  ;; returns nil if coercion is not possible rather than causing an error
  (iu::nsstring-to-class str))

(defmethod coerce-obj ((str string) (cl (eql 'function)) &key &allow-other-keys)
  ;; returns nil if coercion is not possible rather than causing an error
  (iu::find-func str))

(defmethod coerce-obj ((str string) (cl (eql :lisp-date)) &key &allow-other-keys)
  ;; returns nil if coercion is not possible rather than causing an error
  (iu::string-to-date str))

(defmethod coerce-obj ((str string) (cl (eql 'iu::lisp-date)) &key &allow-other-keys)
  ;; returns nil if coercion is not possible rather than causing an error
  (iu::string-to-date str))

(defmethod coerce-obj (obj (cl (eql 'string)) &key &allow-other-keys)
  ;; anything can be coerced to a string just by printing it
  (format nil "~s" (if (typep obj 'ns:ns-object)
                     (iu::ns-to-lisp-object obj)
                     obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods that coerce ns-object objects to other ns-object types

(defmethod coerce-obj ((obj ns:ns-string) (type (eql 'ns:ns-attributed-string)) &key &allow-other-keys)
  (declare (ignore type))
  (#/autorelease (#/initWithString: (#/alloc ns:ns-attributed-string) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods that coerce ns-object objects to lisp types

(defmethod coerce-obj ((obj ns:ns-array) (lisp-type (eql 'array)) &key (element-class nil) &allow-other-keys)
  ;; turns an ns:ns-array object into an adjustable length 1-dimensional array
  (iu::ns-to-lisp-array obj :element-class element-class))

(defmethod coerce-obj ((obj ns:ns-array) (lisp-type (eql 'list)) &key  (element-class nil) &allow-other-keys)
  ;; turns an ns:ns-array object into a list
  (iu::ns-to-lisp-list obj :element-class element-class))

(defmethod coerce-obj ((obj ns:ns-dictionary) (lisp-type (eql 'hash-table)) &key (element-class nil) &allow-other-keys)
  ;; turns a ns:ns-dictionary into a hash-table
  (iu::ns-to-lisp-hash-table obj :element-class element-class))

(defmethod coerce-obj ((obj ns:ns-dictionary) (lisp-type (eql 'assoc-list)) &key (element-class nil) &allow-other-keys)
  ;; turns a ns:ns-dictionary into an assoc-list
  (iu::ns-to-lisp-assoc obj :element-class element-class))

(defmethod coerce-obj ((obj ns:ns-string) (lisp-type (eql 'string)) &key &allow-other-keys)
  (iu::ns-to-lisp-string obj))

(defmethod coerce-obj ((obj ns:ns-attributed-string) (lisp-type (eql 'string)) &key &allow-other-keys)
  (iu::ns-attrib-to-lisp-string obj))

(defmethod coerce-obj ((obj ns:ns-attributed-string) 
                       (lisp-type (eql 'iu::attributed-string))
                       &key &allow-other-keys)
  (make-instance 'iu::attributed-string :ns-str obj))

(defmethod coerce-obj ((obj ns:ns-attributed-string) lisp-type &key &allow-other-keys)
  (iu::ns-to-lisp-object obj :lisp-class (find-class lisp-type nil)))

(defmethod coerce-obj ((obj ns:ns-data) (lisp-type (eql 'string)) &key &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::ns-data-to-lisp-str obj))

(defmethod coerce-obj ((obj ns:ns-data) lisp-type &key &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::ns-data-to-lisp-object obj))

(defmethod coerce-obj ((obj ns:ns-string) (lisp-type (eql 'class)) &key &allow-other-keys)
  (iu::nsstring-to-class obj))

(defmethod coerce-obj ((obj ns:ns-string) (lisp-type (eql 'function)) &key &allow-other-keys)
  (iu::nsstring-to-func obj))

(defmethod coerce-obj ((obj ns:ns-string) (lisp-type (eql 'symbol)) &key &allow-other-keys)
  (iu::nsstring-to-sym obj))

(defmethod coerce-obj ((obj ns:ns-date) lisp-type &key &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::ns-to-lisp-date obj))

(defmethod coerce-obj ((obj ns:ns-date) (lisp-type (eql 'string)) &key &allow-other-keys)
  (iu::date-string (iu::ns-to-lisp-date obj)))

(defmethod coerce-obj ((obj ns:ns-decimal-number) (lisp-type (eql 'float)) &key &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::float-from-ns-decimal obj))

(defmethod coerce-obj ((obj ns:ns-decimal-number) lisp-type &key (decimals 2) &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::lisp-from-ns-decimal obj :decimals decimals))

(defmethod coerce-obj ((obj iu::lisp-object-reference) lisp-type &key &allow-other-keys)
  (declare (ignore lisp-type))
  (iu::while-unconverting
   (iu::objc-to-std-instance obj)))

(defmethod coerce-obj ((obj ns:ns-point)  (lisp-type (eql 'list)) &key &allow-other-keys)
  (list (ns:ns-point-x obj) (ns:ns-point-y obj)))

(defmethod coerce-obj ((obj ns:ns-point)  (lisp-type (eql 'cons)) &key &allow-other-keys)
  (cons (ns:ns-point-x obj) (ns:ns-point-y obj)))

(defmethod coerce-obj ((obj ns:ns-point)  (lisp-type (eql :values)) &key &allow-other-keys)
  (values (ns:ns-point-x obj) (ns:ns-point-y obj)))

(defmethod coerce-obj ((obj ns:ns-size)  (lisp-type (eql 'list)) &key &allow-other-keys)
  (list (ns:ns-size-width obj) (ns:ns-size-height obj)))

(defmethod coerce-obj ((obj ns:ns-size)  (lisp-type (eql 'cons)) &key &allow-other-keys)
  (cons (ns:ns-size-width obj) (ns:ns-size-height obj)))

(defmethod coerce-obj ((obj ns:ns-size)  (lisp-type (eql :values)) &key &allow-other-keys)
  (values (ns:ns-size-width obj) (ns:ns-size-height obj)))

(defmethod coerce-obj ((obj ns:ns-rect)  (lisp-type (eql 'list)) &key (size 4) &allow-other-keys)
  (if (= size 2)
      (list (cons (ns:ns-rect-x obj) (ns:ns-rect-y obj)) (cons (ns:ns-rect-width obj) (ns:ns-rect-height obj)))
      (list (ns:ns-rect-x obj) (ns:ns-rect-y obj) (ns:ns-rect-width obj) (ns:ns-rect-height obj))))

(defmethod coerce-obj ((obj ns:ns-rect)  (lisp-type (eql :values)) &key &allow-other-keys)
  (values (ns:ns-rect-x obj) (ns:ns-rect-y obj) (ns:ns-rect-width obj) (ns:ns-rect-height obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods that coerce lisp objects to ns-object types

(defmethod coerce-obj (obj (type (eql 'iu::lisp-ptr-wrapper)) &key &allow-other-keys)
  (iu::wrapper-for obj))

(defmethod coerce-obj ((obj ccl::macptr) (type (eql 'iu::lisp-ptr-wrapper)) &key &allow-other-keys)
  ;; This will be called in preference to the method that converts macptrs to general lisp objects
  (iu::wrapper-for obj))

(defmethod coerce-obj (obj (type (eql 'ns:ns-object)) &key (ns-format nil))
  (iu::lisp-to-ns-object obj ns-format))

(defmethod coerce-obj ((obj list) (type (eql 'ns:ns-array)) &key &allow-other-keys)
  (iu::lisp-to-ns-array obj))

(defmethod coerce-obj ((obj array) (type (eql 'ns:ns-array)) &key &allow-other-keys)
  (iu::lisp-to-ns-array obj))

(defmethod coerce-obj ((obj list) (type (eql 'ns:ns-dictionary)) &key &allow-other-keys)
  (if (typep obj 'assoc-list)
    (iu::lisp-to-ns-dict obj)
    (error "~a is not an assoc-list" obj)))

(defmethod coerce-obj ((obj hash-table) (type (eql 'ns:ns-dictionary)) &key &allow-other-keys)
  (iu::lisp-to-ns-dict obj))

(defmethod coerce-obj ((obj string) (type (eql 'ns:ns-data)) &key &allow-other-keys)
  (iu::lisp-str-to-ns-data obj))

(defmethod coerce-obj (obj (type (eql 'ns:ns-data)) &key &allow-other-keys)
  (iu::lisp-object-to-ns-data obj))

(defmethod coerce-obj ((obj string) (type (eql 'ns:ns-string)) &key &allow-other-keys)
  (iu::lisp-to-temp-nsstring obj))

(defmethod coerce-obj (obj (type (eql 'ns:ns-date)) &key &allow-other-keys)
  (iu::lisp-to-ns-date obj))

(defmethod coerce-obj ((obj string) (type (eql 'ns:ns-date)) &key &allow-other-keys)
  (iu::string-to-ns-date obj))

(defmethod coerce-obj ((obj number) (type (eql 'ns:ns-decimal-number)) &key (decimals 2) &allow-other-keys)
  (iu::lisp-to-ns-decimal obj :decimals decimals))

(defmethod coerce-obj ((obj standard-object) (type (eql 'ns:ns-object)) &key &allow-other-keys)
  (iu::while-converting
   (iu::std-instance-to-objc obj)))

(defmethod coerce-obj ((obj structure-object) (type (eql 'ns:ns-object)) &key &allow-other-keys)
  (iu::while-converting
   (iu::std-instance-to-objc obj)))

(defmethod coerce-obj ((obj iu::attributed-string) (type (eql 'ns:ns-string)) &key &allow-other-keys)
  (#/string (iu::att-ns-str obj)))

(defmethod coerce-obj ((obj iu::attributed-string) type &key (ns-format nil) &allow-other-keys)
  (declare (ignore type))
  (iu::lisp-to-ns-object obj ns-format))

(defmethod coerce-obj ((obj string) (type (eql 'ns:ns-attributed-string)) &key &allow-other-keys)
  (declare (ignore type))
  (iu::lisp-to-ns-object obj :rich-text))

(defmethod coerce-obj ((obj cons) (type (eql 'ns:ns-point)) &key &allow-other-keys)
  (declare (ignore type))
  (ns:make-ns-point (or (car obj) 0)
                    (or (if (atom (cdr obj))
                            (cdr obj)
                            (second obj))
                        0)))

(defmethod coerce-obj ((obj cons) (type (eql 'ns:ns-size)) &key &allow-other-keys)
  ;; works for (x y) or (x . y)
  (declare (ignore type))
  (ns:make-ns-size (or (first obj) 0)
                   (or (if (atom (cdr obj))
                           (cdr obj)
                           (second obj))
                       0)))

(defmethod coerce-obj ((obj list) (type (eql 'ns:ns-rect)) &key &allow-other-keys)
  (declare (ignore type))
  (if (= (list-length obj) 2)
      ;; have two point representation which may be either (x y) or (x . y)
      (if (and (atom (cdar obj)) (atom (cdadr obj)))
          (ns:make-ns-rect (or (caar obj) 0) (or (cdar obj) 0) (or (caadr obj) 0) (or (cdadr obj) 0))
          (ns:make-ns-rect (or (caar obj) 0) (or (cadar obj) 0) (or (caadr obj) 0) (or (cadadr obj) 0)))
      (ns:make-ns-rect (or (first obj) 0) (or (second obj) 0) (or (third obj) 0) (or (fourth obj) 0))))

(defun random-color ()
  (let ((red (random (gui::cgfloat 1.0)))
        (green (random (gui::cgfloat 1.0)))
        (blue (random (gui::cgfloat 1.0)))
        (alpha (+ (gui::cgfloat 0.5) (random (gui::cgfloat 0.5)))))
    (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                                red
                                                green
                                                blue
                                                alpha)))

(defmethod coerce-obj ((obj keyword) (type (eql 'ns:ns-color)) &key &allow-other-keys)
  (declare (ignore type))
  (ecase obj
    (:black (#/blackColor ns:ns-color))
    (:blue (#/blueColor ns:ns-color))
    (:brown (#/brownColor ns:ns-color))
    (:clear (#/clearColor ns:ns-color))
    (:cyan (#/cyanColor ns:ns-color))
    (:dark-gray (#/darkGrayColor ns:ns-color))
    (:gray (#/grayColor ns:ns-color))
    (:green (#/greenColor ns:ns-color))
    (:light-gray (#/lightGrayColor ns:ns-color))
    (:magenta (#/magentaColor ns:ns-color))
    (:orange (#/orangeColor ns:ns-color))
    (:purple (#/purpleColor ns:ns-color))
    (:random (random-color))
    (:red (#/redColor ns:ns-color))
    (:white (#/whiteColor ns:ns-color))
    (:yellow (#/yellowColor ns:ns-color))))
    
(provide :coerce-obj)