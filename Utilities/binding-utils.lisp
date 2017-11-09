;; binding-utils.lisp

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
  (require :lc-classes)
  (require :coerce-obj)
  (require :nslog-utils)
  (require :kvo-slot)
  (require :selector-utils))

(defgeneric lc::modified-bound-value (controller edited-obj key old-val new-val))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous utility functions

;; Given a window and another object, find all the subviews of the window that link to that object
;; for their value and the key paths that they use (returned as a string).

(defmethod linking-views ((win ns:ns-window) (obj ns:ns-object))
  (let* ((cview (#/contentView win))
         (subviews (ns-to-lisp-list (#/subviews cview) :element-class ns:ns-object)))
    (mapcan #'(lambda (v)
                (multiple-value-bind (bound-p path) (bound-to v obj #@"value")
                  (when bound-p
                    (list (cons v path)))))
            subviews)))

;; This looks old and I think assumes a conversion to symbols in ns-to-lisp-assoc
;; that doesn't really happen ...
(defmethod bound-to ((view ns::ns-view) (bound-to-obj ns::ns-object) binding-ns-string)
  (let* ((objc-info (#/infoForBinding: view binding-ns-string))
         (info (and (not (eql objc-info (%null-ptr))) 
                    (ns-to-lisp-assoc objc-info :element-class ns::ns-object)))
         (proxy (cdr (assoc 'CL-USER::NSOBSERVEDOBJECT info)))
         (proxy-desc (and proxy (ns-to-lisp-string (#/description proxy))))
         (bound-to-desc (ns-to-lisp-string (#/description bound-to-obj)))
         (path (and info (ns-to-lisp-string (cdr (assoc 'CL-USER::NSOBSERVEDKEYPATH info))))))
    (if (and info (find-substring bound-to-desc proxy-desc))
      (values t path)
      (values nil nil))))

(defun link-path-components (path-str)
  (if (zerop (length path-str))
    nil
    (do* ((begin 0
                 (1+ end))
          (end (or (position #\. path-str :start (1+ begin))
                   (length path-str))
               (or (position #\. path-str :start (1+ begin))
                   (length path-str)))
          (words (list (subseq path-str begin end))
                 (push (subseq path-str begin end) words)))
         ((>= end (length path-str))
          (mapcar #'objc-to-lisp-keypathname (nreverse words))))))

(defun convert-binding-option (bind-key)
  (ecase bind-key
    (:allows-editing-multiple-values-selection #$NSAllowsEditingMultipleValuesSelectionBindingOption)
    (:allows-null-argument #$NSAllowsNullArgumentBindingOption)
    (:always-presents-application-modal-alerts #$NSAlwaysPresentsApplicationModalAlertsBindingOption)
    (:conditionally-sets-editable #$NSConditionallySetsEditableBindingOption)
    (:conditionally-sets-enabled #$NSConditionallySetsEnabledBindingOption)
    (:conditionally-sets-hidden #$NSConditionallySetsHiddenBindingOption)
    (:continuously-updates-value #$NSContinuouslyUpdatesValueBindingOption)
    (:creates-sort-descriptor #$NSCreatesSortDescriptorBindingOption)
    (:deletes-objects-on-remove #$NSDeletesObjectsOnRemoveBindingsOption)
    (:display-name #$NSDisplayNameBindingOption)
    (:display-pattern #$NSDisplayPatternBindingOption)
    (:content-placement-tag #$NSContentPlacementTagBindingOption)
    (:handles-content-as-compound-value #$NSHandlesContentAsCompoundValueBindingOption)
    (:inserts-null-placeholder #$NSInsertsNullPlaceholderBindingOption)
    (:invokes-separately-with-array-objects #$NSInvokesSeparatelyWithArrayObjectsBindingOption)
    (:multiple-vlaues-placeholder #$NSMultipleValuesPlaceholderBindingOption)
    (:no-selection-placeholder #$NSNoSelectionPlaceholderBindingOption)
    (:not-applicable-placeholder #$NSNotApplicablePlaceholderBindingOption)
    (:null-placeholder #$NSNullPlaceholderBindingOption)
    (:raises-for-not-applicable-keys #$NSRaisesForNotApplicableKeysBindingOption)
    (:predicate-format #$NSPredicateFormatBindingOption)
    (:selector-name #$NSSelectorNameBindingOption)
    (:selects-all-when-setting-content #$NSSelectsAllWhenSettingContentBindingOption)
    (:validates-immediately #$NSValidatesImmediatelyBindingOption)
    (:transformer-name #$NSValueTransformerNameBindingOption)
    (:value-transformer #$NSValueTransformerBindingOption)))

(defun convert-key-val-options (olist)
  ;; olist should be a list of keyword-value pairs where keywords are from the table above
  ;; note that the coerce-obj to ns-dictionary automatically converts values to appropriate
  ;; objective-C objects if needed.
  (do* ((assoc-list nil)
        (lst olist
             (cddr lst))
        (key (convert-binding-option (first lst))
             (convert-binding-option (first lst)))
        (val (second lst)
             (second lst)))
       ((null key) (coerce-obj assoc-list 'ns:ns-dictionary))
    (push (cons key val) assoc-list)))

#|

Constants

NSAllowsEditingMultipleValuesSelectionBindingOption
An NSNumber object containing a Boolean value that determines if the binding allows editing 
when the value represents a multiple selection.

NSAlwaysPresentsApplicationModalAlertsBindingOption
An NSNumber object containing a Boolean value that determines if validation and error alert 
panels displayed as a result of this binding are displayed as application modal alerts. If YES, 
then the alerts are displayed application model, otherwise they are displayed as sheets.

NSAllowsNullArgumentBindingOption
An NSNumber object containing a Boolean value that determines if the argument bindings allows 
passing argument values of nil.

NSConditionallySetsEditableBindingOption
An NSNumber object containing a Boolean value that determines if the editable state of the user 
interface item is automatically configured based on the controller's selection.

NSConditionallySetsEnabledBindingOption
An NSNumber object containing a Boolean value that determines if the enabled state of the user 
interface item is automatically configured based on the controller's selection.

NSConditionallySetsHiddenBindingOption
An NSNumber object containing a Boolean value that determines if the hidden state of the user 
interface item is automatically configured based on the controller's selection.

NSContentPlacementTagBindingOption
An NSNumber object specifying the tag id of the popup menu item to replace with the content of the 
array. This allows you to use a popup menu that contains both static and bindings generated items.

NSContinuouslyUpdatesValueBindingOption
An NSNumber object containing a Boolean value that determines whether the value of the binding 
is updated as edits are made to the user interface item or is updated only when the user interface 
item resigns as the responder.

NSCreatesSortDescriptorBindingOption
An NSNumber object containing a Boolean value that determines if a sort descriptor is created 
for a table column. If this value is NO, then the table column does not allow sorting.

NSDeletesObjectsOnRemoveBindingsOption
An NSNumber object containing a Boolean value that determines if an object is deleted from the 
managed context immediately upon being removed from a relationship.

NSDisplayNameBindingOption
An NSString object containing a human readable string to be displayed for a predicate.

NSDisplayPatternBindingOption
An NSString object that specifies a format string used to construct the final value of a string.

NSHandlesContentAsCompoundValueBindingOption
An NSNumber object containing a Boolean value that determines if the content is treated as 
a compound value.

NSInsertsNullPlaceholderBindingOption
An NSNumber object containing a Boolean value that determines if an additional item which 
represents nil is inserted into a matrix or pop-up menu before the items in the content array.

NSInvokesSeparatelyWithArrayObjectsBindingOption
An NSNumber object containing a Boolean value that determines whether the specified selector 
is invoked with the array as the argument or is invoked repeatedly with each array item as an argument.

NSMultipleValuesPlaceholderBindingOption
An object that is used as a placeholder when the key path of the bound controller returns the 
NSMultipleValuesMarker marker for a binding.

NSNoSelectionPlaceholderBindingOption
An object that is used as a placeholder when the key path of the bound controller returns the 
NSNoSelectionMarker marker for a binding.

NSNotApplicablePlaceholderBindingOption
An object that is used as a placeholder when the key path of the bound controller returns the 
NSNotApplicableMarker marker for a binding.

NSNullPlaceholderBindingOption
An object that is used as a placeholder when the key path of the bound controller returns nil 
for a binding.

NSPredicateFormatBindingOption
An NSString object containing the predicate pattern string for the predicate bindings. Use 
$value to refer to the value in the search field.


NSRaisesForNotApplicableKeysBindingOption
An NSNumber object containing a Boolean value that specifies if an exception is raised when
the binding is bound to a key that is not applicable—for example when an object is not key-value
coding compliant for a key.

NSSelectorNameBindingOption
An NSString object that specifies the method selector invoked by the target binding when the 
user interface item is clicked.

NSSelectsAllWhenSettingContentBindingOption
An NSNumber object containing a Boolean value that specifies if all the items in the array controller 
are selected when the content is set.

NSValidatesImmediatelyBindingOption
An NSNumber object containing a Boolean value that determines if the contents of the binding are 
validated immediately.


NSValueTransformerNameBindingOption
The value for this key is an identifier of a registered NSValueTransformer instance that is applied 
to the bound value.

NSValueTransformerBindingOption
An NSValueTransformer instance that is applied to the bound value.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods used to determine what slots are exposed for binding

(defun make-proto (class)
  (let ((pr (#/alloc class)))
    ;; some classes require initialization to avoid uncatchable errors when getting their #/exposedBindings
    ;; I suspect that they have exposedBindings methods that query related objects or otherwise test the 
    ;; state of the object and without initialization they fail. We can't just call #/init for every class
    ;; because that isn't always valid either.
    (when (some #'(lambda (cl)
                    (ccl::subclassp class cl))
                (list ns:ns-cell ns:ns-text))
      (setf pr (#/init pr)))
    pr))

(let ((proto-hash (make-hash-table)))
  (defmethod proto-for-class ((self objc::objc-class))
    (or (gethash self proto-hash)
        (handler-case (setf (gethash self proto-hash) (make-proto self))
          (condition (c)
             (declare (ignore c))
             (setf (gethash self proto-hash) (proto-for-class ns:ns-object)))))))

(defmethod valid-bindings ((self ns:ns-object))
  (coerce-obj (handler-case (#/exposedBindings self)
                (condition (c)
                  (declare (ignore c))
                  (ns-log-format "Error trying to get #/exposedBings for ~s" self)
                  nil))
              'list))

(defmethod valid-bindings ((self objc::objc-class))
  (coerce-obj (handler-case (#/exposedBindings (proto-for-class self))
                (condition (c)
                  (declare (ignore c))
                  (ns-log-format "Error trying to get #/exposedBings for ~s" self)
                  nil))
              'list))

(defmethod valid-bindings (non-class)
  (declare (ignore non-class))
  nil)

(defmethod valid-binding-p ((self ns:ns-object) property-key-path)
  (find property-key-path (valid-bindings self) :test #'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods used to add Objective-C methods to Objectiv-C objects that have :kvo slot declarations

(let ((kvc-hash (make-hash-table)))

  (defmethod define-kvc-methods ((self class))
    ;; do nothing for non objective-c classes
    )

  (defmethod define-kvc-methods ((self objc:objc-class-object))
    (unless (gethash self kvc-hash nil)
      (setf (gethash self kvc-hash) t)
      (eval `(objc:defmethod (#/valueForUndefinedKey: :id)
                             ((self ,(class-name self)) (key :id))
               (let* ((lisp-key (coerce-obj key 'string))
                      (slot (kvo-slot-for self lisp-key)))
                 (if slot
                   (convert-new-val self (slot-value self (slot-definition-name slot)) lisp-key)
                   (call-next-method key)))))
      (eval `(objc:defmethod (#/setValue:forUndefinedKey: :void)
                             ((self ,(class-name self)) (new-val :id) (key :id))
               (if (kvo-slot-for self (coerce-obj key 'string))
                 (set-value-for-key self new-val key)
                 (call-next-method new-val key))))
      (eval `(objc:defmethod (#/addObserver:forKeyPath:options:context: :void)
                             ((self ,(class-name self))
                              (obs :id)
                              (key-path :id)
                              (options #>NSUInteger)
                              (context :address))
               (set-observer-format self obs key-path)
               (call-next-method obs key-path options context)))))

  
  (defun assure-kvc-methods-defined ()
    (dolist (c (new-kvo-classes))
      (define-kvc-methods c)))

  (defun redefine-all-kvc-methods ()
    (setf kvc-hash (make-hash-table))
    (dolist (c (kvo-classes))
      (define-kvc-methods c)))

) ;; end of methods using kvc-hash         


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main Lisp bind method


(defmethod bind ((self ns:ns-object) property-key-path target-obj key-path &rest key-val-options)
  ;; target-obj must be an objective-c object. property-key-path and key-path are defined as:
  ;; key-path        ::= <path-string> | ( <path-object>*)
  ;; path-object     ::= <path-string> | <lisp-function>
  ;; path-string     ::= "<path-elt-string>{.<path-string>}*
  ;; path-elt-string ::= any legal string for Objective-C keypaths

  ;; Before binding anything, make sure that all Objective-C classes with KVO slots have some methods 
  ;; defined for them.
  (assure-kvc-methods-defined)
  (if (valid-binding-p self property-key-path)
    (progn
      (#/bind:toObject:withKeyPath:options: self
                                            (convert-path-list property-key-path)
                                            (if (typep target-obj 'ns:ns-object)
                                              target-obj
                                              (wrapper-for target-obj))
                                            (convert-path-list key-path)
                                            (if key-val-options
                                              (convert-key-val-options key-val-options)
                                              (%null-ptr)))
      t)
    (progn
      (ns-log-format "Property path ~s is not a valid bind path for ~s"
                     property-key-path
                     self)
      nil)))

#|
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
;; end of definitions with access to kvc-observed assoc-array

|#

(defun reader-selector (str)
  (ccl::%get-selector (ccl::load-objc-selector  str)))

(defun writer-selector (str)
  (ccl::%get-selector (ccl::load-objc-selector (concatenate 'string "set" (string-capitalize str :end 1) ":"))))

(defmethod bound-slot-will-be-modified ((self standard-object) slot-name)
  (declare (ignore slot-name))
  ;; do nothing by default
  )

(defmethod bound-slot-modified ((self standard-object) slot-name)
  (declare (ignore slot-name))
  ;; do nothing by default
  )

(defmethod real-observer (observer)
  ;; observer will be an instance of one of server types of non-public Apple Classes. Examples include
  ;; NSSelectionBinder, NSValueBinder, NSTextValueBinder
  ;; All we want is a pointer to the original observer extracted from that object, but without
  ;; a public API that is a little tricky. But we are trickier. 
  ;; We can see from (#/description ...) results for these objects that they all seem to respond to 
  ;; the #/object message which returns  the original observer. So we test for the object's response
  ;; to that selector and use it to return the original observer.


  (when *log-bindings*
    (ns-log-format "Observer description is: ~a" (coerce-obj (#/description observer) 'string)))
  (if (responds-to-selector observer "object")
    (#/object observer)
    observer))

#|
This is an old method that worked in previous OSX versions
I'm leaving it around in case something like this is ever useful in the future
  ;; We use the #/description of that
  ;; object which provides a substring of the form "Observer: 0x<whatever>" which points to 
  ;; possibly another non-public object of type NSSelectionBinder. We extract the address and
  ;; create a pointer to the NSSelectionbinder. We now use its description to extract its
  ;; "object" slot address, which hopefully points to the original view object that we bound
  ;; to our lisp object. We use that to create a macptr and return it.
  (let* ((desc (ns-to-lisp-string (#/description nskvo)))
         (obs-start (search "Observer: " desc))
         (hex-start (and obs-start (position #\x desc :start (+ obs-start 9))))
         (hex-end (and hex-start (position #\, desc :start hex-start)))
         (hex-val (and hex-end (read-from-string (concatenate 'string "#" (subseq desc hex-start hex-end)) nil 0)))
         (nssb-ptr (and hex-val (ccl::%int-to-ptr hex-val)))
         (nssb-desc (and nssb-ptr (ns-to-lisp-string (#/description nssb-ptr))))
         (obj-start (search "object:" nssb-desc))
         (obj-class-end (and obj-start (position #\: nssb-desc :start (+ obj-start 8))))
         (obj-hex-start (and obj-class-end (position #\x nssb-desc :start obj-class-end)))
         (obj-hex-end (and obj-hex-start (position #\> nssb-desc :start obj-hex-start)))
         (obj-hex-val (and obj-hex-end
                           (read-from-string (concatenate 'string 
                                                          "#"
                                                          (subseq nssb-desc obj-hex-start obj-hex-end))
                                             nil 0))))
    (when obj-hex-val
      (ccl::%int-to-ptr obj-hex-val))))
|#

(defun more-specific-format (fmt1 fmt2)
  (cond ((null fmt2)
         t)
        ((eq fmt1 fmt2)
         nil)
        ((eq fmt2 :rich-text)
         t)
        ((eq fmt1 :rich-text)
         nil)
        ((listp fmt1)  ;; only ns-decimal
         t)
        ((listp fmt2)
         nil)
        (t ;; ??
         t)))

(let ((format-assoc (make-instance 'assoc-array :rank 2 :tests (list #'eql #'equal))))
  ;; used to hold needed Objective-C format information for slots that are observed by
  ;; Objective-C objects that have associated formatter objects from which we can take hints.

  (defun set-format-assoc (path-obj path format)
    (let ((existing-format (format-for path-obj path)))
      (when (more-specific-format format existing-format)
        (setf (assoc-aref format-assoc path-obj path) format))))

  (defun format-for (path-obj path)
    (assoc-aref format-assoc path-obj path))

)

(defmethod convert-new-val ((self lisp-ptr-wrapper) value key-str)
  ;; converts the value to an appropriate Objective-C value depending on the key used to access it
  (let ((kr (key-return key-str)))
    (cond ((eql value (%null-ptr))
           value)
          ((null value)
           (%null-ptr))
          ((member value 
                   (list #$NSNoSelectionMarker
                         #$NSNullPlaceholderBindingOption
                         #$NSNotApplicablePlaceholderBindingOption)
                   :test #'eql)
           value)
          ((eq kr :convert)
           (coerce-obj value 'ns:ns-object :ns-format (format-for (lpw-lisp-ptr self) key-str)))
          ((eq kr :path)
           (if (typep value 'ns:ns-object)
             value
             (wrapper-for value)))
          ((typep value 'lisp-ptr-wrapper)
           value)
          ((eq kr :path)
           (wrapper-for value
                        :controller (lpw-controller self)
                        :parent (lpw-lisp-ptr self)))
          ;; Cases below should only occur if binding was made using native objective-c methods
          ((typep value 'ns:ns-object)
           value)
          ((typep value 'objc-displayable)
           (coerce-obj value 'ns:ns-object :ns-format (format-for (lpw-lisp-ptr self) key-str)))
          (t
           (wrapper-for value
                        :controller (lpw-controller self)
                        :parent (lpw-lisp-ptr self))))))

(defmethod convert-new-val ((self ns:ns-object) value key-str)
  ;; converts the value to an appropriate Objective-C value depending on the key used to access it
  ;; If bindings are made using the lisp bind function, then we will know whether the value should
  ;; be converted to some sort of objective-c type for display or is just an intermediate value on
  ;; a binding path and should be left alone. We make a best guess for bindings made using native
  ;; objective-c calls depending on the type of the object returned. If it can be displayed, we
  ;; convert it. Otherwise we leave it alone (or wrap it, if it's a lisp object).
  (let ((kr (key-return key-str)))
    (cond ((eql value (%null-ptr))
           value)
          ((null value)
           (%null-ptr))
          ((member value 
                   (list #$NSNoSelectionMarker
                         #$NSNullPlaceholderBindingOption
                         #$NSNotApplicablePlaceholderBindingOption)
                   :test #'eql)
           value)
          ;; Above cases apply whether value is a path element or end value
          ((eq kr :convert)
           (coerce-obj value 'ns:ns-object :ns-format (format-for self key-str)))
          ((eq kr :path)
           (if (typep value 'ns:ns-object)
             value
             (wrapper-for value)))
          ;; Cases below should only occur if binding was made using native objective-c methods
          ((typep value 'ns:ns-object)
           value)
          ((typep value 'objc-displayable)
           (coerce-obj value 'ns:ns-object :ns-format (format-for self key-str)))
          (t
           (wrapper-for value)))))

(defun set-observer-format (self obs key-path)
  ;; We use the ns-format as a hint about how the lisp field is
  ;; formatted and convert accordingly when that value is retrieved.
  (let ((observer (real-observer obs))
        (ns-format nil))
    (cond ((typep observer 'ns:ns-control)
           (let* ((cell (#/cell observer))
                  (formatter (#/formatter cell)))
             (cond ((or (typep cell 'ns:ns-date-picker-cell)
                        (typep formatter 'ns:ns-date-formatter))
                    (setf ns-format :date))
                   ((typep formatter 'ns:ns-number-formatter)
                    (cond ((#/generatesDecimalNumbers formatter)
                           (let ((dec-digits (#/maximumFractionDigits formatter)))
                             (setf ns-format (list :decimal dec-digits))))
                          (t
                           (setf ns-format :number))))
                   (t
                    ;; NSControls can always accept NSAttributedStrings
                    (setf ns-format :rich-text)))))
          ((typep observer 'ns:ns-text)
           ;; This is a little tricky becasue NSText objects can bind to either
           ;; an NSAttributedString or to an NSString via separate bindings.
           ;; But we have no real way of knowing which they bound to in IB, so
           ;; we guess based on the value returned by #/isRichText. Also the
           ;; developer gives us a hint by what sort of object is in the slot that
           ;; the interface object is bound to. If that slot has a lisp attributed-string
           ;; object and the field allows rich-text, we'll pass it an NSMutableAttributedString.
           ;; Otherwise it will just get an NSString.
           (setf ns-format (if (#/isRichText observer)
                             :rich-text
                             :text)))
          ((typep observer 'lc::lisp-controller)
           ;; We generally don't want to convert from some lisp format to an Objective-C form and then back.
           ;; Rather we'd like to get a pointer to the original Lisp object. So just create a wrapper for
           ;; the value.
           (setf ns-format :wrapper)))
    (when ns-format
      (set-format-assoc self (ns-to-lisp-string key-path) ns-format))))

(objc:defmethod (#/addObserver:forKeyPath:options:context: :void)
                ((self lisp-ptr-wrapper) (obs :id) (key-path :id) (options #>NSUInteger) (context :address))
  (multiple-value-bind (observer ns-format)
                       (set-observer-format (lpw-lisp-ptr self) obs key-path)
    (when *log-bindings*
      (ns-log-format "~s observed by ~s" (lpw-lisp-ptr self) observer)
      (ns-log-format "Observer data format: ~s" ns-format))
    ;; Any controls that previously observed this field might have
    ;; received misformatted data, so indicate that the data changed
    ;; so they will go get it again. This primarily occurs when a new
    ;; window is open and controls are first observing the field.
    (#/willChangeValueForKey: self key-path)
    (#/didChangeValueForKey: self key-path))
  (call-next-method obs key-path options context))

(objc:defmethod (#/valueForKey: :id)
                ((self lisp-ptr-wrapper) (path :id))
  ;; Treat path as a lisp path as long as it works.
  ;; If it is not valid and the next target is an Objective-C object
  ;; then treat the path as a normal Objective-C Key and return the results
  ;; of calling #/valueForKey: on the target using path as the key.
  (let* ((lisp-str (ns-to-lisp-string path))
         (lisp-path (objc-to-lisp-keypathname lisp-str))
         (func (func-for-keypath lisp-str))
         (ptr-obj (lpw-lisp-ptr self))
         ;; to set next-obj we try 4 ways:
         ;; 1. If the path is one that we generated to represent a Lisp function use it
         ;; 2. If the path is a valid lisp function name use it to access the slot
         ;; 3. If the object is an Objective-C object try calling its #/valueForKey method
         ;; 4. Look for any KVO slots defined for the object with path specified as the KVO
         ;;    accessor and use the lisp function value-for-kvo-key to access that slot
         (next-obj (cond ((and func 
                               (compute-applicable-methods func (list ptr-obj)))
                          (funcall func ptr-obj))
                         ((and (typep lisp-path 'function-name)
                               (fboundp lisp-path)
                               (compute-applicable-methods lisp-path (list ptr-obj)))
                          (funcall lisp-path ptr-obj))
                         ((and (typep ptr-obj 'objc:objc-object)
                               (#/respondsToSelector: ptr-obj (reader-selector lisp-str)))
                          (#/valueForKey: ptr-obj path))
                         ((and (typep ptr-obj 'ccl::kvo-object)
                               (compute-applicable-methods #'ccl::value-for-kvo-key (list ptr-obj lisp-str))
                          (ccl::value-for-kvo-key ptr-obj lisp-str)))
                         (t
                          (%null-ptr)))))
    ;; First track that the path is being observed by somebody
    (note-kvc-observed self (lpw-lisp-ptr self) lisp-path)
    (note-kvc-observed self (lpw-lisp-ptr self) lisp-str)
    (when func
      (note-kvc-observed self (lpw-lisp-ptr self) func))
    (when *log-bindings*
      (ns-log-format "(~s ~s) returned ~s" lisp-path (lpw-lisp-ptr self) next-obj))
    (convert-new-val self next-obj lisp-str)))

(defmethod set-value-for-key (self new-value (path ns:ns-object))
  (let* ((lisp-str (coerce-obj path 'string))
         (lisp-path (objc-to-lisp-keypathname lisp-str))
         (prev-obj (cond ((and (typep lisp-path 'function-name)
                               (fboundp lisp-path))
                          (funcall lisp-path self))
                         ((and (typep self 'objc:objc-object)
                               (#/respondsToSelector: self (reader-selector lisp-str)))
                          (#/valueForKey: self path))
                         (t
                          (ccl::value-for-kvo-key self lisp-str))))
         (prev-class (class-of prev-obj))
         (new-lisp-obj (ns-to-lisp-object new-value :lisp-class prev-class :ns-format (format-for self lisp-str)))
         (setf-func (fboundp (list 'setf lisp-path))))
    (when *log-bindings*
      (ns-log-format "Prev Class: ~s" prev-class))
    (cond (setf-func
           (funcall setf-func new-lisp-obj self))
          ((and (typep self 'objc:objc-object)
                (typep new-lisp-obj 'objc:objc-object)
                (#/respondsToSelector: self (writer-selector lisp-str)))
           (#/setValue:forKey: self new-lisp-obj path))
          (t
           (let* ((found-slot (ccl::kvo-slot-for self lisp-str))
                  (slot-name (and found-slot (ccl::slot-definition-name found-slot))))
             (if found-slot
               (progn
                 (bound-slot-will-be-modified self slot-name)
                 (setf (ccl::value-for-kvo-key self lisp-str) new-lisp-obj)
                 (bound-slot-modified self (ccl::slot-definition-name found-slot)))
               ;; If the setf  failed, log the original condition
               (error "No way to setValue: ~s forKey: ~s for lisp-ptr ~s"
                      new-lisp-obj
                      lisp-str
                      self)))))
    (values new-lisp-obj prev-obj)))

(objc:defmethod (#/setValue:forKey: :void)
                ((self lisp-ptr-wrapper) (new-value :id) (path :id))
  (let ((ptr-obj (lpw-lisp-ptr self))
        (ctrl (lpw-controller self))
        (lisp-path (objc-to-lisp-keypathname (coerce-obj path 'string))))
    (when ptr-obj
      (multiple-value-bind (new-lisp-obj prev-obj)
                           (set-value-for-key ptr-obj new-value path)
        (when ctrl
          (lc::modified-bound-value ctrl
                                    (lpw-lisp-ptr self) 
                                    lisp-path
                                    prev-obj
                                    new-lisp-obj))
        new-lisp-obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods to observe kvo-compliant paths through and to either Objective-C or Lisp instances

#|
(defclass kvo-observer (ns:ns-object)
  ((observed-instance :accessor observed-instance
                      :initarg :obj)
   (observe-path :accessor observe-path
                 :initarg :path)
   (when-observed-func :accessor when-observed-func
                       :initarg :func)
   (convert-type :accessor convert-type
                 :initarg :convert-type))
  (:default-initargs
    :obj nil
    :path nil
    :func nil
    :convert-type nil)
  (:metaclass ns:+ns-object))
|#

(defmethod added-class-key-values ((self (eql (find-class 'kvo-observer))))
  ;; provide documenation for keywords
  '((:obj "An ns:ns-object to be observed by the kvo-observer being created")
    (:path "A list of lisp forms that provides a path within the observed object")
    (:func "A function called when the object is observed to have changed")
    (:convert-type "A data type to which new observations will be converted")))

(defmethod initialize-instance :after ((self kvo-observer)
                                       &key
                                       (obj nil)
                                       (path nil)
                                       (func nil))
  (assure-kvc-methods-defined)
  (when (and obj path func)
    (let ((observed-obj (if (typep obj 'ns:ns-object)
                          obj
                          (wrapper-for obj))))
      (#/addObserver:forKeyPath:options:context: observed-obj self (convert-path-list path) 0 (%null-ptr)))))

(objc:defmethod (#/dealloc :void)
                ((self kvo-observer))
  (when (and (observed-instance self) (observe-path self))
    (let ((observed-obj (if (typep (observed-instance self) 'ns:ns-object)
                          (observed-instance self)
                          (wrapper-for (observed-instance self)))))
      (when *log-bindings*
        (ns-log-format "about to remove observer of ~s via path ~s" observed-obj (convert-path-list (observe-path self))))
      (#/removeObserver:forKeyPath:context: observed-obj self (convert-path-list (observe-path self)) (%null-ptr))))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc::defmethod (#/observeValueForKeyPath:ofObject:change:context: :void)
                 ((self kvo-observer)
                  (key-path :id)
                  (observed-obj :id)
                  (change-dict :id)
                  (context :address))
  (declare (ignore key-path observed-obj change-dict context))
  (when (functionp (when-observed-func self))
    (funcall (when-observed-func self)
             (coerce-obj (#/valueForKeyPath: (observed-instance self) (coerce-obj (observe-path self) 'ns:ns-string))
                         (convert-type self)))))
                  
(defmacro when-observed ((object path new-value-decl) &rest forms)
  (let ((new-value-var (if (listp new-value-decl)
                         (first new-value-decl)
                         new-value-decl))
        (new-value-type (if (listp new-value-decl)
                          (second new-value-decl)
                          t)))
    `(make-instance 'kvo-observer
       :obj ,object
       :path ,path
       :convert-type ',new-value-type
       :func #'(lambda (,new-value-var)
                 ,@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; methods to support access to Lisp lists and arrays as if they were NSArrays

(objc:defmethod (#/count #>NSUInteger)
                ((self lisp-ptr-wrapper))
  (let ((ptr-obj (lpw-lisp-ptr self)))
    (typecase ptr-obj
      (list (list-length ptr-obj))
      (vector (length ptr-obj))
      (t 0))))

(objc:defmethod (#/objectAtIndex: :id)
                ((self lisp-ptr-wrapper) (indx #>NSUInteger))
  (let* ((ptr-obj (lpw-lisp-ptr self))
         (next-obj (typecase ptr-obj
                     (list (nth indx ptr-obj))
                     (vector (elt ptr-obj indx))
                     (t (%null-ptr)))))
    (cond ((eql next-obj (%null-ptr))
           next-obj)
          ((null next-obj)
           (%null-ptr))
          ((and (typep next-obj 'ns:ns-object)
                (not (typep next-obj 'ccl::kvo-object)))
           ;; any kvo-objects will be encapsulated in a lisp-ptr-wrapper
           ;; so that subsequent accesses through this method will try to 
           ;; use ccl::value-for-kvo-key
           next-obj)
          ((typep next-obj 'objc-displayable)
           (lisp-to-ns-object next-obj))
          (t
           (wrapper-for next-obj
                        :controller (lpw-controller self)
                        :parent (lpw-lisp-ptr self))))))

(provide :binding-utils)
            