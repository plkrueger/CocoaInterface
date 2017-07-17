;; lisp-controller.lisp
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

;; Implements a class that can be used within IB as a content object
;; for various standard view types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :lc-classes)
  (require :coerce-obj)
  (require :ns-string-utils)
  (require :ns-object-utils)
  (require :nslog-utils)
  (require :undo)
  (require :assoc-array)
  (require :list-utils)
  (require :alert)
  (require :dev-tools))

(in-package :lc)

;; lisp-controller functionality
;; Class and methods used to view a lisp object using a NSTableView
;; or NSOutlineView or NSBrowser.
;; The :root initarg should be used to provide an object to be represented
;; in an associated view table. Unless user-specified reader/writer
;; functions are also specified (as described later), that object 
;; must be a sequence of objects (e.g. a list or vector) or a
;; hash-table which is treated as a sequence of (key . value) pairs.
;; For use with non-hierarchical display views (NSTableView)
;; the objects will be displayed one per row in the table.
;; For use with hierarchical display views (NSOutlineView or 
;; NSBrowser) the objects comprise the set of root objects; each
;; of which may contain children that will be displayed. Children
;; may themselves have children, etc.
;; Objects can be an arbitrary lisp objects (e.g. a list,
;; vector, structure, hash-table, or class instance).
;;
;; The root object is considered to be at level 0 of the controlled table. Its children
;; are the highest level objects that can be seen in the table and are considered to be at
;; level 0. Any children displayed in multi-level displays like ns:ns-outline-view
;; objects are at sequentially higher levels.
;;
;; When initializing the controller you can specify lisp methods to:
;;   access children at the next higher level with :root-child-key or :child-key-<n> keywords
;;   access information to be displayed in each column from the row-object with :col-keys or :col-keys-<n> keywords
;;       (supply a list of functions, one for each column)
;;   create a new object at a level using :initform (level 0) or :initform-<n> (level n)
;;   specify an id for each column using the :col-ids keyword (supply a list of column ids)
;;   specify a sort-key for items at a level using :sort-key (level 0) or :sort-key-<n> (level n)
;;   specify a sort-predicate for items at a level using :sort-pred (level 0) or :sort-pred-<n> (level n)
;;   
;; The user may also set the undo-doc and undo-name which will cause the lisp-controller
;; to register undo actions for any changes made to the table that it controls.
;; The string is used as part of the undo action name that appears in the edit
;; menu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables

(defvar *lisp-controller-debug* nil)
(defvar *no-selection* (%null-ptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class and methods specific to hash-tables

#| ;; defined in lc-classes.lisp
(defclass ht-entry ()
  ((ht-key :reader ht-key :initarg :key)
   (ht-value :reader ht-value :initarg :value)
   (ht :accessor ht :initarg :ht))
  (:default-initargs
    :key (gensym "KEY")
    :value nil
    :ht nil))
|#

(defmethod (setf ht-key) (new-key (self ht-entry))
  (block set-it
    (let ((new-key-exists (not (eq :not-found (gethash new-key (ht self) :not-found)))))
      (when new-key-exists
        ;; They are redefining a key to be one that already exists in the hash-table
        ;; first verify they want to do this
        (let ((res (alert :text (format nil 
                                        "Continuing will reset value for existing key: ~s"
                                        new-key)
                          :right "Cancel key change"
                          :left "Continue and change key")))
          (unless (eq res :left)
            ;; they don't want to continue
            (return-from set-it))))
      ;; change the value for the existing key
      (setf (gethash new-key (ht self))
            (gethash (ht-key self) (ht self)))
      ;; and then remove the old key that was changed both from the hash table
      ;; and from the list of keys
      ;; new keys are always put at the end of the list unless a sort predicate
      ;; has been specified.
      (remhash (ht-key self) (ht self))
      (setf (slot-value self 'ht-key) new-key))))

(defmethod (setf ht-value) (new-val (self ht-entry))
  (setf (gethash (ht-key self) (ht self)) new-val)
  (setf (slot-value self 'ht-value) new-val))

(let ((ht-hash (make-hash-table)))
  ;; in order to treat hash-tables as containers like lists and vectors we
  ;; need to define a few functions which use a cache of the "children" of
  ;; a hash-table so that we don't need to recreate the whole list every time
  ;; a new child is added

  (defmethod children ((parent hash-table))
    (or (gethash parent ht-hash)
        (setf (gethash parent ht-hash)
              (let ((ht-list nil))
                (maphash #'(lambda (key val)
                             (push (make-instance 'ht-entry
                                     :key key
                                     :value val
                                     :ht parent)
                                   ht-list))
                         parent)
                ht-list))))

  (defmethod (setf children) (new-value (parent hash-table))
    (setf (gethash parent ht-hash) new-value))

  (defmethod add-to-child-seq (parent (seq list) (thing ht-entry))
    (with-slots (ht ht-key ht-value) thing
      (setf (gethash ht-hash parent) (cons thing seq))
      (setf ht parent)
      (setf (gethash parent ht-key) ht-value)))

  (defmethod delete-from-child-seq ((seq list) (thing ht-entry))
    (with-slots (ht ht-key) thing
      (remhash ht-key ht)
      (delete-from-list seq thing)))

) ;; end of hash-table functions within let

;;; Functions to access children for other common types

(defmethod children ((parent sequence))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  parent)

(defmethod (setf children) (new-children (parent vector))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  (or parent new-children))

(defmethod (setf children) (new-children (parent list))
  ;; root objects that are lists or vectors are
  ;; also the sequence of children
  new-children)

(defmethod children (parent)
  (declare (ignore parent))
  ;; any other unknown type of parent
  nil)

;;; Functions to add/delete items from sequences
;;; See also corresponding methods for sequences of ht-entry items
;;; in section of hash-table methods above

(defmethod add-to-child-seq (parent (seq list) thing)
  (declare (ignore parent))
  (nconc seq (list thing)))

(defmethod add-to-child-seq (parent (seq vector) thing)
  (declare (ignore parent))
  (when (array-has-fill-pointer-p seq)
    (vector-push-extend thing seq)))

(defmethod delete-from-child-seq ((seq vector) thing)
  (let ((pos (position thing seq)))
    (dotimes (i (- (fill-pointer seq) pos 1))
      (setf (aref seq (+ pos i))
            (aref seq (+ pos i 1)))))
  (vector-pop seq))

(defmethod delete-from-child-seq ((seq list) thing)
  (delete-from-list seq thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some utility functions used later

         
(defun valid-setf-for (read-func)
  ;; read-func can either be a named function or the name of a function (symbol or string)
  (let* ((fname (typecase read-func
                  (function (function-name read-func))
                  (symbol read-func)
                  (string (read-from-string read-func))))
         (read-form (and (symbolp fname)
                         (typep fname 'function-name)
                         (list fname 'x))))
    (multiple-value-bind (a b c func-form d) (get-setf-expansion read-form)
      (declare (ignore a b c d))
      (if (not (eq (first func-form) 'funcall))
        ;; built in function, so assume setf works
        (when (and (symbolp fname) (not (null fname)))
          (eval `(function (lambda (new-val thing)
                             (setf (,fname thing) new-val)))))
        (fboundp (second (second func-form)))))))

(defun eval-without-errors (form)
  (when *lisp-controller-debug*
    (ns-log (format nil "evaling form: ~s" form)))
  (handler-case (eval form)
    (condition (c)
      (when *lisp-controller-debug*
        (format t "~%condition: ~s" c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-controller class

#| ;; defined in lc-classes.lisp
(defclass lisp-controller (ns:ns-object)
  ((root :accessor root
         :initarg :root)
   (root-type :accessor root-type
              :initarg :root-type)
   (selection :accessor selection
              :initform *no-selection*)
   (undo-doc :accessor undo-doc
             :initarg :undo-doc)
   (undo-name :accessor undo-name
              :initarg :undo-name)
   (objects :accessor objects
            :initform nil)
   (col-ids :accessor col-ids
            :initarg :col-ids)
   (count-func :accessor count-func
               :initarg :count-func)
   (select-func :accessor select-func
                :initarg :select-func)
   (edited-func :accessor edited-func
                :initarg :edited-func)
   (added-func :accessor added-func
               :initarg :added-func)
   (removed-func :accessor removed-func
                 :initarg :removed-func)
   (add-child-func :accessor add-child-func
                   :initarg :add-child-func)
   (search-key :accessor search-key
               :initarg :search-key)
   (search-string :accessor search-string
                  :initform "")
   (search-test :accessor search-test
                :initarg :search-test)
   (search-results :accessor search-results
                   :initform nil)
   (prev-search-results :accessor prev-search-results
                        :initform nil)
   (level-info :accessor level-info)
   (max-level :accessor max-level
              :initform 0)
   (column-info :accessor column-info)
   (nib-initialized :accessor nib-initialized)
   (view-class :accessor view-class)
   (single-level :accessor single-level
                 :initform t)
   (bind-path :accessor bind-path
              :initform nil)
   (bind-obj :accessor bind-obj
             :initform nil)
   (reflect-to-bound-object :accessor reflect-to-bound-object
                            :initform t)
   (observer-obj :accessor observer-obj
                 :initform nil)
   (can-remove :foreign-type #>BOOL :accessor can-remove)
   (can-insert :foreign-type #>BOOL :accessor can-insert)
   (can-add-child :foreign-type #>BOOL :accessor can-add-child)
   (can-search-next :foreign-type #>BOOL :accessor can-search-next)
   (can-search-prev :foreign-type #>BOOL :accessor can-search-prev)
   (has-selection :foreign-type #>BOOL :accessor has-selection)
   (func-owner :accessor func-owner
               :initarg :func-owner)
   (view :foreign-type :id :accessor view))
  (:default-initargs
    :root nil
    :root-type nil
    :col-ids nil
    :count-func nil
    :select-func nil
    :edited-func nil
    :added-func nil
    :removed-func nil
    :add-child-func nil
    :func-owner nil
    :undo-doc nil
    :undo-name "table"
    :search-key #'identity
    :search-test #'string-equal)
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/dealloc :void)
                ((self lisp-controller))
  (when (observer-obj self)
    (unbind self))
  (call-next-method)
  (objc:remove-lisp-slots self))

(defmethod added-class-keywords ((self (eql 'lisp-controller)))
  '(:sort-key-<n> :sort-pred-<n> :col-keys-<n> :initform-<n> :child-key-<n> :row-height-<n>))

(defmethod added-class-key-values ((self (eql (find-class 'lisp-controller))))
  ;; provide keyword documenation for dev-tools functions
  '((:root "The primary data content accessed through a lisp-controller")
    (:root-child-key "Lisp function that when applied to the controller's root returns a list of child objects")
    (:child-key "Lisp function that returns the children of both the root object and row objects at level 0")
    (:child-key-<n> "Lisp function that returns the children of row objects at level <n> of an outline view")
    (:root-type "Type of root; enforced only when the root is bound to some other slot")
    (:row-height "Default height of all rows in the table")
    (:row-height-<n> "Height of rows at level <n>")
    (:search-key "Function to apply to row objects before applying search-test function")
    (:search-test "Function of two arguments. First is result of applying :search-key function to a row object. Second is a search string.")
    (:sort-key "Lisp function")
    (:sort-key-<n> "Lisp function for rows at level <n> of an outline view")
    (:sort-pred "Lisp predicate function")
    (:sort-pred-<n> "Lisp predicate function for rows at level <n> of an outline view")
    (:col-keys "List of lisp functions that return values for their respective column")
    (:col-ids "List of table column identifiers (0, 1, 2 ... used by default)")
    (:col-keys-<n> "List of lisp functions that return values for their respective column for rows at level <n> of an outline view")
    (:func-owner "Lisp object on which select-func, edited-func, added-func, and removed-func functions are called")
    (:count-func "Lisp function that provides a row count if root object has no obvious length")
    (:add-child-func "Developer-supplied Lisp override function called to add a child to some table entry")
    (:select-func "Lisp function called when table selection changes")
    (:edited-func "Lisp function called when table entry has been edited by the user")
    (:added-func "Lisp function called when a table entry has been added by the user")
    (:removed-func "Lisp function called when a table entry has been removed by the user")
    (:initform "Lisp function called to create a new row entry")
    (:initform-<n> "Lisp function called to create a new row entry for rows at level <n> of an outline view")
    (:undo-doc "Document that owns the undo-manager that will be used for table undo actions")
    (:undo-name "Name of the table that will be used in undo menu choices")))

(defun parse-level-keyword (key)
  (let* ((key-name (symbol-name key))
         (name-length (length key-name)))
    (cond ((and (> name-length 9) (string= (subseq key-name 0 9) "SORT-KEY-"))
           (values :sort-key (read-from-string (subseq key-name 9))))
          ((and (> name-length 10) (string= (subseq key-name 0 10) "CHILD-KEY-"))
           (values :child-key (read-from-string (subseq key-name 10))))
          ((and (> name-length 10) (string= (subseq key-name 0 10) "SORT-PRED-"))
           (values :sort-pred (read-from-string (subseq key-name 10))))
          ((and (> name-length 9) (string= (subseq key-name 0 9) "INITFORM-"))
           (values :initform (read-from-string (subseq key-name 9))))
          ((and (> name-length 9) (string= (subseq key-name 0 9) "COL-KEYS-"))
           (values :col-keys (read-from-string (subseq key-name 9))))
          ((and (> name-length 11) (string= (subseq key-name 0 11) "ROW-HEIGHT-"))
           (values :row-height (read-from-string (subseq key-name 11))))
          (t
           (values nil 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instance initialization methods

(defmethod initialize-instance :after ((self lisp-controller)
                                       &rest key-vals
                                       &key
                                       (root nil)
                                       (col-keys nil)
                                       (child-key nil)
                                       (row-height nil)
                                       (sort-key nil)
                                       (sort-pred nil)
                                       (initform nil initform-provided)
                                       (root-child-key #'children)
                                       &allow-other-keys)

  (with-slots (column-info level-info col-ids max-level selection has-selection) self
    (setf selection (wrapper-for nil))
    (setf has-selection #$NO)
    (setf level-info (make-instance 'assoc-array :rank 2))
    (setf column-info (make-instance 'assoc-array :rank 3 :tests (list 'equal 'eql 'eql)))

    ;; set root's :child-key and :child-setf-key if child-key is not provided
    (unless child-key
      (let ((child-writer (valid-setf-for root-child-key)))
        (setf (assoc-aref level-info -1 :child-key) root-child-key)
        (when child-writer
          (setf (assoc-aref level-info -1 :child-setf-key) child-writer))))

    ;; set values either for a single level table or which apply to all levels
    (when col-keys
      (do* ((keys col-keys
                  (rest keys))
            (col-key (first keys)
                     (first keys))
            (col-indx 0
                      (1+ col-indx))
            (col-id (if col-ids (nth col-indx col-ids) (format nil "~s" col-indx))
                    (if col-ids (nth col-indx col-ids) (format nil "~s" col-indx)))
            (col-writer-func (valid-setf-for col-key)
                             (valid-setf-for col-key)))
           ((null keys))
        (setf (assoc-aref column-info col-id 0 :reader) col-key)
        (when col-writer-func
          (setf (assoc-aref column-info col-id 0 :writer) col-writer-func))))
    (when child-key
      ;; sets both root and level 0 child-keys
      (let ((child-writer (valid-setf-for child-key)))
        (setf (assoc-aref level-info -1 :child-key) child-key)
        (setf (assoc-aref level-info 0 :child-key) child-key)
        (when child-writer
          (setf (assoc-aref level-info -1 :child-setf-key) child-writer)
          (setf (assoc-aref level-info 0 :child-setf-key) child-writer))))
    (when sort-key
      (setf (assoc-aref level-info -1 :sort-key) sort-key))
    (when sort-pred
      (setf (assoc-aref level-info -1 :sort-pred) sort-pred))
    (when initform-provided
      (setf (assoc-aref level-info 0 :initform) initform))
    (when row-height
      (setf (assoc-aref level-info -1 :height) row-height))

    ;; set values specific to a particular level. Keywords are the same as above, except that a prefix: level<n>-
    ;; where <n> is the level number precedes the key name. We'll just look through all initargs to find these.
    (do* ((key-val-list key-vals
                        (cddr key-val-list))
          (key (first key-val-list)
               (first key-val-list))
          (val (second key-val-list)
               (second key-val-list)))
         ((null key-val-list))
      (when (keywordp key)
        (multiple-value-bind (key-name level) (parse-level-keyword key)
          (when key-name
            (if (> level max-level)
              (setf max-level level))
            (ecase key-name
              (:row-height
               (setf (assoc-aref level-info level :height) val))
              (:child-key
               (let ((child-writer (valid-setf-for val)))
                 (setf (assoc-aref level-info level :child-key) val)
                 (when child-writer
                   (setf (assoc-aref level-info level :child-setf-key) child-writer))))
              ((:sort-key :sort-pred :initform)
               (setf (assoc-aref level-info level key-name) val))
              (:col-keys (do* ((keys val
                                     (rest keys))
                               (col-key (first keys)
                                        (first keys))
                               (col-indx 0
                                         (1+ col-indx))
                               (col-id (if col-ids (nth col-indx col-ids) (format nil "~s" col-indx))
                                       (if col-ids (nth col-indx col-ids) (format nil "~s" col-indx)))
                               (col-writer-func (valid-setf-for col-key)
                                                (valid-setf-for col-key)))
                              ((null keys))
                           (setf (assoc-aref column-info col-id level :reader) col-key)
                           (when col-writer-func
                             (setf (assoc-aref column-info col-id level :writer) col-writer-func)))))))))
    (when root
      (setf (root self) root) ;; needed to get side effects of (setf root)
      (set-can-insert self (root self)))))

(defmethod obj-level ((self lisp-controller) object)
  (if (single-level self)
    0  ;; all objects are at level 0
    (#/levelForItem: (view self) object)))

(defmethod row-level ((self lisp-controller) row-num)
  (if (single-level self)
    0  ;; all rows are at level 0
    (#/levelForRow: (view self) row-num)))

(defmethod set-can-add-child ((self lisp-controller) row-selected)
  ;; indicates whether new children objects can be placed within
  ;; the object represented in the row specified.
  (#/willChangeValueForKey: self #@"canAddChild")
  (if (assoc-aref (level-info self) (min (row-level self row-selected) (max-level self)) :child-setf-key)
    (setf (can-add-child self)  #$YES)
    (setf (can-add-child self) #$NO))
  (#/didChangeValueForKey: self #@"canAddChild"))

(defmethod set-can-insert :around ((self lisp-controller) new-obj)
  (declare (ignore new-obj))
  (#/willChangeValueForKey: self #@"canInsert")
  (call-next-method)
  (#/didChangeValueForKey: self #@"canInsert"))

(defmethod set-can-insert ((self lisp-controller) new-obj)
  (declare (ignore new-obj))
  ;; indicates whether new children objects can be placed within
  ;; the root object.
  ;; If we have been given an explict insert function then we can or
  ;; if we know the child type for the root type and have a valid
  ;; child key for which there is an associated setf function
  ;; then we can also insert a new object.
  (if (or (add-child-func self)
          (assoc-aref (level-info self) -1 :child-setf-key))
    (setf (can-insert self) #$YES)
    (setf (can-insert self) #$NO)))

(defmethod set-can-insert ((self lisp-controller) (new-obj vector))
  (setf (can-insert self) 
        (if (or (add-child-func self)
                (and (assoc-aref (level-info self) -1 :child-setf-key)
                     (array-has-fill-pointer-p (objects self))
                     (or (adjustable-array-p (objects self))
                         (< (fill-pointer (objects self)) 
                            (first (array-dimensions (objects self)))))))
          #$YES
          #$NO)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View-specific methods

(defmethod init-column-info ((self lisp-controller) (view ns:ns-table-view))
  (with-slots (column-info) self
    (let* ((tc-arr (#/tableColumns view))
           (col-obj nil)
           (idc nil)
           (col-count (#/count tc-arr)))
      (unless tc-arr
        (ns-log "#/tableColumns returned nil for view")
        (return-from init-column-info))
      (dotimes (i col-count)
        (setf col-obj (#/objectAtIndex: tc-arr i))
        (setf idc (ns-to-lisp-string (#/identifier col-obj)))
        ;; make it easy to go convert from column object to column index to column identifier
        (setf (assoc-aref column-info idc nil :col-object) col-obj)
        (setf (assoc-aref column-info i nil :col-object) col-obj)
        (setf (assoc-aref column-info col-obj nil :col-id) idc)
        (setf (assoc-aref column-info col-obj nil :col-title)
              (ns-to-lisp-string (#/title (#/headerCell col-obj))))
        ;; find any formatter attached to the data cell for this column and 
        ;; use info from it to help us translate to and from lisp objects
        ;; appropriately
        (let ((formatter-object (#/formatter (#/dataCell col-obj))))
          (unless (eql formatter-object (%null-ptr))
            (cond ((typep formatter-object 'ns:ns-date-formatter)
                   (setf (assoc-aref column-info col-obj nil :col-format) :date))
                  ((typep formatter-object 'ns:ns-number-formatter)
                   (cond ((#/generatesDecimalNumbers formatter-object)
                          (let ((dec-digits (#/maximumFractionDigits formatter-object)))
                            (setf (assoc-aref column-info col-obj nil :col-format)
                                  (list :decimal dec-digits))))
                         (t
                          (setf (assoc-aref column-info col-obj nil :col-format)
                                :number)))))))))))

(defmethod object-hierarchy ((self lisp-controller) obj)
  (do* ((hierarchy (and (typep obj 'lisp-ptr-wrapper) (list (lpw-lisp-ptr obj)))
                   (cons (lpw-lisp-ptr parent) hierarchy))
        (parent (#/parentForItem: (view self) obj)
                (#/parentForItem: (view self) parent)))
       ((eql parent (%null-ptr)) hierarchy)))

(defmethod object-at-row ((self lisp-controller) row-selected)
  ;; returns two objects: the lisp object represented by the specified 
  ;; row and the parent of that object
  (unless (eql row-selected -1)
    (cond ((typep (view self) 'ns:ns-outline-view)
           (let ((ptr-wrapper (#/itemAtRow: (view self) row-selected)))
             (values (lpw-lisp-ptr ptr-wrapper)
                     (lpw-parent ptr-wrapper))))
          ((typep (view self) 'ns:ns-table-view)
           (values (elt (objects self) row-selected) (root self))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods callable by outside functions within a lisp program

(defmethod current-selection ((self lisp-controller))
  ;; returns the Lisp object and it's row number as multiple values
  (let* ((row-num (#/selectedRow (view self)))
         (obj (object-at-row self row-num)))
    (values obj row-num)))

(defmethod lisp-controller-changed ((self lisp-controller))
  ;; program should call this if it changes the contents of the
  ;; list object (but not the pointer to the list itself). In
  ;; the latter case (setf (root <lisp-controller-instance>) <new-value>) should be called.
  (#/reloadData (view self)))

(defmethod (setf root) :before (new-obj (self lisp-controller))
  (declare (ignore new-obj))
  (#/willChangeValueForKey: self #@"root"))

(defmethod (setf root) :after (new-obj (self lisp-controller))
  ;; cache the children of the root object because they are used so frequently
  (setf (objects self) (children-of-object self new-obj -1))
  (when (and (bind-obj self) (bind-path self) (reflect-to-bound-object self))
    ;; root is bound to some lisp value, make that value eq to the root by passing a wrapper
    (#/setValue:forKey: (bind-obj self) (wrapper-for new-obj) (coerce-obj (bind-path self) 'ns:ns-string)))
  (#/didChangeValueForKey: self #@"root")
  (unless (eql (view self) (%null-ptr))
    (set-can-insert self new-obj)
    (sort-sequence self (objects self) 0)
    (#/willChangeValueForKey: self #@"selection")
    (setf (selection self) *no-selection*)
    (#/didChangeValueForKey: self #@"selection")
    (#/reloadData (view self))
    (if (typep (view self) 'ns:ns-outline-view)
      (#/collapseItem:collapseChildren: (view self) (%null-ptr) t)))
  new-obj)

(defmethod (setf view) :after (new-view (self lisp-controller))
  (when (and new-view (not (eql new-view (%null-ptr))))
    (setf (view-class self) (#/class (view self)))
    (setf (single-level self) (not (typep new-view 'ns:ns-outline-view)))
    (init-column-info self (view self)))
    (set-can-insert self (root self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous methods for accessing and transforming objects

(defmethod new-object-for-level ((self lisp-controller) level)
  ;; Create & initialize a new instance of some type
  (let ((initform (assoc-aref (level-info self) (min level (max-level self)) :initform)))
    (if (functionp initform)
      (funcall initform)
      (eval initform))))

(defmethod children-of-object ((self lisp-controller) obj lev)
  ;; Get the children of an instance of some type
  (let* ((level (min lev (max-level self)))
         (child-key (assoc-aref (level-info self) level :child-key))
         (children-object nil))
    (if child-key
      (setf children-object (funcall child-key obj)))
    ;; if the children object is a hash-table, expand it into an ht-entry list
    (when (typep children-object 'hash-table)
      (setf children-object (children children-object)))
    (sort-sequence self children-object (1+ level))))

(defmethod add-child-to ((self lisp-controller) (parent ht-entry) level &key (new-child nil))
  (declare (ignore new-child))
  ;; We want to handle the children of ht-entry objects a bit differently
  ;; Basically we would like to add a child to the children of the value of the entry
  (add-child-to self (ht-value parent) (min level (max-level self))))

(defmethod add-child-to ((self lisp-controller) par lev &key (new-child nil))
  (let* ((level (min lev (max-level self)))
         (parent (if (eql lev -1) (root self) par))
         (child-key (assoc-aref (level-info self) level :child-key))
         (child-initform (assoc-aref (level-info self) (min (1+ level) (max-level self)) :initform))
         (set-child-func (assoc-aref (level-info self) level :child-setf-key))
         (new-children nil))
    (if (and child-key child-initform set-child-func)
      ;; we've got everything we need to set the child ourselves
      (let ((children (children-of-object self parent level)))
        (unless new-child (setf new-child (eval child-initform)))
        ;; if new-child is :cancel, ignore it. This lets the child-initform
        ;; effectively cancel the addition of a new child
        (if (eq new-child :cancel)
          (setf new-children children)
          (progn
            (when (undo-doc self)
              (set-undo (undo-doc self) 
                        #'(lambda ()
                            (remove-child-from self par new-child lev)
                            (#/reloadData (view self)))
                        (format nil "add row to ~a" (undo-name self))))
            (setf new-children 
                  (funcall set-child-func 
                           (add-to-child-seq parent children new-child) 
                           parent))
            (when (typep new-child 'ht-entry)
              (setf (ht new-child) parent)
              (setf (gethash (ht-key new-child) parent) (ht-value new-child))))))
      ;; else see if there is a user-specified function to add a child
      (when (add-child-func self)
        (multiple-value-setq (new-children new-child)
          (funcall (add-child-func self) parent))))
    (when (and (added-func self) (not (eq new-child :cancel)))
      ;; notify by calling the function specified
      (let ((last-child (if (typep new-child 'ht-entry)
                          (list (ht-key new-child) (ht-value new-child))
                          new-child)))
        (when last-child
          (funcall (added-func self) 
                   (func-owner self)
                   self
                   (root self)
                   parent 
                   last-child))))
    (sort-sequence self new-children (1+ level))))

(defmethod col-value ((self lisp-controller) row-obj lev col-obj)
  ;; Get the lisp value for some column for an object
  ;; return "" if there isn't one so the display doesn't
  ;; have "nil" for columns without values.
  (let* ((level (min lev (max-level self)))
         (id (assoc-aref (column-info self) col-obj nil :col-id))
         (reader-func (assoc-aref (column-info self) id level :reader)))
    (if reader-func
      (funcall reader-func row-obj)
      "")))

(defmethod set-col-value ((self lisp-controller) row-obj lev col-obj new-value)
  ;; set the lisp value for some column for an object
  (let* ((level (min lev (max-level self)))
         (id (assoc-aref (column-info self) col-obj nil :col-id))
         (writer-func (assoc-aref (column-info self) id level :writer)))
    (if writer-func
      (funcall writer-func new-value row-obj))))

(defmethod remove-child-from ((self lisp-controller) par child parent-lev)
  (let* ((parent-level (min (max-level self) parent-lev))
         (parent (if (eql parent-lev -1) (root self) par))
         (child-key (assoc-aref (level-info self) parent-level :child-key))
         (set-child-func (assoc-aref (level-info self) parent-level :child-setf-key))
         (parent-is-root (eq parent (root self)))
         (new-children nil))
    (when (and child-key set-child-func)
        (when (undo-doc self)
          (set-undo (undo-doc self) 
                    #'(lambda ()
                        (add-child-to self par parent-lev :new-child child)
                        (#/reloadData (view self)))
                    (format nil "remove row from ~a" (undo-name self))))
        (let ((children (funcall child-key parent)))
          (setf new-children 
                (funcall set-child-func
                         (delete-from-child-seq children child)
                         parent))))
    (when (and parent-is-root (listp parent) (null new-children))
      ;; This is only applicable if the parent is the root and there are
      ;; no children left as a result of the removal
      (setf (root self) nil)
      (setf (objects self) nil))
    (when (removed-func self)
      (funcall (removed-func self) (func-owner self) self (root self) parent child))
    (sort-sequence self new-children (1+ parent-level))))

(defmethod sort-sequence ((self lisp-controller) (seq sequence) lev)
  ;; sort a sequence of objects
  (if (plusp (length seq))
    (let* ((level (min lev (max-level self)))
           (seq-elt-sort-pred (or (assoc-aref (level-info self) level :sort-pred)
                                  (assoc-aref (level-info self) -1 :sort-pred)))
           (seq-elt-sort-key (or (assoc-aref (level-info self) level :sort-key)
                                 (assoc-aref (level-info self) -1 :sort-key))))
      (if seq-elt-sort-pred
        (typecase seq
          (cons 
           (sort-list-in-place seq seq-elt-sort-pred seq-elt-sort-key))
          (vector 
           (if seq-elt-sort-key
             (sort seq seq-elt-sort-pred :key seq-elt-sort-key)
             (sort seq seq-elt-sort-pred))))
        seq))
    seq))

(defmethod sort-sequence ((self lisp-controller) thing level)
  (declare (ignore level))
  ;; trying to sort something that isn't a sequence
  ;; just do nothing
  thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods invoked by NSTableView objects at runtime.
;; Needed to be a data source for NSTableView

(objc:defmethod (#/tableView:objectValueForTableColumn:row: :id) 
                ((self lisp-controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (declare (ignore tab))
  (let ((ns-format (assoc-aref (column-info self) col nil :col-format)))
    (lisp-to-ns-object (col-value self (elt (objects self) row) (row-level self row) col)
                       ns-format)))

(objc:defmethod (#/numberOfRowsInTableView: #>NSInteger) 
                ((self lisp-controller) (tab :id))
  ;; Assumes that objects is some type of sequence
  ;; Subclass should override this method if that is not true.
  (declare (ignore tab))
  (with-slots (root objects count-func func-owner) self
    (if root
      (if count-func
        (funcall count-func func-owner self root)
        (typecase objects
          (array (if (array-has-fill-pointer-p objects)
                   (fill-pointer objects)
                   (first (array-dimensions objects))))
          (t
           (length objects))))
      0)))
  
(objc:defmethod (#/tableView:setObjectValue:forTableColumn:row: :void)
                ((self lisp-controller) 
                 (tab :id)
                 (val :id)
                 (col :id)
                 (row #>NSInteger))
  ;; We let the user edit the table and something was changed
  ;; try to convert it to the same type as what is already in that
  ;; position in the objects.
  (let* ((row-obj (elt (objects self) row))
         (level (row-level self row))
         (old-val (col-value self row-obj level col))
         (lisp-class (class-of old-val))
         (ns-format (assoc-aref (column-info self) col nil :col-format))
         (new-val (ns-to-lisp-object val :lisp-class lisp-class :ns-format ns-format)))
    (set-col-value self row-obj level col new-val)
    (when (undo-doc self)
      (set-undo (undo-doc self) 
                #'(lambda ()
                    (#/tableView:setObjectValue:forTableColumn:row:
                     self
                     tab
                     (lisp-to-ns-object old-val (assoc-aref (column-info self) col nil :col-format))
                     col
                     row))
                (format nil "change ~a row ~s" (undo-name self) row)))
    (when (edited-func self)
      (let* ((row-obj (object-at-row self row))
             (edited-obj (if (typep row-obj 'ht-entry)
                           (list (ht-key row-obj) (ht-value row-obj))
                           row-obj)))
        (funcall (edited-func self)
                 (func-owner self)
                 self
                 (root self)
                 row
                 (assoc-aref (column-info self) col nil :col-title)
                 edited-obj
                 old-val
                 new-val)))
    ;; re-sort and reload the table
    ;; unfortunately we probably have to do this for every change since
    ;; we don't know what affects the sort order
    (sort-sequence self (objects self) 0)
    (#/reloadData (view self))))

(objc:defmethod (#/tableView:shouldEditTableColumn:row: #>BOOL)
                ((self lisp-controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (declare (ignore tab))
  ;; allow editing if there is a function available to setf a new value
  (if (assoc-aref (column-info self)
                  (assoc-aref (column-info self) col nil :col-id)
                  (row-level self row)
                  :writer)
    #$YES
    #$NO))

(objc:defmethod (#/tableViewSelectionDidChange: :void) 
                ((self lisp-controller) (notif :id))
  (let* ((tab (#/object notif))
         (row-indx (#/selectedRow tab))
         (col-indx (#/selectedColumn tab)))
    ;; update KVC so that anything bound to "selection" will be notified
    (#/willChangeValueForKey: self #@"selection")
    (setf (selection self)
          (if (and (view self) (root self))
            (let* ((row-num (#/selectedRow (view self)))
                   (obj (object-at-row self row-num)))
              (cond ((eql row-num -1)
                     *no-selection*)
                    ((typep obj 'objc:objc-object)
                     obj)
                    (t
                     (wrapper-for obj :controller self))))
            *no-selection*))
    (#/didChangeValueForKey: self #@"selection")
    ;; enable/disable buttons that remove current selection
    (#/willChangeValueForKey: self #@"canRemove")
    (#/willChangeValueForKey: self #@"hasSelection")
    (if (minusp row-indx)
      (progn
        (setf (can-remove self) #$NO)
        (setf (has-selection self) #$NO))
      (progn
        (setf (can-remove self) #$YES)
        (setf (has-selection self) #$YES)))
    (#/didChangeValueForKey: self #@"hasSelection")
    (#/didChangeValueForKey: self #@"canRemove")
    ;; enable/disable buttons that want to add a child to
    ;; the current selection
    (set-can-add-child self row-indx)
    ;; User code to do something when a cell is selected
    (when (select-func self)
      (let* ((row-obj (and (not (eql row-indx -1)) (object-at-row self row-indx)))
             (col (assoc-aref (column-info self) col-indx nil :col-obj))
             (col-title (or (assoc-aref (column-info self) col nil :col-title) ""))
             (selected-obj (cond ((and (minusp col-indx) (minusp row-indx))
                                  nil)
                                 ((minusp col-indx)
                                  row-obj)
                                 ((minusp row-indx)
                                  col-title)
                                 (t
                                  (col-value self row-obj (row-level self row-indx) col)))))
        (funcall (select-func self)
                 (func-owner self)
                 self
                 (root self)
                 row-indx
                 col-title
                 selected-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods invoked by NSOutlineView objects at runtime.
;; Needed to be a data source for NSOutlineView.

(objc:defmethod (#/outlineView:child:ofItem: :id)
                ((self lisp-controller) 
                 (olview :id)
                 (child #>NSInteger)
                 (item :id))
  (declare (ignore olview))
  (with-slots (objects) self
    (cond ((typep item 'lisp-ptr-wrapper)
           (let* ((parent (lpw-lisp-ptr item))
                  (parent-depth (lpw-depth item))
                  (children (children-of-object self parent (obj-level self item)))
                  (child-ptr (elt children child)))
             (wrapper-for child-ptr 
                          :controller self
                          :depth (1+ parent-depth)
                          :parent parent)))
          ((eql item (%null-ptr))
           (let ((child-ptr (elt objects child)))
             (wrapper-for child-ptr :controller self :depth 1)))
          (t
           (%null-ptr)))))

(objc:defmethod (#/outlineView:isItemExpandable: #>BOOL)
                ((self lisp-controller)
                 (olview :id)
                 (item :id))
  (declare (ignore olview))
  (cond ((eql item (%null-ptr))
         ;; root object
         #$YES)
        ((typep item 'lisp-ptr-wrapper)
         (if (children-of-object self (lpw-lisp-ptr item) (obj-level self item))
           #$YES
           #$NO))
         (t
          #$NO)))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: #>NSInteger)
                ((self lisp-controller) 
                 (olview :id)
                 (item :id))
  (declare (ignore olview))
  (cond ((typep item 'lisp-ptr-wrapper)
         (length (children-of-object self (lpw-lisp-ptr item) (obj-level self item))))
        ((eql item (%null-ptr))
         (length (objects self)))
        (t
         0)))

(objc:defmethod (#/outlineView:objectValueForTableColumn:byItem: :id)
                ((self lisp-controller) 
                 (olview :id)
                 (col :id)
                 (item :id))
  (declare (ignore olview))
  (let* ((level (obj-level self item))
         (ns-format (assoc-aref (column-info self) col nil :col-format)))
    (lisp-to-ns-object (col-value self (lpw-lisp-ptr item) level col) ns-format)))

(objc:defmethod (#/outlineView:setObjectValue:forTableColumn:byItem: :void)
                ((self lisp-controller) 
                 (olview :id)
                 (val :id)
                 (col :id)
                 (item :id))
  (let* ((row-obj (lpw-lisp-ptr item))
         (level (obj-level self item))
         (old-val (col-value self row-obj level col))
         (lisp-class (class-of old-val))
         (ns-format (assoc-aref (column-info self) col nil :col-format))
         (new-val (ns-to-lisp-object val :lisp-class lisp-class :ns-format ns-format)))
    (set-col-value self row-obj level col new-val)
    (when (undo-doc self)
      (set-undo (undo-doc self) 
                #'(lambda ()
                    (#/outlineView:setObjectValue:forTableColumn:byItem:
                     self
                     olview
                     (lisp-to-ns-object old-val (assoc-aref (column-info self) col nil :col-format))
                     col
                     item))
                (format nil "change ~a" (undo-name self))))
    (when (edited-func self)
      (let* ((row (#/rowForItem: olview item))
             (edited-obj (if (typep row-obj 'ht-entry)
                           (list (ht-key row-obj) (ht-value row-obj))
                           row-obj)))
        (funcall (edited-func self)
                 (func-owner self)
                 self
                 (root self)
                 row
                 (or (assoc-aref (column-info self) col nil :col-title) "")
                 edited-obj
                 old-val
                 new-val)))))

(objc:defmethod (#/outlineView:shouldEditTableColumn:item: #>BOOL)
                ((self lisp-controller) 
                 (olview :id)
                 (col :id)
                 (item :id))
  (declare (ignore olview))
  ;; allow editing if there is a function available to setf a new value
  (if (assoc-aref (column-info self)
                  (assoc-aref (column-info self) col nil :col-id)
                  (min (obj-level self item) (max-level self))
                  :writer)
    #$YES
    #$NO))

(objc:defmethod (#/outlineViewSelectionDidChange: :void) 
                ((self lisp-controller) (notif :id))
  (let* ((ov (#/object notif))
         (row-indx (#/selectedRow ov))
         (col-indx (#/selectedColumn ov))
         (selected-item (#/itemAtRow: (view self) row-indx))
         (obj-hierarchy (object-hierarchy self selected-item)))
    ;; update KVC so that anything bound to "selection" will be notified
    (#/willChangeValueForKey: self #@"selection")
    ;; set the object that will be returned as the value of #/selection
    (setf (selection self)
          (if (and (view self) (root self))
            (let* ((row-num (#/selectedRow (view self)))
                   (obj (object-at-row self row-num)))
              (if (typep obj 'objc:objc-object)
                obj
                (wrapper-for obj :controller self)))
            *no-selection*))
    (#/didChangeValueForKey: self #@"selection")
    ;; enable/disable buttons that remove current selection
    (#/willChangeValueForKey: self #@"canRemove")
    (#/willChangeValueForKey: self #@"hasSelection")
    (if (minusp row-indx)
      (progn
        (setf (can-remove self) #$NO)
        (setf (has-selection self) #$NO))
      (progn
        (setf (can-remove self) #$YES)
        (setf (has-selection self) #$YES)))
    (#/didChangeValueForKey: self #@"hasSelection")
    (#/didChangeValueForKey: self #@"canRemove")
    ;; enable/disable buttons that want to add a child to
    ;; the current selection
    (set-can-add-child self row-indx)
    ;; User code to do something when a cell is selected
    (when (select-func self)
      (let* ((row-obj (and (not (eql row-indx -1)) (object-at-row self row-indx)))
             (col (assoc-aref (column-info self) col-indx nil :col-obj))
             (col-title (or (assoc-aref (column-info self) col nil :col-title) ""))
             (selected-obj (cond ((and (minusp col-indx) (minusp row-indx))
                                  nil)
                                 ((minusp col-indx)
                                  row-obj)
                                 ((minusp row-indx)
                                  col-title)
                                 (t
                                  (col-value self row-obj (row-level self row-indx) col)))))
        (funcall (select-func self)
                 (func-owner self)
                 self
                 (root self)
                 obj-hierarchy
                 col-title
                 selected-obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for inserting and removing rows. User interface can trigger these
;; (for example) by setting action methods on buttons 

(objc:defmethod (#/insert: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  ;; insert a new object into the root object
  (unless (root self)
    ;; need to create a root object
    ;; may still be null if root type is 'list
    (setf (root self)
          (new-object-for-level self -1)))
  (let ((new-children (add-child-to self (root self) -1)))
    (when (null (root self))
      ;; special hack for root objects that are lists and may be null
      (setf (root self) new-children)))
  (#/reloadData (view self)))

(objc:defmethod (#/addChild: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  ;; add a new child to the currently selected item
  (let* ((row-num (#/selectedRow (view self)))
         (parent (object-at-row self row-num)))
    (add-child-to self parent (row-level self row-num)))
  (#/reloadData (view self)))

(objc:defmethod (#/remove: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  (let ((row-num (#/selectedRow (view self))))
    (multiple-value-bind (child parent) (object-at-row self row-num)
      (when parent
        (remove-child-from self parent child (1- (row-level self row-num)))
        (#/reloadData (view self))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for searching for a row
;; Methods will locate a row that matches the search string and scroll
;; the view to that row. Use this by binding some text field value
;; to "searchString" for this lisp-controller. When the string in the
;; text field is modified, the object will be found and the view scrolled
;; to the row found.

(objc:defmethod (#/searchString :id)
                ((self lisp-controller))
  (coerce-obj (search-string self) 'ns:ns-object))

(objc:defmethod (#/setSearchString: :void)
                ((self lisp-controller) (obj :id))
  (setf (search-string self) (coerce-obj obj 'string))
  ;; now do the search
  (search-for-rows self))

;; make search-string kvo compliant
(defmethod (setf search-string) (new-value (self lisp-controller)) 
  (#/willChangeValueForKey: self #@"searchString")
  (setf (slot-value self 'search-string) new-value)
  (#/didChangeValueForKey: self #@"searchString"))

(defmethod search-for-rows ((self lisp-controller))
  ;; search for a rows that match the current value of search-string
  ;; if none is found, pop up an alert to tell user
  (let ((row-paths (mapcan #'(lambda (row)
                               (search-in-row-hierarchy self row 0))
                           (objects self))))
    (setf (search-results self) nil)
    (setf (prev-search-results self) nil)
    (if row-paths
      (progn
        (setf (search-results self) row-paths)
        (scroll-to-next-found self))
      (alert :text (format nil "String ~s not found" (search-string self))))))
         
(defmethod search-in-row-hierarchy ((self lisp-controller) row level)
  (let ((res nil))
    (unless (null row)
      (when (funcall (search-test self) (funcall (search-key self) row) (search-string self))
        (setf res (list (list row))))
      (when (typep (view self) 'ns:ns-outline-view)
        ;; check the children too
        (let* ((children (children-of-object self row level))
               (found-paths (mapcan #'(lambda (row)
                                        (search-in-row-hierarchy self row (1+ level)))
                                    children)))
          (when found-paths
            (setf res (nconc res (mapcar #'(lambda (found-path)
                                             (cons row found-path))
                                         found-paths))))))
      res)))

(defmethod scroll-to  ((self lisp-controller) row-path)
  (if (typep (view self) 'ns:ns-outline-view)
    (progn
      (dolist (row-obj row-path)
        (#/expandItem: (view self) (wrapper-for row-obj :controller self)))
      (let ((row (#/rowForItem: (view self) (wrapper-for (first (last row-path)) :controller self))))
        (#/selectRowIndexes:byExtendingSelection: (view self)
                                                  (#/indexSetWithIndex: ns:ns-index-set row)
                                                  #$NO)
        (#/scrollRowToVisible: (view self) row)))
    (let ((row (position (first row-path) (objects self))))
      (#/selectRowIndexes:byExtendingSelection: (view self)
                                                (#/indexSetWithIndex: ns:ns-index-set row)
                                                #$NO)
      (#/scrollRowToVisible: (view self) row))))
  
(defmethod scroll-to-next-found ((self lisp-controller))
  (let ((row-path (pop (search-results self))))
    (when row-path
      (push row-path (prev-search-results self))
      (scroll-to self row-path))
    (#/willChangeValueForKey: self #@"canSearchNext")
    (setf (can-search-next self)
          (if (null (search-results self)) #$NO #$YES))
    (#/didChangeValueForKey: self #@"canSearchNext")
    (#/willChangeValueForKey: self #@"canSearchPrev")
    (setf (can-search-prev self)
          (if (> (length (prev-search-results self)) 1) #$YES #$NO))
    (#/didChangeValueForKey: self #@"canSearchPrev")))

(defmethod scroll-to-prev-found ((self lisp-controller))
  ;; top of the prev-search-results is the last thing displayed, so skip it
  (let* ((current-row-path (pop (prev-search-results self)))
         (row-path (first (prev-search-results self))))
    (when current-row-path
      (push current-row-path (search-results self)))
    (when row-path
      (scroll-to self row-path))
    (#/willChangeValueForKey: self #@"canSearchNext")
    (setf (can-search-next self)
          (if (null (search-results self)) #$NO #$YES))
    (#/didChangeValueForKey: self #@"canSearchNext")
    (#/willChangeValueForKey: self #@"canSearchPrev")
    (setf (can-search-prev self)
          (if (> (length (prev-search-results self)) 1) #$YES #$NO))
    (#/didChangeValueForKey: self #@"canSearchPrev")))

(objc:defmethod (#/searchNext: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  (scroll-to-next-found self))

(objc:defmethod (#/searchPrev: :void)
                ((self lisp-controller) (button :id))
  (declare (ignore button))
  (scroll-to-prev-found self))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for using a lisp-controller as an initial binding target
;; Binding paths must start with "root" or "selected" and can use
;; lisp accessors from that point to other lisp targets.
;; Any atomic value found by following the path is converted to an 
;; Objective-C value and returned. If a non-atomic value is found by 
;; following the path it is encapsulated within a lisp-ptr-wrapper object
;; and returned. When that path is subsequently followed the lisp-ptr-wrapper
;; will handle the path reference in the same way.

(objc:defmethod (#/root :id)
                ((self lisp-controller))
  (cond ((and (typep (root self) 'objc:objc-object)
              (not (typep (root self) 'ccl::kvo-object)))
             (root self))
        ((null (root self))
         (%null-ptr))
        ((typep (root self) 'objc-displayable)
         (coerce-obj (root self) 'ns:ns-object))
        ((typep (root self) 'ns:ns-object)
         (root self))
        (t
         (wrapper-for (root self) :controller self))))

(objc:defmethod (#/setRoot: :void)
                ((self lisp-controller) (obj :id))
  (setf (root self)
        (cond ((eql (%null-ptr) obj)
               nil)
              ((typep obj 'lisp-ptr-wrapper)
               (lpw-lisp-ptr obj))
              (t
               (coerce-obj obj 't)))))

(objc:defmethod (#/selection :id)
                ((self lisp-controller))
  ;; since this is almost always used when binding, we'll always return a lisp-pointer-wrapper
  ;; for non objective-c objects to support subsequent binding path elements
  (selection self))

(defmethod modified-bound-value ((self lisp-controller) edited-obj key old-val new-val)
  ;; called when a bound lisp slot is modified by the interface.
  ;; Calls any edited-func set up for this controller.
  (when (edited-func self)
    (funcall (edited-func self)
             (func-owner self)
             self
             (root self)
             nil
             key
             edited-obj
             old-val
             new-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods related to using the controller's root object as a legal binding argument

(objc:defmethod (#/exposedBindings :id)
                ((self lisp-controller))
  (coerce-obj (list "root") 'ns:ns-array))

(objc:defmethod (#/bind:toObject:withKeyPath:options: :void)
                ((self lisp-controller)
                 (bind-str :id)
                 (to-obj :id)
                 (key-path :id)
                 (options :id))
  (declare (ignore options))
  (when (string= (coerce-obj bind-str 'string) "root")
    (let ((lisp-path-str (coerce-obj key-path 'string)))
      (setf (bind-path self) lisp-path-str)
      (setf (bind-obj self) to-obj)
      (setf (reflect-to-bound-object self) t)
      (setf (observer-obj self)
            (when-observed (to-obj lisp-path-str (new-root lisp-ptr-wrapper))
              (setf (reflect-to-bound-object self) nil)
              (setf (root self) (lpw-lisp-ptr new-root))
              (setf (reflect-to-bound-object self) t))))))

(objc:defmethod (#/unbind: :void)
                ((self lisp-controller)
                 (bind-str :id))
  (when (and (string= (coerce-obj bind-str 'string) "root")
             (bind-path self)
             (bind-obj self)
             (observer-obj self))
    (#/release (observer-obj self))
    (setf (observer-obj self) nil)
    (setf (reflect-to-bound-object self) nil)
    (setf (bind-path self) nil)
    (setf (bind-obj self) nil)))

(defmethod unbind ((self lisp-controller) &optional (str "root"))
  (#/unbind: self (coerce-obj str 'ns:ns-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns:ns-table-view delegate methods

(objc:defmethod (#/outlineView:heightOfRowByItem: #>CGFloat)
                ((self lisp-controller) (ov :id) (item :id))
  (cgfloat (or (assoc-aref (level-info self)  (min (max-level self) (obj-level self item)) :height)
               (assoc-aref (level-info self)  -1 :height)
               (#/rowHeight ov))))


(provide :lisp-controller)
      