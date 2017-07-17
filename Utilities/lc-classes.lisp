;; lc-classes.lisp

#|
The MIT license.

Copyright (c) 2013 Paul L. Krueger

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

;; Defines all the classes used within the lisp-controller (lc) package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :lc)

(defclass ht-entry ()
  ((ht-key :reader ht-key :initarg :key)
   (ht-value :reader ht-value :initarg :value)
   (ht :accessor ht :initarg :ht))
  (:default-initargs
    :key (gensym "KEY")
    :value nil
    :ht nil))

(defclass lisp-controller (ns:ns-object)
  ((root :accessor root
         :initarg :root)
   (root-type :accessor root-type
              :initarg :root-type)
   (selection :accessor selection
              :initform (%null-ptr))
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

(provide :lc-classes)
