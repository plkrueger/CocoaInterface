;; dev-tools.lisp

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :objc-initialize)
  (require :objc-method-info)
  (require :file-directory-utils)
  (require :lv-classes)
  (require :coerce-obj))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer support methods that print out applicable keywords for make-instance for a class or object
;; and which provide information about acceptable values for each keyword

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method used to document acceptable arguments to coerce-obj for a given target-type

(defun sources-for-target-class (target-class)
  (case target-class
    (ns:ns-color "ns:ns-color or one of :black :blue :brown :clear :cyan :dark-gray :gray :green :light-gray :magenta :orange :purple :red :white :yellow")
    (ns:ns-rect "ns:ns-rect or list of the form (x y width height)")
    (ns:ns-size "ns:ns-size or list of the form (width height)")
    (ns:ns-point "ns:ns-point or list of the form (x y)")
    (ns:ns-attributed-string "ns:ns-string, ns:ns-attributed-string, string, or attributed-string")
    (ns:ns-string "ns:ns-string, string, or attributed-string")
    (ns:ns-decimal-number "ns:ns-decimal or integer with implied decimal point (see decimal.lisp)")
    (ns:ns-object "pretty much anything")
    (ns:ns-date "a lisp time (as created by encode-universal-time) or see date.lisp")
    (ns:ns-data "pretty much anything")
    (ns:ns-dictionary "ns:ns-dictionary, association list, or hash table")
    (ns:ns-array "ns:ns-array or sequence")
    (t nil)))

(defmethod added-class-keywords ((self symbol))
;; This method is used to tell the init-keys function about additional keywords that are 
;; applicable to the initialization of some object. It should return a list of additional keywords.
  nil)

(defmethod init-keys ((self ns:ns-object) &key (return-list nil) (columns 3))
  ;; returns a list of all applicable keywords for all initialize-instance :after methods that will be called
  ;; when (make-instance self ...) is called.
 (init-keys (class-of self) :return-list return-list :columns columns))

(defmethod concatenated-keyword ((key-list cons))
  (intern (subseq (format nil "~{~s~^_~}" key-list) 1) :keyword))

(defmethod init-keys ((self objc::objc-class) &key (return-list nil) (columns 3))
  (let ((keys (sort (remove-duplicates
                     (append (mapcan #'function-keywords
                                      (compute-applicable-methods-using-classes #'initialize-instance
                                                                                (list self)))
                              (ccl::class-slot-initargs self)
                              (mapcar #'(lambda (key-func-pair)
                                          (concatenated-keyword (car key-func-pair)))
                                      (ccl::all-init-keywords-for-class self))
                              (added-class-keywords (class-name self))))
                     #'string<
                     :key #'symbol-name)))
    (assure-added-keys-documented self)
    (assure-init-keys-documented self)
    (unless return-list
      (let* ((max-key-length (apply #'max (mapcar #'(lambda (k)
                                                           (length (symbol-name k)))
                                                  keys)))
             (col-fmt-str (format nil "~a~d~a"
                                  "~^~0,"
                                  (+ max-key-length 3)
                                  "@t~@[:~a~]"))
             (all-cols-fmt-str ""))
        (dotimes (c (1- columns))
          (setf all-cols-fmt-str (concatenate 'string all-cols-fmt-str col-fmt-str)))
        (format t
                (format nil "~a~a~a"
                        "~{~%:~a"
                        all-cols-fmt-str
                        "~}")
                (mapcar #'(lambda (k)
                            (unless (null k)
                              (string-downcase (symbol-name k))))
                        (columnize keys columns)))))
    (if return-list
      keys
      (values))))

(defmethod init-keys ((self symbol) &key (return-list nil) (columns 3))
  (let ((cl (find-class self nil)))
    (when cl (init-keys cl return-list columns))))

(defun convert-form-p (form)
  (and (eq (first form) 'defun)
       (string= (subseq (symbol-name (second form)) 0 8) "CONVERT-")))

(defmethod convert-func-p ((sym symbol))
  (let* ((name (symbol-name sym))
         (lngth (length name)))
    (and (> lngth 8)
         (string= (subseq name 0 8) "CONVERT-"))))

(defun init-form-p (form)
  (and (eq (first form) 'defmethod)
       (eq (second form) 'initialize-instance)
       (eq (third form) :after)))

(defun recursive-find-if (if-func form)
  (cond ((funcall if-func form)
         form)
        ((listp form)
         (some #'(lambda (x)
                   (recursive-find-if if-func x))
               form))
        (t
         nil)))

(defun dash-p-sym-p (sym)
  ;; return true if symbol ends with "-P"
  (when (symbolp sym)
    (let* ((name (symbol-name sym))
           (lngth (length name)))
      (when (> lngth 2)
        (string= "-P" (subseq name (- lngth 2)))))))

(defun objc-set-function-p (sym)
  ;; returns true if symbol is an objective-c function that takes an argument
  (and (symbolp sym)
       (eq (symbol-package sym) (find-package :nextstep-functions))
       (find #\: (symbol-name sym))))

(defun find-form (first-sym in-form)
  ;; finds a form with the specified first-sym within in-form
  ;; recursing to sub-forms if necessary
  (cond ((not (listp in-form))
         nil)
        ((eq first-sym (first in-form))
         in-form)
        (t
         (some #'(lambda (sf)
                   (find-form first-sym sf))
               in-form))))

(defun objc-arg-type-string (objc-arg-type)
  (cond ((string= objc-arg-type "BOOL")
         (list "t" "nil" "#$YES" "#$NO"))
        ((string= objc-arg-type "id")
         (list "any Objective-C object"))
        ((string= objc-arg-type "SEL")
         (list "any Objective-C function name"))
        (t
         (list (concatenate 'string
                            "ns:"
                            (string-downcase (ccl::objc-to-lisp-classname objc-arg-type)))))))

(defmethod key-name (k)
  (string k))

(defmethod key-name ((k keyword))
  ;; when multiple keywords are required to be together, we combined them with an _ character
  ;; so now we just replace those characters with spaces to get the original set
  (concatenate 'string ":" (substitute #\space #\_ (string-downcase (string k)))))

(defun key-names (k)
  ;; k is a keyword symbol or list of keyword symbols. A list of strings is returned.
  (if (listp k)
    (mapcar #'key-name k)
    (list (key-name k))))

(defmethod added-class-key-values ((self objc::objc-class))
  ;; can be overridden by a class to provide information about keywords that it accepts
  ;; This default method returns nil. The method should return a list of the form:
  ;; ((<keyword> <doc-string>) ...)
  nil)

(defmethod key-doc-string (class non-keyword)
  ;; to protect from a runtime error if a developer creates (for example) a slot initarg
  ;; that isn't a keyword.
  (format nil "No information available for ~s for class ~s. First arg should be a keyword, but is not"
          non-keyword
          class))

(defmethod key-list-to-init-name ((key-list cons))
  (do* ((keys (mapcar #'(lambda (symb)
                          (format nil "~s" symb))
                      key-list)
              (rest keys))
        (next-key (concatenate 'string "init-" (subseq (first keys) 1))
                  (and keys (subseq (first keys) 1)))
        (sub-strs nil))
       ((null next-key) (apply #'concatenate 'string (nreverse sub-strs)))
    (setf next-key (string-capitalize next-key))
    (setf (elt next-key 0) (char-downcase (elt next-key 0)))
    (push (concatenate 'string (delete #\- next-key) ":") sub-strs)))

(defmethod highest-super-accepting ((self objc::objc-class) sel)
  (when (#/instancesRespondToSelector: self sel)
    (let ((sup (#/superclass self)))
      (if (eql sup (%null-ptr))
          self
          (or (highest-super-accepting sup sel)
              self)))))

(defmethod multiple-key-doc-string ((self objc::objc-class) (key-func-pair cons))
  ;; Creates documentation for a list of keywords that must be used together to trigger
  ;; a call to a particular init method for the class
  ;; Need to find highest superview that accects the method
  (let* ((sel-str (symbol-name (cdr key-func-pair)))
         (sc (highest-super-accepting self (coerce-obj sel-str :sel))))
    (if sc
        (format nil
                "See ~a documentation for legal values for initializer ~a"
                (ccl::objc-class-name-string (class-name sc))
                sel-str)
        (format nil "~a does not accept the selector ~a" (class-name self) sel-str))))

(defmethod added-class-key-values ((self symbol))
  ;; can be overridden by a class to provide information about keywords that it accepts
  ;; This default method returns nil. The method should return a list of the form:
  ;; ((<keyword> <doc-string>) ...)
  (let ((cl (find-class self nil)))
    (when cl (added-class-key-values cl))))

(defmethod with-key-p ((key keyword))
  (let ((key-str (symbol-name key)))
    (and (> (length key-str) 4)
         (string= (subseq key-str 0 4) "WITH"))))
  
(let ((key-hash (make-hash-table)))

  (defmethod key-doc-string ((self objc::objc-class) (key keyword))
    (let* ((key-strings (gethash key key-hash nil))
           (additional-key-classes (mapcar #'second (gethash (class-name self) key-hash))))
      (or (second (find-if #'(lambda (class-key-list)
                               (subtypep (class-name self) (first class-key-list)))
                           key-strings))
          (some #'(lambda (cl)
                    (key-doc-string cl key))
                additional-key-classes)
          (format nil "No information available for ~a"
                  (key-name key)))))

  (defmethod assure-added-keys-documented ((self objc::objc-class))
    (unless (member :added (gethash self key-hash))
      ;; keywords not already documented for this class
      (push :added (gethash self key-hash))
      ;; Above is valid assuming that added-class-key-values is 
      ;; always defined in the same file that the class is.
      (dolist (kv-pair  (added-class-key-values self))
        (push (list (class-name self) (second kv-pair))
              (gethash (first kv-pair) key-hash)))))

  (defmethod assure-init-keys-documented ((self objc::objc-class))
    (unless (member :init (gethash self key-hash))
      ;; init keywords not already documented for this class
      (push :init (gethash self key-hash))
      (dolist (key-func-pair  (ccl::all-init-keywords-for-class self))
        (push (list (class-name self) (multiple-key-doc-string self key-func-pair))
              (gethash (concatenated-keyword (first key-func-pair)) key-hash)))))

  (defun ht ()
    key-hash)

  (defun reload-documentation ()
    (setf key-hash (make-hash-table))
    (populate-key-hash))

  (defmethod key-values ((self objc::objc-class) &optional (keys nil))
    (if keys
      (unless (listp keys)
        (setf keys (list keys)))
      (setf keys (init-keys self :return-list t)))
    (assure-added-keys-documented self)
    (assure-init-keys-documented self)
    (dolist (key keys)
      (format t "~%~a~%   ~a~%"
              (key-name key)
              (key-doc-string self key)))
    (values))

  (defun extract-info-from-convert (form)
    (let* ((body-form (fourth form))
           (ecase-form (find-form 'ecase body-form))
           (listp-form (find-form 'listp body-form))
           (key-list (mapcar #'first (nthcdr 2 ecase-form))))
      (setf (gethash (second form) key-hash) 
            (format nil "~aone of ~{~a~^, ~}"
                    (if listp-form "list or " "")
                    (mapcan #'key-names key-list)))))
  
  (defun extract-info-from-init (form)
    ;; Assumes all key arguments are of the form (<key> init <key>-p)
    ;; Assumes all body forms are of the form 
    ;; (when <expression containing <key>-p somewhere> (#/<objc-func> <expression containing <key> somewhere>))
    (let* ((arg-form (fourth form))
           (class (find-class (second (first arg-form))))
           (and-key-pos (position '&key arg-form))
           (and-allow-pos (position '&allow-other-keys arg-form))
           (key-forms (and and-key-pos (subseq arg-form (1+ and-key-pos) (or and-allow-pos (list-length arg-form)))))
           (key-pkg (find-package :keyword))
           (key-sym-key-p-assoc (mapcar #'(lambda (kv)
                                            (cons (first kv)
                                                  (third kv)))
                                        key-forms))
           (body-forms (subseq form 4)))
      (dolist (bf body-forms)
        (when (and (listp bf) (eq (first bf) 'when))
          (let ((dash-p-list (if (listp (second bf))
                               (remove-if-not #'dash-p-sym-p (second bf))
                               (list (second bf)))))
            (dolist (dash-p dash-p-list)
              (let* ((key-sym (car (rassoc dash-p key-sym-key-p-assoc)))
                     (objc-func-form (or (recursive-find-if #'(lambda (f)
                                                                (and (listp f)
                                                                     (objc-set-function-p (first f))
                                                                     (recursive-find-if #'(lambda (sf)
                                                                                            (and (listp sf)
                                                                                                 (member key-sym sf)))
                                                                                        f)))
                                                            (third bf))
                                         (recursive-find-if #'(lambda (f)
                                                                (and (listp f)
                                                                     (objc-set-function-p (first f))))
                                                            (third bf))))
                     (objc-func-sym (first objc-func-form))
                     (objc-func-name (and objc-func-sym (symbol-name objc-func-sym)))
                     (objc-arg-type (and class objc-func-name (second (or (method-info class objc-func-name)
                                                                          (method-info nil objc-func-name)))))
                     (key-form (recursive-find-if #'(lambda (s)
                                                      (and (listp s)
                                                           (member key-sym s)))
                                                  objc-func-form))
                     (key-arg-vals (cond ((eq (first key-form) objc-func-sym)
                                          ;; key is directly used as an argument of the objc-func
                                          (objc-arg-type-string objc-arg-type))
                                         ((convert-func-p (first key-form))
                                          (list "integer or" (gethash (first key-form) key-hash)))
                                         ((eq (first key-form) 'coerce-obj)
                                          (list (or (sources-for-target-class (second (third key-form))) ;; assumes class is '<class>
                                                    (format nil "Any object that coerce-obj can transform into ~s"
                                                            (cadar (last key-form))))))
                                         ((eq (first key-form) 'coerce)
                                          (list (format nil "Any object that coerce can transform into ~s"
                                                        (cadar (last key-form)))))
                                         ((eq (first key-form) 'cgfloat)
                                          (list "Any number that can be coerced to a double-float"))
                                         ((eq (first key-form) 'get-selector)
                                          (list "String name of an Objective-C method (message)"))
                                         ((eq (first key-form) 'if)
                                          (list "t or nil"))
                                         ((null key-form)
                                          ;; best guess is that we're iterating over the key arg which must
                                          ;; be a list of things acceptable to the objc set function
                                          (list (if (null objc-arg-type)
                                                  ;; function not found ???
                                                  (format nil "list of arguments acceptable to ~a" objc-func-name)
                                                  (apply #'concatenate
                                                         'string
                                                         "list of "
                                                         (objc-arg-type-string objc-arg-type)))))
                                         (t
                                          (list (format nil "Any object acceptable as an argument to ~s" (first key-form)))))))
                (push (list (class-name class) (format nil "~{~a~^ ~}" key-arg-vals))
                       (gethash (intern (symbol-name key-sym) key-pkg) key-hash))))))))))

(defun populate-key-hash ()
  (with-open-file (v (iu::find-in-krueger-contrib "objc-initialize.lisp"))
    (do ((next-form (read v nil :eof)
                    (read v nil :eof)))
        ((eq next-form :eof))
      (cond ((convert-form-p next-form)
             (extract-info-from-convert next-form))
            ((init-form-p next-form)
             (extract-info-from-init next-form))
            (t
               nil)))))

(populate-key-hash)

(provide :dev-tools)