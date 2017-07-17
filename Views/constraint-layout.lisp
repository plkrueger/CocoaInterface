;; constraint-layout.lisp

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

Implements lisp interfaces for the Cocoa constraint-based autolayout facility. This require OSX 10.7 or later.

Basically you create views that will be put into the window content view and then add constraints that tell the
content view how to display everything. Don't set explicit frame rects anywhere; let the layout system do this for you.
You can also structure views hierarchically and set constraints at each level of the hierarchy. Constraints added
to a view must only refer to a view and its direct or indirect subviews although all subviews on the path to an
indirect view must by layed out by the constraint system and not manually or the constraint will have no effect.

There are two fundamental lisp methods ( make-constraint and make-constraints-for) that are fairly direct translations
of ObjC counterparts. There are also a number of methods that provide common constraint idioms to make interface layout easier.

Returned constraints are owned by the view to which they were added. You should not #/release them ever.

|#

(assert (>= #$NSAppKitVersionNumber #$NSAppKitVersionNumber10_7)
        nil
        "CCL version 1.9 and OSX 10.7 or higher are required to run this code")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :coerce-obj)
  (require :view-utils)
  (require :nslog-utils))

(in-package :lv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Variables

(defvar *default-space* 6)
(defvar *border-space* 20)
(defvar *no-space* 1)
(defvar *log-constraints* nil)
(defvar *rel-keywords* '(:<= := :>=))
(defvar *attr-keywords* '(:left :right :top :bottom :leading :trailing :width :height :center-x :center-y :baseline))
(defvar *op-keywords* '(:+ :* :- :/))
(defvar *constraint-symbol-names*
  (pairlis '("<=" "=" ">=" 
             "left" "right" "top" "bottom" "leading" "trailing" "width" "height" "center-x" "center-y" "baseline"
             "+" "*" "-" "/")
           (append *rel-keywords* *attr-keywords* *op-keywords*)))
(defvar *constraint-log-time* 0)
(defvar *constraint-log-count* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility methods for converting keywords to constants and other things

(defun relation-convert (rel-key)
  (case rel-key
    (:<= #$NSLayoutRelationLessThanOrEqual)
    (:= #$NSLayoutRelationEqual)
    (:>= #$NSLayoutRelationGreaterThanOrEqual)
    (t #$NSLayoutRelationEqual)))

(defun attribute-convert (att-key)
  (case att-key
    (:left #$NSLayoutAttributeLeft)
    (:right #$NSLayoutAttributeRight)
    (:top #$NSLayoutAttributeTop)
    (:bottom #$NSLayoutAttributeBottom)
    (:leading #$NSLayoutAttributeLeading)
    (:trailing #$NSLayoutAttributeTrailing)
    (:width #$NSLayoutAttributeWidth)
    (:height #$NSLayoutAttributeHeight)
    (:center-x #$NSLayoutAttributeCenterX)
    (:center-y #$NSLayoutAttributeCenterY)
    (:baseline #$NSLayoutAttributeBaseline)
    (:none #$NSLayoutAttributeNotAnAttribute)
    (t #$NSLayoutAttributeNotAnAttribute)))

(defun orientation-convert (o-key)
  (case o-key
    (:h #$NSLayoutConstraintOrientationHorizontal)
    (:v #$NSLayoutConstraintOrientationVertical)
    (t #$NSLayoutConstraintOrientationHorizontal)))

(defun priority-convert (p-key)
  (case p-key
    (:required (float #$NSLayoutPriorityRequired))
    (:high (float #$NSLayoutPriorityDefaultHigh))
    (:can-resize (float #$NSLayoutPriorityDragThatCanResizeWindow))
    (:window-stay-put (float #$NSLayoutPriorityWindowSizeStayPut))
    (:cannot-resize (float #$NSLayoutPriorityDragThatCannotResizeWindow))
    (:low (float #$NSLayoutPriorityDefaultLow))
    (:fitting-size-compression (float #$NSLayoutPriorityFittingSizeCompression))
    (t (float #$NSLayoutPriorityDefaultLow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; invisible-spacer view
;;
;; Since there doesn't seem to be any way to constrain the different the space between different objects
;; to be both adjustable and equal to the space between some other object, you can't readily use contraints
;; to distribute objects evenly over some adjustable range. So we'll play a game by creating invisible
;; objects that have very low contentHugging and CompressionResistance priorities and putting them
;; between objects that we want to distribute. If the objects at both ends of that set are anchored
;; then the size of the invisible objects will be dynamically increased or decreased as necessary.

(defmethod make-invisible-spacer ((parent-view ns:ns-view))
  (let ((v (make-instance ns:ns-view))
        (p (+ #$NSLayoutPriorityFittingSizeCompression 1.0)))
    (#/setTranslatesAutoresizingMaskIntoConstraints: v #$NO)
    (#/setContentHuggingPriority:forOrientation: v p #$NSLayoutConstraintOrientationHorizontal)
    (#/setContentHuggingPriority:forOrientation: v p #$NSLayoutConstraintOrientationVertical)
    (#/setContentCompressionResistancePriority:forOrientation: v p #$NSLayoutConstraintOrientationHorizontal)
    (#/setContentCompressionResistancePriority:forOrientation: v p #$NSLayoutConstraintOrientationVertical)
    (#/addSubview: parent-view v)
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods corresponding to ObjC methods, but also immediately add the new constraints to the parent view

(defmethod make-constraints-for (subview-assoc-list constraint-string
                                 &key 
                                 (install-view nil)
                                 (options 0)
                                 (metrics nil))
  (let* ((mets (if metrics (coerce-obj metrics 'ns:ns-dictionary) (%null-ptr)))
         (views (coerce-obj subview-assoc-list 'ns:ns-dictionary))
         (constraint-array (#/constraintsWithVisualFormat:options:metrics:views:
                            ns:ns-layout-constraint
                            (coerce-obj constraint-string 'ns:ns-string)
                            options
                            mets
                            views))
         (constraint-list (coerce-obj constraint-array 'list)))
    (when install-view
      (ccl::call-in-event-process
       #'(lambda ()
           (#/addConstraints: install-view constraint-array))))
    constraint-list))


#| Params for the method above

parent-view is a view to be designed

subview-assoc-list is an association list of the form ((<string> . <view>)*) where
<string> is the name of the view that is used in constraint strings and
<view> is the view that is named and must be a subview of parent-view

constraint-string is a string as described in “Visual Format Language” section of the "Cocoa Auto Layout Guide"

:options value is a bitmask that specifies both a part of an interface element to 
align-views and a direction for the alignment between two interface elements.

     Pick one of the following alignment bits:

     #$NSLayoutFormatAlignAllLeft
     Align all specified interface elements using NSLayoutAttributeLeft on each.

     #$NSLayoutFormatAlignAllRight
     Align all specified interface elements using NSLayoutAttributeRight on each.

     #$NSLayoutFormatAlignAllTop
     Align all specified interface elements using NSLayoutAttributeTop on each.

     #$NSLayoutFormatAlignAllBottom
     Align all specified interface elements using NSLayoutAttributeBottom on each.

     #$NSLayoutFormatAlignAllLeading
     Align all specified interface elements using NSLayoutAttributeLeading on each.

     #$NSLayoutFormatAlignAllTrailing
     Align all specified interface elements using NSLayoutAttributeTrailing on each.

     #$NSLayoutFormatAlignAllCenterX
     Align all specified interface elements using NSLayoutAttributeCenterX on each.

     #$NSLayoutFormatAlignAllCenterY
     Align all specified interface elements using NSLayoutAttributeCenterY on each.

     #$NSLayoutFormatAlignAllBaseline
     Align all specified interface elements using NSLayoutAttributeBaseline on each.

     logand the constant below if you want the constraint to specify only alignment options:

     #$NSLayoutFormatAlignmentMask
     Bitmask that can be combined with a NSLayoutFormatOptions variable to yield only the alignment portion of the format options.

     Pick only one of the following direction options:

     #$NSLayoutFormatDirectionLeadingToTrailing
     Arrange objects in order based on the normal text flow for the current user interface language. 
     In English this results in the first object being placed farthest to the left, the next one to its right, and so on. 
     In right to left languages this ordering is reversed.

     #$NSLayoutFormatDirectionLeftToRight
     Arrange objects in order from left to right.

     #$NSLayoutFormatDirectionRightToLeft
     Arrange objects in order from right to left.

     logand the constant below if you want the constraint to only specify directional relationships, not alignment

     #$NSLayoutFormatDirectionMask
     Bitmask that can be combined with a NSLayoutFormatOptions variable to yield only the direction portion of the format options.

:metrics value should be an association list of the form ((<string> . <number>)*)
These strings can be used as constants in the constraint-string

The method returns a list of the constraints added in case you want to interact with them further. The items in the
list need not be retained or released. They will be valid until they are removed from the parent view or the view itself
goes away.

|#

(defun make-constraint (&key
                        (install-view nil install-view-provided)
                        (priority nil)
                        item1
                        (att1 :width)
                        (relation :=)
                        (item2 nil)
                        (att2 nil) ;; defaults to att1 if item2 is not null
                        (mult 1)
                        (const 0))
  (unless install-view-provided
    (setf install-view (common-superview item1 item2)))
  (when (and (view-p item1)
             install-view
             (not (eql item1 install-view))
             (#/translatesAutoresizingMaskIntoConstraints item1))
    ;; make sure no automatic constraints are used for this view
    (#/setTranslatesAutoresizingMaskIntoConstraints: item1 #$NO))
  (when (and (view-p item2)
             install-view
             (not (eql item2 install-view))
             (#/translatesAutoresizingMaskIntoConstraints item2))
    ;; make sure no automatic constraints are used for this view
    (#/setTranslatesAutoresizingMaskIntoConstraints: item2 #$NO))
  (let* ((rel (relation-convert relation))
         (a1 (attribute-convert att1))
         (a2 (cond (att2 (attribute-convert att2))
                   ((or (null item2) (eql item2 (%null-ptr))) (attribute-convert :none))
                   (t a1)))
         (constraint (#/constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:
                      ns:ns-layout-constraint
                      item1
                      a1
                      rel
                      (or item2 (%null-ptr))
                      a2
                      (cgfloat mult)
                      (cgfloat const))))
    (when (numberp priority)
      (when *log-constraints*
        (ns-log-format "Setting constraint priority to ~s" priority))
      (#/setPriority: constraint (float priority)))
    (when *log-constraints*
      ;; max of 500 log messages per second before they are dropped
      ;; Assume roughly 3-to-1 ratio between all other log entries and this one
      (let ((now (now)))
        (when (> now *constraint-log-time*)
          (setf *constraint-log-count* 0)
          (setf *constraint-log-time* now))
        (when (> (incf *constraint-log-count*) 125)
          (sleep 1)
          (setf *constraint-log-time* (now))
          (setf *constraint-log-count* 0)))
      (let* ((left-str (format nil "Constraint: (~a (~a ~s) "
                               (symbol-name relation)
                               (symbol-name att1)
                               item1))
             (right-str (unless (or (null item2) (eql item2 (%null-ptr)))
                          (format nil "(~a ~s)"
                                  (symbol-name (or att2 att1)) item2)))
             (mult-str (if (and right-str (not (eql mult 1)))
                         (format nil "(* ~s ~a)" mult right-str)
                         right-str))
             (const-str (if mult-str
                          (if (zerop const)
                            (format nil "~a)" mult-str)
                            (format nil "(+ ~s ~a))" const mult-str))
                          (format nil "~s)" const))))
        (ns-log (concatenate 'string left-str const-str))))
    (when (view-p install-view)
      (when *log-constraints*
        (ns-log-format "Adding constraint to ~s" install-view))
      (ccl::call-in-event-process
       #'(lambda ()
           (#/addConstraint: install-view constraint))))
      constraint))

#|

NOTE:
Unfortunately, there is currently no way to set the priority of a constraint created this way. You can set
expansion and compression priorities for individual views, but if you must set the priority for any other
constraint, then you need to use the make-constraints-for method above and supply a string format which has
the necessary syntax for specifying constraint priorities.

Examples:

(make-constraint :install-view parent-view :item1 v1 :item2 v2)
constrains the width of v1 to be the same as the width of v2; v1 and v2 must be subviews of parent-view.

(make-constraint :install-view parent-view :item1 v1 :item2 v2 :att1 :height)
constrains the height of v1 to be the same as the height of v2; v1 and v2 must be subviews of parent-view.

(make-constraint :install-view parent-view :item1 v1 :att1 :leading :item2 parent-view :const 20)
constrains the leading edge of v1 (left edge in English systems) to be 20 points greater than the 
left edge of the parent view; v1 must be a subview of parent-view. That will keep v1 nailed to the left
edge of the parent view.

|#

(defun flip-rel (rel)
  (ecase rel
    (:= :=)
    (:>= :<=)
    (:<= :>=)))

(defun rel-keyword (sym)
  (let ((key (constraint-keyword sym)))
    (and key (find key *rel-keywords*))))

(defun attr-keyword (sym)
  (let ((key (constraint-keyword sym)))
    (and key (find key *attr-keywords*))))

(defmethod constraint-keyword (sym)
  (and (symbolp sym)
       (cdr (assoc (symbol-name sym)
                   *constraint-symbol-names*
                   :test #'string-equal))))

(defun arg-type (arg-expr)
  (if (and (consp arg-expr) (keywordp (first arg-expr)))
    (first arg-expr)
    :lisp-expr))

(defun negate (expr)
  (case (arg-type expr)
    (:mult
     (let* ((pos (position :mult expr))
            (mult (nth (1+ pos) expr))
            (neg (list '- mult)))
       (nsubst neg mult expr)))
    (:right
     (list* :mult :mult -1 (rest expr)))
    (:constant
     (let* ((pos (position :const expr))
            (const (nth (1+ pos) expr))
            (neg (list '- const)))
       (nsubst neg const expr)))
    (t
     nil)))

(defun invert (expr)
  (case (arg-type expr)
    (:mult-const
     `(:mult-const :mult (/ 1.0 ,(caddr expr))))
    (t
     nil)))

(defmethod parse-constraint-form ((cons-form list))
  ;; constraint ::= ( <relation> <left> <sum-expr> ) | ( <relation> <sum-expr> <left> )
  (unless (eql (list-length cons-form) 3)
    (ns-log-format "Constraint form ~s must have 3 subexpressions" cons-form)
    (return-from parse-constraint-form nil))
  (let* ((rel (first cons-form))
         (rel-key (or (rel-keyword rel) rel))
         (a2 (or (parse-left (second cons-form))
                 (parse-sum (second cons-form))))
         (a2-type (arg-type a2))
         (a3 (if (eq a2-type :left)
               (parse-sum (third cons-form))
               (parse-left (third cons-form)))))
    (and a2 a3 (append (list :relation (if (eq a2-type :left)
                                         rel-key
                                         `(flip-rel ,rel-key)))
                       (rest a2)
                       (rest a3)))))

(defmethod parse-left ((left-expr list))
  ;; left ::= ( <attribute> <view> )
  (when (eql (list-length left-expr) 2)
    (let* ((attr-expr (first left-expr))
           (attr (or (attr-keyword attr-expr)
                     attr-expr))
           (view (second left-expr)))
      (unless (and (symbolp attr) (fboundp attr))
        (list :left
              :att1 attr
              :item1 view)))))

(defmethod parse-sum (sum-expr)
  ;; sum ::= ( + <const-expr> <mult-expr> ) | ( + <mult-expr> <const-expr> ) | <mult-expr> | <const-expr>
  ;; not a list, so treat it as a constant
  (parse-const sum-expr))

(defmethod parse-sum ((sum-expr list))
  ;; sum ::= ( + <const-expr> <mult-expr> ) | ( + <mult-expr> <const-expr> ) | 
  ;;         ( - <const-expr> <mult-expr> ) | ( - <mult-expr> <const-expr> ) | <mult-expr> | <const-expr>
  (let ((a1 (constraint-keyword (first sum-expr))))
    (cond ((eq a1 :+)
           (let* ((a2 (or (parse-mult (second sum-expr))
                          (parse-const (second sum-expr))))
                  (a3 (if (eq (arg-type a2) :constant)
                        (parse-mult (third sum-expr))
                        (parse-const (third sum-expr)))))
             (and a2 a3 (append (list :right) (rest a2) (rest a3)))))
          ((eq a1 :-)
           (let* ((a2 (or (parse-mult (second sum-expr))
                          (parse-const (second sum-expr))))
                  (a3 (if (eq (arg-type a2) :constant)
                        (negate (parse-mult (third sum-expr)))
                        (negate (parse-const (third sum-expr))))))
             (and a2 a3 (append (list :right) (rest a2) (rest a3)))))
          (t
           (parse-mult sum-expr)))))

(defmethod parse-mult ((mult-expr list))
  ;; mult ::= ( * <mult-const> <right> ) | ( * <right> <mult-const> ) |
  ;;          ( / <right> <mult-const> ) | <right>
  ;; Note that dividing BY a <right> form cannot be converted into the form
  ;; demanded by Apple's constraint specification.
  (let ((a1 (constraint-keyword (first mult-expr))))
    (cond ((eq a1 :*)
           (let* ((a2 (or (parse-right (second mult-expr))
                          (parse-mult-const (second mult-expr))))
                  (a3 (if (eq (arg-type a2) :mult-const)
                        (parse-right (third mult-expr))
                        (parse-mult-const (third mult-expr)))))
             (and a2 a3 (append (list :mult) (rest a2) (rest a3)))))
          ((eq a1 :/)
           (let* ((a2 (parse-right (second mult-expr)))
                  (a3 (invert (parse-mult-const (third mult-expr)))))
             (and a2 a3 (append (list :mult) (rest a2) (rest a3)))))
          (t
           (or (parse-right mult-expr)
               (parse-const mult-expr))))))

(defmethod parse-mult (expr)
  (declare (ignore expr))
  ;; not a list, so return nil
  nil)

(defmethod parse-right (right-expr)
  (declare (ignore right-expr))
  ;; not a list so return nil
  nil)

(defmethod parse-right ((right-expr list))
  ;; right ::= ( <attribute> <view> ) 
  (when (eql (list-length right-expr) 2)
    (let* ((attr-expr (first right-expr))
           (attr (or (attr-keyword attr-expr)
                     attr-expr))
           (view (second right-expr)))
      (unless (and (symbolp attr) (fboundp attr))
        (list :right
              :att2 attr
              :item2 view)))))

(defun parse-mult-const (c)
  (list :mult-const :mult c))

(defun parse-const (c)
  (list :constant :const c))

(defmacro constrain (lisp-constraint-form &rest other-keys)
  (let ((key-list (parse-constraint-form lisp-constraint-form)))
    (if key-list
      `(progn
         (when *log-constraints*
           (ns-log-format "Starting constrain"))
         (make-constraint ,@key-list ,@other-keys))
      `(ns-log-format "Invalid input to constrain: ~s" ,lisp-constraint-form))))

#|
Examples
        
(constrain (= (width x) (width y)))
(constrain (>= (height x) (+ (* (width y) 4) 5)))
(constrain (<= (height x) (+ (width y) 5)))
(constrain (= (center-x x) (* (center-y y) 6)))

|#

(defmethod remove-constraints ((parent-view ns:ns-view) constraint-list)
  (#/removeConstraints: parent-view (lisp-to-ns-array constraint-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods that make layouts easier

(defmethod constrain-size ((sized-view ns:ns-view)
                           &key
                           (install-view sized-view)
                           (priority nil)
                           (min-width nil)
                           (max-width nil)
                           (min-height nil)
                           (max-height nil)
                           (width nil)
                           (height nil))
  (when *log-constraints*
      (ns-log (format nil "Starting constrain-size")))
  (let ((constraints nil))
    (when (numberp min-width)
      (push (constrain (>= (width sized-view) min-width) :install-view install-view :priority priority)
            constraints))
    (when (numberp max-width)
      (push (constrain (<= (width sized-view) max-width)  :install-view install-view :priority priority)
            constraints))
    (when (numberp min-height)
      (push (constrain (>= (height sized-view) min-height) :install-view install-view :priority priority)
            constraints))
    (when (numberp max-height)
      (push (constrain (<= (height sized-view) max-height) :install-view install-view :priority priority)
            constraints))
    (when (numberp width)
      (push (constrain (= (width sized-view) width) :install-view install-view :priority priority)
            constraints))
    (when (numberp height)
      (push (constrain (= (height sized-view) height) :install-view install-view :priority priority)
            constraints))
    constraints))

(defmethod constrain-to-natural-size ((sized-view ns:ns-view)
                                      &key
                                      (priority nil)
                                      (install-view sized-view)
                                      (rel :=))
  ;; This should only be called if you're sure that the size of the sized-view has been
  ;; set somehow. This will then constrain it to be that size.
  ;; If called on an object that has already set internal constraints, then it will do nothing
  (when *log-constraints*
      (ns-log-format "Starting constrain-to-natural-size"))
  (cond ;;((and (responds-to-selector sized-view "sizeToFit")
              ;;(responds-to-selector sized-view "stringValue")
              ;;(plusp (length (coerce-obj (#/stringValue sized-view) 'string))))
         ;;(when *log-constraints*
           ;;(ns-log-format "Calling #/sizeToFit for ~s" sized-view))
         ;;(#/sizeToFit sized-view))
        ((coerce-obj (#/constraints sized-view) 'list)
         (ns-log-format "Ignoring constrain-to-natural-size because ~s is internally constrained" sized-view))
        (t
         (let* ((min-size (natural-size sized-view))
                (width (ns:ns-size-width min-size))
                (height (ns:ns-size-height min-size)))
           ;; since we're going to add our own constraints, turn off the auto
           (#/setTranslatesAutoresizingMaskIntoConstraints: sized-view #$NO)
           (case rel
             (:=
              (constrain-size sized-view
                              :install-view install-view
                              :priority priority
                              :width width
                              :height height))
             (:>=
              (constrain-size sized-view
                              :install-view install-view
                              :priority priority
                              :min-width width
                              :min-height height))
             (:<=
              (constrain-size sized-view
                              :install-view install-view
                              :priority priority
                              :max-width width
                              :max-height height)))))))
  
(defmethod constrain-to-natural-width ((sized-view ns:ns-view)
                                       &key
                                       (priority nil)
                                       (install-view sized-view)
                                       (rel :=))
  ;; This should only be called if you're sure that the width of the sized-view has been
  ;; set somehow. This will then constrain it to be that width.
  (when *log-constraints*
      (ns-log (format nil "Starting constrain-to-natural-width")))
  (if (coerce-obj (#/constraints sized-view) 'list)
    (ns-log-format "Ignoring constrain-to-natural-width because ~s is internally constrained" sized-view)
    (let* ((min-size (natural-size sized-view))
           (width (ns:ns-size-width min-size)))
      ;; since we're going to add our own constraints, turn off the auto
      (#/setTranslatesAutoresizingMaskIntoConstraints: sized-view #$NO)
      (case rel
        (:=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :width width))
        (:>=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :min-width width))
        (:<=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :max-width width))))))

(defmethod constrain-to-natural-height ((sized-view ns:ns-view)
                                        &key
                                        (priority nil)
                                        (install-view sized-view)
                                        (rel :=))
  ;; This should only be called if you're sure that the height of the sized-view has been
  ;; set somehow. This will then constrain it to be that height.
  (when *log-constraints*
      (ns-log (format nil "Starting constrain-to-natural-height")))
  (if (coerce-obj (#/constraints sized-view) 'list)
    (ns-log-format "Ignoring constrain-to-natural-height because ~s is internally constrained" sized-view)
    (let* ((min-size (natural-size sized-view))
           (height (ns:ns-size-height min-size)))
      ;; since we're going to add our own constraints, turn off the auto
      (#/setTranslatesAutoresizingMaskIntoConstraints: sized-view #$NO)
      (case rel
        (:=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :height height))
        (:>=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :min-height height))
        (:<=
         (constrain-size sized-view
                         :install-view install-view
                         :priority priority
                         :max-height height))))))

(defmethod constrain-size-relative-to ((sized-view ns:ns-view) (relative-view ns:ns-view)
                                       &key
                                       (priority nil)
                                       (install-view (common-superview sized-view relative-view))
                                       (rel :=)
                                       (width t)
                                       (height t))
  ;; constrains sized-view relative to the size of relative-view
  ;; rel should be one of the keywords := :<= :>=
  (when *log-constraints*
      (ns-log (format nil "Starting constrain-size-relative-to")))
  (let ((constraints nil))
    (when width
      (push (constrain (rel (width sized-view) (width relative-view)) :install-view install-view :priority priority)
            constraints))
    (when height
      (push (constrain (rel (height sized-view) (height relative-view)) :install-view install-view :priority priority)
            constraints))
    constraints))
  
(defmethod anchor ((parent-view ns:ns-view) (anchored-view ns:ns-view) key-list
                   &key
                   (priority nil)
                   (install-view parent-view)
                   (margin *border-space*)
                   (rel :=))
  ;; key-list is a list containing any combination of :leading :trailing :top :bottom :center-x :center-y
  ;; (:left :right) can be used instead of :leading and :trailing respectively wherever left-to-right is 
  ;; the normal order.
  ;; Makes a constraint for each border to which anchored-view should adhere
  (when *log-constraints*
      (ns-log (format nil "Starting anchor")))
  (let ((constraints nil))
    (dolist (key key-list constraints)
      (push (constrain (rel (key anchored-view)
                            (+ (key parent-view)
                               (cond ((member key '(:left :leading :top))
                                      margin)
                                     ((member key '(:right :trailing :bottom))
                                      (- margin))
                                     (t
                                      0))))
                       :install-view install-view
                       :priority priority)
            constraints))))

(defmethod anchor-window ((win ns:ns-window) orientation fixed-point)
  ;; When window is resized in the direction specified by orientation, the fixed-point will remain stationary
  (#/setAnchorAttribute:forOrientation: win (attribute-convert fixed-point) (orientation-convert orientation)))

(defmethod align-views (&key
                        (install-view nil install-view-provided)
                        (priority nil)
                        (align :center-x) 
                        (views nil))
  ;; Add constraints to the parent-view to align the views according to alignment type
  ;; which should be one of :left :right :top :bottom :leading :trailing :center-x :center-y :baseline
  ;; views must all be descendants of parent-view (which includes parent-view itself)
  (when *log-constraints*
      (ns-log (format nil "Starting align-views")))
   (unless (or install-view-provided (null views))
    ;; assume install-view is the common ancestor to all views
    (setf install-view (joint-superview views)))
  (do* ((constraints nil)
        (sv views
            (rest sv))
        (v1 (first sv) (first sv))
        (v2 (second sv) (second sv))
        (v3 (third sv) (third sv)))
       ((null v2) constraints)
    (cond ((and (view-p v1) (view-p v2))
           (push (constrain (= (align v1) (align v2)) :install-view install-view :priority priority)
                 constraints))
          ((and (view-p v1) (view-p v3))
            (push (constrain (= (align v1) (align v3)) :install-view install-view :priority priority)
                  constraints))
          (t
           nil))))

(defmethod constrain-to-array (col-count
                               &key
                               (install-view nil install-view-provided)
                               (h-space *default-space*)
                               (v-space *default-space*)
                               (h-align :top)
                               (v-align :leading)
                               (priority nil)
                               (views nil))
  ;; Layout the views within the parent in the number of rows required for the
  ;; number of columns specified. The :views argument should be a list of views
  ;; with no embedded numbers for spacing. If present, such numbers will be ignored.
  ;; This could be done trivially if we could specify positional constraints relative
  ;; to the size of the parent, but unfortunately that is not permitted. Constraints
  ;; must relate sizes to other sizes or positions to other positions. So ...
  ;; We compute the largest width of any view and the largest height of any view
  ;; and assume that is the size of each array element. We use the natural-size of
  ;; the views to make that determination, which will always be non-zero, although it 
  ;; may not be correct. When in doubt, make sure that the views have reasonable non-zero frame rectangle
  ;; frame rectangle values.
  (when *log-constraints*
      (ns-log (format nil "Starting row-col-array-views")))
   (unless (or install-view-provided (null views))
    ;; assume install-view is the common ancestor to all views
    (setf install-view (joint-superview views)))
  (let* ((constraints nil)
         (real-views (remove-if-not #'view-p views))
         (max-width (reduce #'max real-views :key #'natural-view-width))
         (max-height (reduce #'max real-views :key #'natural-view-height)))
    ;; Order and align objects in each row
    (dolist (row-list (sequential-sublists real-views col-count))
      (setf constraints 
            (nconc constraints
                   (order-views :orientation :h
                                :views row-list
                                :from v-align
                                :to v-align
                                :spacing (+ h-space max-width)
                                :install-view install-view
                                :priority priority
                                :align h-align))))
          
    ;; Order and align objects in each column
    (dolist (col-list (every-n-sublists real-views col-count))
      (setf constraints
            (nconc constraints
                   (order-views :orientation :v
                                :views col-list
                                :from h-align
                                :to h-align
                                :spacing (+ v-space max-height)
                                :install-view install-view
                                :priority priority
                                :align v-align))))
    constraints))
    
(defmethod order-views (&key
                        (install-view nil install-view-provided)
                        (priority nil)
                        (orientation :h)
                        (spacing nil)
                        (from nil)
                        (to nil)
                        (rel :=)
                        (align nil)
                        (views nil))
  ;; orientation must be :v or :h
  ;; views can contain subviews, numbers that specify spacing in points, or keywords
  ;; :border :no-space :default.
  ;; When views are not separated by any keyword, the spacing param value is used if provided
  ;; otherwise :default is assumed.
  ;; This method results in a precise layout of objects with fixed amount of space between them.
  ;; By default the distance is measured from trailing to leading in the horizontal dimension
  ;; and from bottom to top in the vertical dimension. Other ways to measure may be specified
  ;; by providing :from and :to arguments. For example, to measure :leading to :leading you
  ;; could specify :from as :leading. Or to measure center to center in for a vertical layout
  ;; you could specify both :from and :to as :center-y. If :to and/or :from are specified, then
  ;; either :spacing should also be specified or explicit distances should be included in the 
  ;; :views list since default spacing is only appropriate for default :to and :from.
  ;; Numbers at the beginning or end of the views list establish distances between the appropriate
  ;; parent view and view boundaries (i.e. ignoring any :to and :from specification).
  ;; To order within the parent-view, anchoring the views to an edge, either the first
  ;; or last element of views (or both) should be a number which specifies the distance to
  ;; the parent. Do not explicity include the parent object in the view
  ;; list because the constraint created by doing so will not be correct.
  ;; If the install-view is not specified, but the first and/or last of the views is already
  ;; a subview of some other view, then that view is assumed to be a valid parent-view for 
  ;; purposes of ordering the views relative to that parent (i.e. if the first or last element
  ;; of the views list is a number).
  ;; The align argument, if provided, causes the ordered views to also be aligned as specified.
  ;; See the align-view method for valid values of the align argument.
  (when *log-constraints*
      (ns-log (format nil "Starting order-views")))
   (unless (or install-view-provided (null views))
    ;; assume install-view is the common ancestor to all views
    (setf install-view (joint-superview views)))
  (do* ((constraints (when align
                       (align-views :install-view install-view :align align :views views)))
        (default-from (or from (if (eq orientation :v) :bottom :trailing)))
        (default-to (or to (if (eq orientation :v) :top :leading)))
        (att1 default-from
              default-from)
        (att2 default-to
              default-to)
        (v1 nil)
        (v2 nil)
        (space nil)
        (sv views)
        (sv1 (first sv)
             (first sv))
        (sv2 (second sv)
             (second sv))
        (sv3 (third sv)
             (third sv))
        (parent-view (or install-view (obj-if-not-null (#/superview v1))))
        (valid-constraint t
                          t))
       ((null sv2) constraints)
    (cond ((not (view-p sv1))
           (if parent-view
             (progn
               (setf v1 parent-view)
               (setf v2 sv2)
               (setf att1 (if (eq orientation :v) :top :leading))
               (setf att2 (if (eq orientation :v) :top :leading))
               (setf space  (cond ((numberp sv1) sv1)
                                  ((eq sv1 :border) *border-space*)
                                  ((eq sv1 :no-space) *no-space*)
                                  (t *default-space*)))
               (setf sv (rest sv)))
             (progn
               (ns-log-format ":install-view must be specified to order-views to order relative to parent")
               (setf valid-constraint nil))))
          ((view-p sv2)
           (setf v1 sv1)
           (setf v2 sv2)
           (setf space (or spacing *default-space*))
           (setf sv (rest sv)))
          ((view-p sv3)
           (setf v1 sv1)
           (setf v2 sv3)
           (setf space  (cond ((numberp sv2) sv2)
                              ((eq sv2 :border) *border-space*)
                              ((eq sv2 :no-space) *no-space*)
                              (t *default-space*)))
           (setf sv (nthcdr 2 sv)))
          ((null sv3)
           (setf v1 sv1)
           (setf v2 parent-view)
           (setf att1 (if (eq orientation :v) :bottom :trailing))
           (setf att2 (if (eq orientation :v) :bottom :trailing))
           (setf space  (cond ((numberp sv2) sv2)
                              ((eq sv2 :border) *border-space*)
                              ((eq sv2 :no-space) *no-space*)
                              (t *default-space*)))
           (setf sv (nthcdr 2 sv)))
          (t
           (ns-log (format nil "~s is not a valid input to #'order-views" views))
           (setf sv nil)
           (setf valid-constraint nil)))
    (when valid-constraint
      (push (constrain (rel (att1 v1) (+ (att2 v2) (- space))) :install-view install-view :priority priority)
            constraints))))

(defmethod distribute-views (&key
                             (install-view nil install-view-provided)
                             (priority nil)
                             (orientation :h)
                             (min-space 2.0)
                             (views nil))
  ;; Acts like ordering the subviews except that invisible-spacer views are inserted
  ;; between views. Default minimal spacing is 2 points.
  ;; Typically you might constrain the extreme positions of the end objects in the views and let the layout
  ;; system determine the rest, which it will do by uniformly expanding the size of the invisible spacers. 
  ;; The method distribute-relative-to will do all of that anchoring for you.
  (when *log-constraints*
      (ns-log (format nil "Starting distribute-views")))
   (unless (or install-view-provided (null views))
    ;; assume install-view is the common ancestor to all views
    (setf install-view (joint-superview views)))
  (when (view-p install-view)
    (dolist (v views)
      ;; make sure all views are in the install-view
      (when (and (typep v 'ns:ns-view) (not (#/isDescendantOf: v install-view)))
        (#/addSubview: install-view v))))
  (let ((constraints nil)
        (att1 (if (eq orientation :v) :bottom :trailing))
        (att2 (if (eq orientation :v) :top :leading))
        (att3 (if (eq orientation :v) :height :width))
        (spacing (/ min-space 2.0)))
    (do* ((sv views
              (rest sv))
          (prev-v2 nil
                   v2)
          (v1 (first sv)
              (first sv))
          (v3 (second sv)
              (second sv))
          (v2 (and v3 (make-invisible-spacer install-view))
              (and v3 (make-invisible-spacer install-view))))
         ((null v3) constraints)
      ;; constrain position of first view (v1) to the spacer (v2)
      (push (constrain (= (att1 v1) (+ (att2 v2) spacing)) :install-view install-view :priority priority)
            constraints)
      ;; constrain position of the spacer (v2) to the second view (v3)
      (push (constrain (= (att1 v2) (+ (att2 v3) spacing)) :install-view install-view :priority priority)
            constraints)
      ;; constrain the size of all spacers to be the same in the relevant dimension
      (when prev-v2
        (push (constrain (= (att3 v2) (att3 prev-v2)) :install-view install-view :priority priority)
              constraints)))))

(defmethod center-in-view ((parent-view ns:ns-view) (centered-view ns:ns-view)
                           &key
                           (install-view parent-view)
                           (priority nil))
  ;; constrains the centered-view to be right in the middle of its parent view
  (when *log-constraints*
      (ns-log (format nil "Starting center-in-view")))
  (nconc (align-views :install-view install-view
                      :priority priority
                      :align :center-x
                      :views (list parent-view centered-view))
         (align-views :install-view install-view
                      :priority priority
                      :align :center-y
                      :views (list parent-view centered-view))))
         
(defmethod center-relative-to ((centered-on-view ns:ns-view)
                               &key
                               (install-view (superview centered-on-view))
                               (priority nil)
                               (pos :below)
                               (min-dist *default-space*)
                               (dist nil)
                               (views nil)
                               (align nil))
  ;; The easiest way to center a collection of views that are possibly of 
  ;; different sizes is to put them into a separate view and then center that view on
  ;; the centerd-on-view. If the views are already inside the install-view, they will be moved
  ;; to be inside the new box view. Hopefully this will not disrupt any other constrainsts
  ;; already placed on those views. If install-view is not supplied, it will be necessary
  ;; for the caller to add the new box (second value returned) to the appropriate parent
  ;; view before installing the constraints.
  (when *log-constraints*
      (ns-log (format nil "Starting center-relative-to")))
  (let* ((orientation nil)
         (align-key nil)
         (att1 nil)
         (att2 nil)
         (rel1 nil)
         (att3 nil)
         (att4 nil)
         (const nil)
         (const2 nil)
         (box nil)
         (constraints nil))
    ;; set various constraint params according to keyword values
    (case pos
      ((:before :left :leading)
       (setf orientation :v)
       (setf align-key :trailing)
       (setf att1 :center-y)
       (setf att2 :center-y)
       (setf rel1 :<=)
       (setf att3 :trailing)
       (setf att4 :leading)
       (setf const (- min-dist))
       (setf const2 (when dist (- dist))))
      ((:after :right :trailing)
       (setf orientation :v)
       (setf align-key :leading)
       (setf att1 :center-y)
       (setf att2 :center-y)
       (setf rel1 :>=)
       (setf att3 :leading)
       (setf att4 :trailing)
       (setf const min-dist)
       (setf const2 dist))
      ((:above :top)
       (setf orientation :h)
       (setf align-key :bottom)
       (setf att1 :center-x)
       (setf att2 :center-x)
       (setf rel1 :<=)
       (setf att3 :bottom)
       (setf att4 :top)
       (setf const (- min-dist))
       (setf const2 (when dist (- dist))))
      ((:below :bottom)
       (setf orientation :h)
       (setf align-key :top)
       (setf att1 :center-x )
       (setf att2 :center-x)
       (setf rel1 :>=)
       (setf att3 :top)
       (setf att4 :bottom)
       (setf const min-dist)
       (setf const2 dist))
      (t
       (ns-log (format nil
                       "~s must be :before :left :leading :after :right :trailing :above or :below in #'distribute-relative-to"
                       pos))
       (return-from center-relative-to nil)))

    ;; make an invisible box
    (setf box (make-instance 'organized-box-view
                :views views
                :orientation orientation
                :align (or align align-key)))
    (setf constraints
          (nconc constraints (ns-to-lisp-list (#/constraints box))))
    (when *log-constraints*
      (ns-log (format nil "Resuming center-relative-to")))
      
    ;; constrain the middle of the invisible box relative to the centered-on-view
    (push (constrain (= (att1 box) (att2 centered-on-view)) :install-view install-view :priority priority)
          constraints)

    ;; constrain the invisible box to be in the right position relative to centered-on-view
    (push (if dist
            ;; exact distance between view box and centered-on-box requested
            (constrain (= (att3 box) (+ (att4 centered-on-view) const2))
                       :install-view install-view
                       :priority priority)
            ;; minimum distance between view box and centered-on-box requested
            (constrain (rel1 (att3 box) (+ (att4 centered-on-view) const))
                       :install-view install-view
                       :priority priority))
          constraints)
    
    ;; return the new box as a second value because if the views and constraints have not yet 
    ;; been added to some view (:install-view is nil) it will be necessary for the caller to
    ;; do both of those things.
    (values constraints box)))

(defmethod distribute-relative-to ((centered-on-view ns:ns-view)
                                   &key
                                   (install-view (superview centered-on-view))
                                   (priority nil)
                                   (pos :below)
                                   (min-dist *default-space*)
                                   (views nil)
                                   (align nil))
  ;; Does distribute-views and also adds constraints on the beginning and end objects in the views list
  ;; so that they line up with the centered-on-view.
  ;; pos can be one of :before :after :above :below
  (when *log-constraints*
      (ns-log (format nil "Starting distribute-relative-to")))
  (when install-view
    ;; make sure all views are in the install-view
    (dolist (v views)
      (when (and (typep v 'ns:ns-view) (not (#/isDescendantOf: v install-view)))
        (#/addSubview: install-view v))))
  (let ((dist-orientation nil)
        (align-key nil)
        (att1 nil)
        (att2 nil)
        (rel nil)
        (att3 nil)
        (att4 nil)
        (const nil)
        (v1 (first views))
        (v2 (first (last views)))
        (constraints nil))
    (case pos
      ((:before :left :leading)
       (setf dist-orientation :v)
       (setf align-key :trailing)
       (setf att1 :top)
       (setf att2 :bottom)
       (setf rel :<=)
       (setf att3 :trailing)
       (setf att4 :leading)
       (setf const (- min-dist)))
      ((:after :right :trailing)
       (setf dist-orientation :v)
       (setf align-key :leading)
       (setf att1 :top)
       (setf att2 :bottom)
       (setf rel :>=)
       (setf att3 :leading)
       (setf att4 :trailing)
       (setf const min-dist))
      ((:above :top)
       (setf dist-orientation :h)
       (setf align-key :bottom)
       (setf att1 :leading)
       (setf att2 :trailing)
       (setf rel :<=)
       (setf att3 :bottom)
       (setf att4 :top)
       (setf const (- min-dist)))
      ((:below :bottom)
       (setf dist-orientation :h)
       (setf align-key :top)
       (setf att1 :leading)
       (setf att2 :trailing)
       (setf rel :>=)
       (setf att3 :top)
       (setf att4 :bottom)
       (setf const min-dist))
      (t
       (ns-log (format nil
                       "~s must be :before :left :leading :after :right :trailing :above or :below in #'distribute-relative-to"
                       pos))
       (return-from distribute-relative-to nil)))

    ;; distribute the subviews
    (setf constraints (distribute-views :install-view install-view
                                        :priority priority
                                        :orientation dist-orientation
                                        :views views))
    (when *log-constraints*
      (ns-log (format nil "Resuming distribute-relative-to")))

    ;; aligns the views as user has specified or by default trailing edge if to the left, leading edge if to the right,
    ;; top edge if below, and bottom edge if above)
    (setf constraints (nconc constraints
                             (align-views :install-view install-view
                                          :priority priority
                                          :align (or align align-key)
                                          :views views)))
    (when *log-constraints*
      (ns-log (format nil "Resuming distribute-relative-to")))

    ;; constrains the first view in the views list to the centered-on-view
    (push (constrain (= (att1 v1) (att1 centered-on-view)) :install-view install-view :priority priority)
          constraints)
    ;; constrains the last view in the views list to the centered-on-view
    (push (constrain (= (att2 v2) (att2 centered-on-view)) :install-view install-view :priority priority)
          constraints)
    ;; constrains each view in the views list to be in the right relative position to centered-on-view
    (dolist (v views)
      (push (constrain (rel (att3 v) (+ (att4 centered-on-view) const)) :install-view install-view :priority priority)
            constraints))
    constraints))

(defmethod expansion-resistance-priority ((v ns:ns-view) &optional (orientation :h))
  (#/contentHuggingPriorityForOrientation: v (orientation-convert orientation)))

(defmethod (setf expansion-resistance-priority) (priority (v ns:ns-view) &optional (orientation :h))
  (#/setContentHuggingPriority:forOrientation: v
                                               (priority-convert priority)
                                               (orientation-convert orientation)))

(defmethod compression-resistance-priority ((v ns:ns-view) &optional (orientation :h))
  (#/contentCompressionResistancePriorityForOrientation: v (orientation-convert orientation)))

(defmethod (setf compression-resistance-priority) (priority (v ns:ns-view) &optional (orientation :h))
  (#/setContentCompressionResistancePriority:forOrientation: v
                                                             (priority-convert priority)
                                                             (orientation-convert orientation)))

(defmethod prioritize-expansion-resistance ((parent-view ns:ns-view) orientation &rest subviews)
  ;; Sets each view's ContentHuggingPriorityForOrientation so that things later in the sub-views
  ;; list are expanded before things earlier in the list. If multiple things should be expanded at the
  ;; same relative priority, enclose them in a list and they will all be assigned the same priority.
  ;; The existing priority of the first view listed is the starting priority.
  (dolist (v subviews)
    ;; make sure all views are in the parent-view
    (when (and (typep v 'ns:ns-view) (not (#/isDescendantOf: v parent-view)))
      (#/addSubview: parent-view v)))
  (do* ((orient (orientation-convert orientation))
        (priority (#/contentHuggingPriorityForOrientation: (if (listp (first subviews)) 
                                                             (caar subviews)
                                                             (first subviews))
                                                           orient)
                  (float (decf priority)))
        (views subviews (rest views))
        (nextv (first views) (first views)))
       ((null nextv) priority)
    (cond ((listp nextv)
           (dolist (v nextv)
             (when (view-p v)
               (#/setContentHuggingPriority:forOrientation: v priority orient))))
          ((view-p nextv)
           (#/setContentHuggingPriority:forOrientation: nextv priority orient))
          (t
           (setf priority (decf priority))))))

(defmethod prioritize-compression-resistance ((parent-view ns:ns-view) orientation &rest subviews)
  ;; Sets each view's ContentCompressionResistancePriorityForOrientation so that things later in the sub-views
  ;; list are compressed before things earlier in the list. If multiple things should be compressed at the
  ;; same relative priority, enclose them in a list and they will all be assigned the same priority.
  ;; The existing priority of the first view listed is the starting priority.
  (dolist (v subviews)
    ;; make sure all views are in the parent-view
    (when (and (typep v 'ns:ns-view) (not (#/isDescendantOf: v parent-view)))
      (#/addSubview: parent-view v)))
  (do* ((orient (orientation-convert orientation))
        (priority (#/contentCompressionResistancePriorityForOrientation: (if (listp (first subviews)) 
                                                                           (caar subviews)
                                                                           (first subviews))
                                                                         orient)
                  (float (incf priority)))
        (views subviews (rest views))
        (nextv (first views) (first views)))
       ((null nextv) priority)
    (cond ((listp nextv)
           (dolist (v nextv)
             (when (view-p v)
               (#/setContentCompressionResistancePriority:forOrientation: v priority orient))))
          ((view-p nextv)
           (#/setContentCompressionResistancePriority:forOrientation: nextv priority orient))
          (t
           (setf priority (decf priority))))))

(defmethod visualize-constraints ((win ns:ns-window) &optional (constraints nil))
  (#/visualizeConstraints: win (if constraints
                                 (lisp-to-ns-array constraints)
                                 (#/constraints (#/contentView win)))))

(defmethod visualize-constraints ((v ns:ns-view) &optional (constraints nil))
  (let ((win (#/window v)))
    (if (not (eql win (%null-ptr)))
      (#/visualizeConstraints: win (if constraints
                                     (lisp-to-ns-array constraints)
                                     (#/constraints v)))
      (ns-log (format nil "~s is not in a window, cannot show constraints" v)))))

(defmethod log-constraints (&optional (on-off t))
  (setf *log-constraints* on-off))

(defmethod constraints ((win ns:ns-window))
   (ns-to-lisp-list (#/constraints (#/contentView win))))

(defmethod constraints ((v ns:ns-view))
  (ns-to-lisp-list (#/constraints v)))

(defmethod constraints-affecting-layout ((v ns:ns-view) &key (orientation :h))
  (ns-to-lisp-list (#/constraintsAffectingLayoutForOrientation: v (orientation-convert orientation))))

(defmethod analyze-constraints ((v ns:ns-view) &optional (indent 02))
  (let* ((ind-ctrl (format nil "~~~dt" indent))
         (frame-rect (#/frame v))
         (height (ns:ns-rect-height frame-rect))
         (width (ns:ns-rect-width frame-rect))
         (subs (coerce-obj (#/subviews v) 'list)))
    (format t "~%~@?~s Frame: ~s" ind-ctrl v (#/frame v))
    (when (zerop height)
      (let ((constraints (coerce-obj (#/constraintsAffectingLayoutForOrientation: v #$NSLayoutConstraintOrientationVertical)
                                     'list)))
        (format t "~%~@?*** Height = 0 ***" ind-ctrl)
        (if constraints
          (format t 
                  "~%~@?Constraints that might affect this include:~{~%==> ~s~}~%"
                  ind-ctrl
                  constraints)
          (format t "~%~@?There are no constraints affecting this~%" ind-ctrl))))
    (when (zerop width)
      (let ((constraints (coerce-obj (#/constraintsAffectingLayoutForOrientation: v #$NSLayoutConstraintOrientationHorizontal)
                                     'list)))
        (format t "~%~@?*** Width = 0 ***" ind-ctrl)
        (if constraints
          (format t 
                  "~%~@?Constraints that might affect this include:~{~%==> ~s~}~%"
                  ind-ctrl
                  constraints)
          (format t "~%~@?There are no constraints affecting this~%" ind-ctrl))))
    (when (#/hasAmbiguousLayout v)
       (let ((constraints (delete-duplicates 
                           (nconc (coerce-obj (#/constraintsAffectingLayoutForOrientation: v #$NSLayoutConstraintOrientationVertical)
                                              'list)
                                  (coerce-obj (#/constraintsAffectingLayoutForOrientation: v #$NSLayoutConstraintOrientationHorizontal)
                                              'list)))))
        (format t "~%~@?*** Layout frame is ambiguous ***" ind-ctrl)
        (if constraints
          (format t 
                  "~%~@?Constraints that might affect this include:~{~%==> ~s~}~%"
                  ind-ctrl
                  constraints)
          (format t "~%~@?There are no constraints affecting this~%" ind-ctrl))))
    (dolist (sv subs)
      (analyze-constraints sv (+ 2 indent)))
    (values)))

(defmethod analyze-constraints ((win ns:ns-window) &optional indent)
  (declare (ignore indent))
  ;; Look at the views to find those that have 0 width or height frame-rects. Then list the
  ;; constraints that currently affect that layout.
  (on-main-thread
   (format t "~%~%Analyzing Constraints for ~s" win)
   (analyze-constraints (#/contentView win))
   (values)))

(provide :constraint-layout)
