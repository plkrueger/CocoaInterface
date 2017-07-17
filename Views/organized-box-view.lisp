;; organized-box-view.lisp


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

#|
This class implements a view that is intended to be used either as a stand-alone view or as a content
view for an ns:ns-box or perhaps for a window. It maintains a collection of other views that are ordered
in either the horizontal or vertical direction and aligned in some manner. The box itself is constrained so that
its size is always big enough to encompass all the views within it.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :constraint-layout)
  (require :coerce-obj))

(in-package :lv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; organized-box-view

(defclass organized-box-view (ns:ns-view)
  ((orientation :accessor orientation
               :initarg :orientation)
   (alignment :accessor alignment)
   (h-margin :accessor h-margin
             :initform 0.0d0)
   (v-margin :accessor v-margin
             :initform 0.0d0)
   (spacing :accessor spacing
            :initarg :spacing)
   (last-frame :accessor last-frame
               :initform (ns:make-ns-rect 0 0 0 0))
   (cached-constraints :accessor cached-constraints
                       :initform nil)
   (views :accessor views
          :initarg :views))
  (:default-initargs
    :orientation :h
    :spacing *default-space*
    :views nil)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self organized-box-view)
                                       &key
                                       (views nil)
                                       (align nil)
                                       (margin nil) ;; ns:ns-size or list of the form (h-margin v-margin)
                                       &allow-other-keys)
  (setf (alignment self) (or align (if (eq (orientation self) :h) :center-y :center-x)))
  (dolist (v views)
    ;; constrain the new subviews to a consistent size.
    ;; The method below will also make it a subview of the box
    (when (view-p v)
      (#/addSubview: self v)
      (constrain-to-natural-size v)))
  (unless (or (eq (orientation self) :h) (eq (orientation self) :v))
    (ns-log-format "Orientation ~s must be :h or :v for ~s"
                   (orientation self)
                   self)
    (setf (orientation self) :h))
  (when margin
    (let ((m-sz (coerce-obj margin 'ns:ns-size)))
      (setf (h-margin self) (ns:ns-size-width m-sz))
      (setf (v-margin self) (ns:ns-size-height m-sz))))

  ;; Turn off auto constraint for this view so that we won't get an immediate conflict
  ;; of constraints when we start adding our own
  (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)

  ;; Make the box reluctant to expand
  ;; This needs to be done here rather than in the updateConstraints method because doing it
  ;; there will cause needsUpdateConstraints to become true, which you cannot do within
  ;; an updateConstraints method (causes a runtime exception)
  (setf (expansion-resistance-priority self :h) :high)
  (setf (expansion-resistance-priority self :v) :high)

  ;; Set things up to be notified when the frame size changes so we can 
  ;; void the constraints in the frame and specify that they be recomputed.
  (#/addObserver:selector:name:object: 
   (#/defaultCenter ns:ns-notification-center)
   self
   (ccl::@selector "frameChangedNotification:")
   #$NSViewFrameDidChangeNotification
   self)
  (#/setPostsFrameChangedNotifications: self t))

(objc:defmethod (#/frameChangedNotification: :void) 
                ((self organized-box-view) notification)
  (declare (ignore notification))
  
  (with-slots (cached-constraints last-frame) self
    ;; Check to see if the view size changed
    (unless (equal-size-p (#/frame self) last-frame)
      (setf last-frame  (#/frame self))
      ;; Remove all existing constraints pertaining to layout and size.
      (when cached-constraints
        (let ((count (#/count (#/constraints self))))
          (when *log-constraints*
            (ns-log-format "Removing ~s constraints from ~s" (list-length cached-constraints) self))
          (#/removeConstraints: self (coerce-obj (delete-if-not #'(lambda (c)
                                                                    (or (typep c 'ns:ns-layout-constraint)
                                                                        (progn 
                                                                          (ns-log-format "~s not a constraint" c)
                                                                          nil)))
                                                                cached-constraints)
                                                 'ns:ns-array))
          (when *log-constraints*
            (ns-log-format "~s Constraints removed" (- count  (#/count (#/constraints self)))))
          (setf cached-constraints nil)))
        (#/setNeedsUpdateConstraints: self t))))

(objc:defmethod (#/updateConstraints :void) 
                ((self organized-box-view))
  ;; set all the constraints for subviews
  (call-next-method)
  (when *log-constraints*
      (ns-log "Starting organized-box-view updateConstraints"))
  (with-slots (orientation alignment spacing h-margin v-margin cached-constraints last-frame views) self
    (unless cached-constraints
      ;; if layout has already been done and not invalidated by a frame change
      ;; just exit and leave well enough alone

      (dolist (v views)
        (when (view-p v)
          (ecase orientation
            (:h 
             ;; add vertical constraints for the horizonatally aligned objects
             (push (constrain (>= (top v) (+ (top self) v-margin)) :install-view self) cached-constraints)
             (push (constrain (>= (bottom self) (+ (bottom v) v-margin)) :install-view self) cached-constraints)
             (push (constrain (>= (height self) (+ (height v) (* 2 v-margin))) :install-view self) cached-constraints))
            (:v
             ;; add horizontal constraints for the vertically aligned objects
             (push (constrain (>= (leading v) (+ (leading self) h-margin)) :install-view self) cached-constraints)
             (push (constrain (>= (trailing self) (+ (trailing v) h-margin)) :install-view self) cached-constraints)
             (push (constrain (>= (width self) (+ (width v) (* 2 h-margin))) :install-view self) cached-constraints)))))
        
      ;; order and align the subviews within the organized-box-view
      (when views
        (nconc cached-constraints
               (order-views :orientation orientation
                            :views (append (list (if (eq orientation :h) h-margin v-margin))
                                           views
                                           (list (if (eq orientation :h) h-margin v-margin)))
                            :install-view self
                            :align alignment
                            :spacing spacing)))

      ;; Most things put in these boxes (e.g. buttons, text boxes) will have a fixed height
      ;; and variable width. So if the objects are horizontally oriented, constrain the vertical size.
      (when (eq orientation :h)
        (let* ((min-size (natural-size self))
               (height (ns:ns-size-height min-size)))
          (nconc cached-constraints
                 (constrain-size self :height height))))

      (when *log-constraints*
        (ns-log-format "Resuming organized-box-view updateConstraints"))
      
      (when *log-constraints*
        (ns-log-format "Ending organized-box-view updateConstraints"))

      ;; set the frame that corresponds to the constraints we just added
      (setf last-frame  (#/frame self)))))
  
(defmethod add-box-subviews ((self organized-box-view) views)
  ;; views can be either real views or numbers that define inter-view spacing.
  ;; The views are added to the end of the existing set of views.
  ;; If you use #/addSubview: to add a subview directly to an organized-box-view
  ;; it will NOT be constrained along with other views.
  ;; Note that we must add the sizing constraints to the organized-box-view
  ;; rather than to the individual views themselves (which is the default) so
  ;; that the computation of its natural size works correctly.
  (setf (views self) (nconc (views self) views))
  (dolist (v views)
    (when (view-p v)
      (#/addSubview: self v)
      (constrain-to-natural-size v :install-view self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resizable-box-view
;;
;; This view will layout a set of objects as an array. The dimensions of the
;; array are determined by the frame size of the resizable-box-view. It is up
;; to the user to assure that this size is sufficient to display all the objects.
;; The resizable-box-view will determine the optimal number of rows and columns
;; and the inter-column / inter-row spacing necessary to best fit all the
;; objects. You may constrain the size of the objects separately if desired, but
;; do not place any constraints on their position. If the size of the 
;; resizable-box-view is insufficient to display all the views, then some views
;; will be hidden. If this is anticipated as a possible and permissible event,
;; then this view should be placed within a scroll-view. The resizable-box-view
;; will attempt to match the target-hv-ratio as closely as possible, but will
;; deviate as much as necessary to fit into the frame provided. If objects are
;; not all of the same size, each cell of the array will be of sufficient size
;; to hold the largest object.
;;
;; Parameters: min-h-spacing, min-v-spacing, views, target-hv-ratio, margin

(defclass resizable-box-view (ns:ns-view)
  ((min-h-spacing :accessor min-h-spacing
                  :initarg :min-h-spacing)
   (min-v-spacing :accessor min-v-spacing
                  :initarg :min-v-spacing)
   (target-hv-ratio :accessor target-hv-ratio
                    :initarg :target-hv-ratio)
   (margin :accessor margin
           :initarg :margin)
   (cached-constraints :accessor cached-constraints
                       :initform nil)
   (last-frame :accessor last-frame
               :initform (ns:make-ns-rect 0 0 0 0))
   (views :accessor views
          :initarg :views)
   (max-view-h :accessor max-view-h
               :initform 0)
   (max-view-v :accessor max-view-v
               :initform 0)
   (hv-ratios :accessor hv-ratios
              :initform nil)
   (sized-hv-ratios :accessor sized-hv-ratios
                    :initform nil))
  (:default-initargs
    :views nil
    :min-h-spacing 2
    :min-v-spacing 2
    :target-hv-ratio nil
    :margin 0)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self resizable-box-view)
                                       &key
                                       &allow-other-keys)
  (with-slots (hv-ratios views) self
    (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)
    (dolist (v views)
      (#/setTranslatesAutoresizingMaskIntoConstraints: v #$NO)
      (#/addSubview: self v))

    (set-possible-hv-ratios self)

    ;; Set things up to be notified when the frame size changes so we can 
    ;; void the constraints in the frame and specify that they be recomputed.
     (#/addObserver:selector:name:object: 
        (#/defaultCenter ns:ns-notification-center)
        self
        (ccl::@selector "frameChangedNotification:")
        #$NSViewFrameDidChangeNotification
        self)
    (#/setPostsFrameChangedNotifications: self t)))

(objc:defmethod (#/frameChangedNotification: :void) 
                ((self resizable-box-view) notification)
  (declare (ignore notification))
  (on-main-thread 
   (with-slots (cached-constraints last-frame) self
     ;; Check to see if the view size changed
     (unless (equal-size-p (#/frame self) last-frame)
       (setf last-frame  (#/frame self))
       ;; Remove all existing constraints pertaining to layout and size.
       (when cached-constraints
         (let ((count (#/count (#/constraints self))))
           (when *log-constraints*
             (ns-log-format "Removing ~s constraints from ~s" (list-length cached-constraints) self))
           (#/removeConstraints: self (coerce-obj cached-constraints 'ns:ns-array))
           (when *log-constraints*
             (ns-log-format "~s Constraints removed" (- count  (#/count (#/constraints self)))))
           (setf cached-constraints nil)))
       (#/setNeedsUpdateConstraints: self t)))))

(objc:defmethod (#/updateConstraints :void) 
                ((self resizable-box-view))
  ;; set all the constraints for subviews
  (call-next-method)
  (when *log-constraints*
      (ns-log (format nil "Starting resizable-box-view updateConstraints")))
  (with-slots (cached-constraints hv-ratios max-view-h max-view-v margin 
               min-h-spacing min-v-spacing sized-hv-ratios 
               target-hv-ratio views) self

    (unless (or cached-constraints (null views))
      ;; if layout has already been done and not invalidated by a frame change
      ;; just exit and leave well enough alone
      
      ;; find the current max-h and max-v for all views
      (setf max-view-h 0)
      (setf max-view-v 0)
      (let (sz
            sz-wid
            sz-ht)
        (dolist (v views)
          (setf sz (natural-size v))
          (setf sz-wid (ns:ns-size-width sz))
          (setf sz-ht (ns:ns-size-height sz))
          (when (> sz-wid max-view-h)
            (setf max-view-h sz-wid))
          (when (> sz-ht max-view-v)
            (setf max-view-v sz-ht))))
      
      ;; compute the sized-hv-ratios with the current object heights
      (setf sized-hv-ratios
            (mapcar #'(lambda (hv-rat)
                        (let ((num-cols (car hv-rat))
                              (num-rows (cdr hv-rat))
                              (margin-space (* 2 margin)))
                          (cons (+ (* max-view-h num-cols)
                                   (* min-h-spacing (1- num-cols))
                                   margin-space)
                                (+ (* max-view-v num-rows)
                                   (* min-v-spacing (1- num-rows))
                                   margin-space))))
                    hv-ratios))
      
      ;; This complicated little bit of code implements the following decision for what
      ;; row-column layout to use.
      ;; Set the eligible ratios range to whichever of them fit in the current frame.
      ;; If none of them fit in the current frame, make them all eligible.
      ;; If a target ratio exists, pick the eligible ratio that best matches that target
      ;; Otherwise pick the middle ratio of all those eligible.
      ;; Use the corresponding entry in hv-ratios to specify the horizontal and vertical
      ;; dimensions of array and then use constrain-to-array to create needed constraints.
      (when views
        (let* ((frame-rect (#/frame self))
               (sz-wid (ns:ns-rect-width frame-rect))
               (sz-ht (ns:ns-rect-height frame-rect))
               (frame-ratio (unless (zerop sz-ht) (/ sz-wid sz-ht)))
               (start-pos (or (position-if #'(lambda (hv-rat)
                                               (and (<= (car hv-rat) sz-wid)
                                                    (<= (cdr hv-rat) sz-ht)))
                                           sized-hv-ratios)
                              0))
               (end-pos (or (position-if #'(lambda (hv-rat)
                                             (and (<= (car hv-rat) sz-wid)
                                                  (<= (cdr hv-rat) sz-ht)))
                                         sized-hv-ratios
                                         :from-end t)
                            (1- (list-length sized-hv-ratios))))
               (middle-pos (ceiling (+ start-pos end-pos) 2))
               (middle-rat (nth middle-pos sized-hv-ratios))
               (middle-ratio (/ (car middle-rat) (cdr middle-rat)))
               (target-ratio (or target-hv-ratio frame-ratio middle-ratio))
               (best-for-target-pos (do* ((i start-pos
                                             (1+ i))
                                          (hv-rats (nthcdr i sized-hv-ratios)
                                                   (rest hv-rats))
                                          (hv-rat (first hv-rats)
                                                  (first hv-rats))
                                          (this-diff (abs (- (/ (car hv-rat) (cdr hv-rat)) target-ratio))
                                                     (abs (- (/ (car hv-rat) (cdr hv-rat)) target-ratio)))
                                          (diff this-diff)
                                          (best i
                                                (if (< this-diff diff)
                                                  (progn
                                                    (setf diff this-diff)
                                                    i)
                                                  best)))
                                         ((>= i end-pos) best)))
               (layout-rat (nth best-for-target-pos hv-ratios))
               (num-columns (car layout-rat))
               (num-rows (cdr layout-rat))
               (h-space (if (and sz-wid (> num-columns 1))
                          (/ (- sz-wid  2 (* 2 margin) (* num-columns max-view-h)) (1- num-columns))
                          min-h-spacing))
               (v-space (if (and sz-ht (> num-rows 1))
                          (/ (- sz-ht  2 (* 2 margin) (* num-rows max-view-v)) (1- num-rows))
                          min-v-spacing)))
          ;; Anchor the first view to the top-left of the resizable-box-view with 
          ;; appropriate margins.
          (setf cached-constraints (anchor self (first views) '(:top :leading) :margin margin))
          
          ;; Order and align objects into an array
          (setf cached-constraints 
                  (nconc cached-constraints
                         (constrain-to-array num-columns
                                             :h-space h-space
                                             :v-space v-space
                                             :views views))))
        (#/setNeedsDisplay: self t))
      
      ;; just in case any of the constrain commands failed and returned nil
      (setf cached-constraints (delete-if #'null cached-constraints)))
    
    (when *log-constraints*
      (ns-log (format nil "Ending invisible-box updateConstraints")))))

(defmethod set-possible-hv-ratios ((self resizable-box-view))
  ;; compute possible hv-ratios
  (with-slots (views hv-ratios) self
    (setf hv-ratios nil)
    (do* ((num-views (list-length views))
          (rows 1 (1+ rows))
          (last-cols 0 cols)
          (cols (ceiling num-views rows)
                (ceiling num-views rows)))
         ((> rows num-views))
      (unless (eql cols last-cols) ;; always use fewest number of rows for any number of columns
        (push (cons cols rows) hv-ratios)))))

(defmethod add-box-subviews ((self resizable-box-view) views)
  ;; The views are added to the end of the existing set of views.
  ;; If you use #/addSubview: to add a subview directly to an organized-box-view
  ;; it will NOT be constrained along with other views.
  (setf (views self) (nconc (views self) views))
  (dolist (v views)
    (when (view-p v)
      (#/setTranslatesAutoresizingMaskIntoConstraints: v #$NO)
      (#/addSubview: self v)))
  (set-possible-hv-ratios self))

(provide :organized-box-view)
