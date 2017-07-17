;; text-views.lisp

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
  (require :nslog-utils)
  (require :lv-classes)
  (require :coerce-obj)
  (require :constraint-layout)
  (require :dev-tools))

(in-package :lv)

#|
(defclass label-view (ns:ns-text-field)
  ()
  (:metaclass ns:+ns-object))
|#

(defmethod initialize-instance :after ((self label-view)
                                       &key
                                       (title "Label")
                                       (background-color nil background-color-provided)
                                       (font  (#/labelFontOfSize: ns:ns-font 12.0d0))
                                       (constrain-size t)
                                       &allow-other-keys)
  (declare (ignore background-color))
  (let* ((text (coerce-obj title 'ns:ns-string))
         (dict (coerce-obj (pairlis (list #$NSFontAttributeName)
                                    (list font))
                           'ns:ns-dictionary))
         (txt-size (#/sizeWithAttributes: text dict))
         (width (ceiling (+ 10.0 (ns:ns-size-width txt-size))))
         (height (ceiling (+ 2.0 (ns:ns-size-height txt-size))))
         (sz  (ns:make-ns-size width height)))
    (#/setStringValue: self text)
    (#/setFont: self font)
    (#/setBezeled: self nil)
    (#/setBordered: self nil)
    (unless background-color-provided
      (#/setDrawsBackground: self nil))
    (#/setSelectable: self nil)
    (#/setFrameSize: self sz)
    (#/setRefusesFirstResponder: self t)
    (when constrain-size
      (constrain-to-natural-size self))))

#|
(defclass labeled-text-field (ns:ns-view)
  ((text-field :accessor text-field))
  (:metaclass ns:+ns-object))
|#

;; Any keywords supplied to the make-instance of a labeled-text-field are
;; also supplied to the make-instance of the enclosed ns-text-field. The
;; presence of the added-class-keywords method tells the init-keys function
;; about those additional keywords.

(defmethod added-class-keywords ((self (eql 'labeled-text-field)))
  (init-keys ns:ns-text-field :return-list t))

(objc:defmethod (#/exposedBindings :id)
                ((self labeled-text-field))
  (#/exposedBindings (text-field self)))

(objc:defmethod (#/bind:toObject:withKeyPath:options: :void)
                ((self labeled-text-field)
                 (binding :id)
                 (observable-controller :id)
                 (keypath :id)
                 (options :id))
  ;; just pass the binding along to the text field
  (when (not (eql (text-field self) (%null-ptr)))
    (#/bind:toObject:withKeyPath:options: (text-field self) binding observable-controller keypath options)))

(defmethod initialize-instance :after ((self labeled-text-field)
                                   &rest key-vals
                                   &key
                                   (title "")
                                   (label-pos :leading)
                                   (width nil)
                                   (resizable nil)
                                   &allow-other-keys)
  (let* ((tf (#/autorelease (apply #'make-instance 'ns:ns-text-field key-vals)))
         (lv (#/autorelease (make-instance 'label-view
                              :title title)))
         (frame-rect (#/frame tf))
         (frame-width (ns:ns-rect-width frame-rect))
         (frame-height (ns:ns-rect-height frame-rect))
         (fr-width (or width (if (zerop frame-width) 60.0 frame-width))))

    (setf (text-field self) tf)

    ;; if user didn't specify a :frame-size argument for the text field, make the width a minimum size
    (#/setFrameSize: tf (ns:make-ns-size (cgfloat fr-width) (cgfloat frame-height)))
    
    ;; To avoid immediately creating mutually unsatisfiable constraints
    (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)
    
    ;; add the two views to the box which will also #/retain them
    (#/addSubview: self lv)
    (#/addSubview: self tf)
    
    ;; constrain the text view size
    (constrain-to-natural-width tf :rel (if resizable :>= :=) :install-view self)
    (constrain-to-natural-height tf :install-view self)

    ;; constrain the width of the box to be at least as wide as the label view
    (constrain (>= (width self) (width lv)))
    
    ;; constrain the views in the box as specified
    (case label-pos
      ((:top :bottom :above :below)
       (order-views :orientation :v
                    :align :center-x
                    :views (if (member label-pos '(:top :above))
                             (list 1 lv 3 tf 1)
                             (list 1 tf 3 lv 1)))
       (constrain (= (center-x lv) (center-x self)))
       (when resizable
         (anchor self tf '(:left :right) :margin 0)))
      (t
       (order-views :orientation :h
                    :views (if (member label-pos '(:left :leading))
                             (list 1 lv 3 tf 1)
                             (list 1 tf 3 lv 1))
                    :align :bottom)
       (anchor self tf '(:top :bottom) :margin 1)))))

#|
;; form-view
;; a box containing right-aligned ns-text-fields of the same width and left-aligned
;; labels for each text field. Size of text fields change as width of the box changes.
(defclass form-view (ns:ns-view)
  ((text-fields :accessor text-fields
                :initarg :text-fields)
   (enabled :accessor enabled
            :foreign-type #>BOOL))
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/exposedBindings :id)
                ((self form-view))
  (coerce-obj (list "enabled") 'ns:ns-array))

(defmethod added-class-keywords ((self (eql 'form-view)))
  (init-keys ns:ns-text-field :return-list t))

(defmethod added-class-key-values ((self (eql (find-class 'form-view))))
  ;; provide keyword documenation for dev-tools functions
  '((:text-fields "Optional list of ns:ns-text-field objects")
    (:text-field-width "Minimum width of any text fields automatically created for the form")
    (:labels "List of Lisp strings")))

(defmethod initialize-instance :after ((self form-view)
                                       &rest key-vals
                                       &key
                                       (enabled t)
                                       (text-fields nil)
                                       (text-field-width 60)
                                       (labels nil))
  (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)
  (let ((label-list nil)
        (txt-list nil)
        (min-txt-width (if text-fields
                         (reduce #'max text-fields :key #'(lambda (tf)
                                                            (ns:ns-size-width (natural-size tf))))
                         text-field-width))
        (label-box nil))
    (flet ((add-subviews (fld)
             (#/addSubview: self fld))
           (constrain-label-to-field (lbl fld)
             (constrain (= (leading fld) (+ 3 (trailing label-box))))
             (constrain (= (center-y lbl) (center-y fld)))
             (constrain (>= (width fld) min-txt-width)))
           (constrain-view-width (v-list)
             (when (second v-list)
               (constrain (= (width (first v-list)) (width (second v-list))))))
           (constrain-label-box (v)
             (constrain (>= (width label-box) (+ (width v) 2)))
             (anchor label-box v (list :left) :margin 0)))
      (do* ((texts text-fields
                   (rest texts))
            (labs labels
                  (rest labs))
            (lab (first labs)
                 (first labs))
            (txt (first texts)
                 (first texts)))
           ((null lab) t)
        (push (make-instance 'label-view
                :title lab)
              label-list)
        (push (or txt
                  (#/autorelease (apply #'make-instance 'ns:ns-text-field key-vals)))
              txt-list))
      (setf label-list (nreverse label-list))
      (setf label-box (make-instance 'ns:ns-view
                        :subviews label-list))
      (align-views :views label-list
                   :align :left)
      (mapc #'constrain-label-box label-list)
      (setf txt-list (nreverse txt-list))
      (setf (text-fields self) txt-list)
      ;; Next line must be first thing because it adds in subviews
      (mapc #'add-subviews txt-list)
      (#/addSubview: self label-box)
      (mapl #'constrain-view-width txt-list)
      (mapc #'constrain-label-to-field label-list txt-list)
      (constrain (= (height label-box) (height self)))
      (constrain (= (width label-box) (natural-view-width label-box)))
      (constrain (>= (width self) (+ (width label-box) (+ min-txt-width 3))))
      (anchor self label-box (list :left :top) :margin 1)
      (anchor self (first txt-list) (list :top :right) :margin 1)
      (order-views :views (append (list 1) txt-list (list 1))
                   :align :trailing
                   :orientation :v)
      (align-views :views label-list :align :leading)
      (constrain (>= (top (first txt-list)) (+ 1 (top self))))
      (constrain (>= (trailing self) (+ 1 (trailing (first (last txt-list))))))
      (dolist (tv txt-list)
        (bind tv "enabled" self "enabled"))))
  (if enabled
    (setf (enabled self) #$YES)
    (setf (enabled self) #$NO))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rotated-text-view methods

#|
(defclass rotated-text-view (ns:ns-view)
  ((rtv-str :accessor rtv-str
            :initarg :string)
   (rtv-attributes :accessor rtv-attributes
                   :initarg :attributes)
   (rtv-v-just :accessor rtv-v-just
               :initarg :v-just) ;; one of :top :center :bottom
   (rtv-h-just :accessor rtv-h-just
               :initarg :h-just) ;; one of :left :center :right
   (rtv-v-margin :accessor rtv-v-margin
                 :initarg :v-margin)
   (rtv-h-margin :accessor rtv-h-margin
                 :initarg :h-margin)
   (rtv-angle :accessor rtv-angle
              :initform 0)
   (rtv-text-width :accessor rtv-text-width
                   :initform 0.0)
   (rtv-text-height :accessor rtv-text-height
                    :initform 0.0)
   (rtv-min-width :accessor rtv-min-width
                  :initform 0.0)
   (rtv-min-height :accessor rtv-min-height
                   :initform 0.0)
   (rtv-text-storage :accessor rtv-text-storage)
   (rtv-layout-manager :accessor rtv-layout-manager)
   (rtv-text-container :accessor rtv-text-container)
   (rtv-glyph-range :accessor rtv-glyph-range))
  (:default-initargs
    :string "Test String"
    :attributes nil
    :v-just :top
    :h-just :left
    :v-margin 5.0
    :h-margin 5.0)
  (:metaclass ns:+ns-object))
|#

(defconstant *2pi* (* 2.0d0 pi))
(defconstant *pi-over-2* (/ pi 2.0d0))
(defconstant *3pi-over-2* (* (/ 3.0d0 2.0d0) pi))

(defun rotate-quadrant (r)
  ;; r must be a rotation measured in radians betwee 0 and 2* pi
  (floor r (/ pi 2.0d0)))

(defun normalize-rotation (deg)
  ;; deg given is a number of degrees (positive or negative)
  ;; result is a number of radians between 0 and 2 * pi
  (/ (* (mod deg 360) pi) 180))

(defmethod added-class-keywords ((self (eql 'rotated-text-view)))
  (list :direction :h-just :v-just :h-margin :v-margin :attributes :string))

(defmethod added-class-key-values ((self (eql (find-class 'rotated-text-view))))
  ;; provide keyword documenation for dev-tools functions
  '((:direction ":up :down :right or number of degrees rotated from horizontal (positive = counter-clockwise, negative = clockwise)")
    (:h-just "One of :left :center :right")
    (:v-just "One of :top :center :bottom")
    (:h-margin "The number of pixels to be left blank at the left and right ends of the text")
    (:v-margin "The number of pixels to be left blank at the top and bottom of the text")
    (:attributes "an NSDictionary (or Lisp assoc list) of attribute name and value pairs (see attributed-strings.lisp)") 
    (:string "Instance of: lisp string, attributed-string, ns-string, ns-attributed-string, ns-mutable-attributed-string")))

(defmethod initialize-instance :after ((self rotated-text-view)
                                       &key
                                       (direction :right)
                                       &allow-other-keys)
  (with-slots (rtv-str rtv-angle rtv-attributes rtv-text-storage rtv-text-container rtv-layout-manager rtv-glyph-range
               rtv-rotated-text-width rtv-rotated-text-height
               rtv-min-width rtv-min-height rtv-v-margin rtv-h-margin rtv-text-width rtv-text-height) self
    (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)

    ;; Set the angle (in radians) at which text flows. As per Apple's convention, all angles are relative to
    ;; a horizontal line, positive is counter-clockwise, negative is clockwise.
    (setf rtv-angle (case direction
                      (:right 0.0d0)
                      (:up *pi-over-2*)
                      (:down *3pi-over-2*)
                      (t (if (numberp direction)
                             (normalize-rotation direction)
                             (progn
                               (ns-log-format "Illegal :direction argument ~s, using :right" direction)
                               0.0d0)))))
    
    ;; Set up the text-container layout-manager and text-storage objects
    (let ((att-dict (typecase rtv-attributes
                      (ns:ns-dictionary rtv-attributes)
                      (cons (coerce-obj rtv-attributes ns:ns-dictionary))
                      (t nil))))
      (setf rtv-text-storage (if att-dict
                                 (make-instance 'ns:ns-text-storage
                                   :with-string (coerce-obj rtv-str 'ns:ns-string)
                                   :attributes att-dict)
                                 (make-instance 'ns:ns-text-storage
                                   :with-attributed-string (coerce-obj rtv-str 'ns:ns-attributed-string))))
      (setf rtv-text-container (#/autorelease (make-instance ns:ns-text-container)))
      (setf rtv-layout-manager (#/autorelease (make-instance ns:ns-layout-manager)))
      (#/addTextContainer: rtv-layout-manager rtv-text-container)
      (#/addLayoutManager: rtv-text-storage rtv-layout-manager)
      (#/setUsesScreenFonts: rtv-layout-manager #$NO)
      (setf rtv-glyph-range (#/glyphRangeForTextContainer: rtv-layout-manager rtv-text-container)))

    ;; Set the minimum size of a rectangle that will enclose the text rectangle when displayed at the designated
    ;; angle. For text angles outside of the first quadrant, we find a corresponding angle inside the first
    ;; quadrant that will have the same size bounding rectangle and use that to compute the rectangle.
    (let* ((base-size (#/size (rtv-text-storage self)))
           (base-width (ceiling (+ 10.0d0 (ns:ns-size-width base-size))))
           (base-height (ceiling (+ 12.0d0 (ns:ns-size-height base-size))))
           (cis-angle (cis (cond ((<= rtv-angle *pi-over-2* )
                                  rtv-angle)
                                 ((<= rtv-angle pi)
                                  (- pi rtv-angle))
                                 ((<= rtv-angle *3pi-over-2*)
                                  (- rtv-angle pi))
                                 ((<= rtv-angle *2pi*)
                                  (- *2pi* rtv-angle)))))
           (sin-angle (imagpart cis-angle))
           (cos-angle (realpart cis-angle)))
      (setf rtv-rotated-text-width (ceiling (+ (* sin-angle base-height)
                                               (* cos-angle base-width))))
      (setf rtv-rotated-text-height (ceiling (+ (* cos-angle base-height)
                                                (* sin-angle base-width))))
      (setf rtv-text-width base-width)
      (setf rtv-text-height base-height)
      (setf rtv-min-width (ceiling (+ rtv-rotated-text-width (* 2.0 rtv-h-margin))))
      (setf rtv-min-height (ceiling (+ rtv-rotated-text-height (* 2.0 rtv-v-margin)))))))

(objc:defmethod (#/dealloc :void)
                ((self rotated-text-view))
  (#/release (rtv-text-storage self))
  (call-next-method)
  (objc:remove-lisp-slots self))


#|
;; A test method for a rotated-text-view

(defmethod test-shifts ((self rotated-text-view))
  (with-slots (rtv-text-storage rtv-layout-manager rtv-glyph-range
               rtv-text-height rtv-text-width
               rtv-v-just rtv-h-just rtv-v-margin rtv-h-margin) self
    (dotimes (i 16)
      (let* ((rtv-angle (* i (/ pi 8.0d0)))
             (quad (rotate-quadrant rtv-angle))
             ;; after we rotate the text around the origin, we need to shift it as much as
             ;; necessary to bring it back into view at the right place, depending on margins
             ;; and user-specified justification.
             (x-shift (gui::cgfloat (- 0
                                       (case quad
                                         (0 (* (sin rtv-angle) rtv-text-height))
                                         (1 (+ (* (sin (- rtv-angle *pi-over-2*)) rtv-text-width)
                                               (* (cos (- rtv-angle *pi-over-2*)) rtv-text-height)))
                                         (2 (* (cos (- rtv-angle pi)) rtv-text-width))
                                         (3 0.0d0)))))
             (y-shift (gui::cgfloat (- 0
                                       (case quad
                                       (0 0.0d0)
                                       (1 (* (sin (- rtv-angle *pi-over-2*)) rtv-text-height))
                                       (2 (+ (* (cos (- *3pi-over-2* rtv-angle)) rtv-text-width)
                                             (* (sin (- *3pi-over-2* rtv-angle)) rtv-text-height)))
                                       (3 (* (sin (- *2pi* rtv-angle)) rtv-text-width)))))))
        (format t "~%At angle: ~s  X-shift = ~s  Y-shift = ~s" rtv-angle x-shift y-shift)))))
|#

(objc:defmethod (#/drawRect: :void)
                ((self rotated-text-view) (rect #>NSRect))
  (with-slots (rtv-angle rtv-text-storage rtv-layout-manager rtv-glyph-range
               rtv-text-height rtv-text-width rtv-rotated-text-width rtv-rotated-text-height
               rtv-v-just rtv-h-just rtv-v-margin rtv-h-margin) self
    (let* ((context (#/currentContext ns:ns-graphics-context))
           (transform (#/transform ns:ns-affine-transform))
           (rw (ns:ns-rect-width rect))
           (rh (ns:ns-rect-height rect))
           (quad (rotate-quadrant rtv-angle))
           ;; after we rotate the text around the origin, we need to shift it as much as
           ;; necessary to bring it back into view at the right place, depending on margins
           ;; and user-specified justification.
           (x-shift (gui::cgfloat (+ (case rtv-h-just
                                       (:left rtv-h-margin)
                                       (:center (/ (- rw rtv-rotated-text-width) 2.0d0))
                                       (:right (- rw rtv-rotated-text-width rtv-h-margin)))
                                     (case quad
                                       (0 (* (sin rtv-angle) rtv-text-height))
                                       (1 (+ (* (sin (- rtv-angle *pi-over-2*)) rtv-text-width)
                                             (* (cos (- rtv-angle *pi-over-2*)) rtv-text-height)))
                                       (2 (* (cos (- rtv-angle pi)) rtv-text-width))
                                       (3 0.0d0)))))
           (y-shift (gui::cgfloat (+ (case rtv-v-just
                                       (:bottom rtv-v-margin)
                                       (:center (/ (- rh rtv-rotated-text-height) 2.0d0))
                                       (:top (- rh rtv-rotated-text-height rtv-v-margin)))
                                     (case quad
                                       (0 0.0d0)
                                       (1 (* (sin (- rtv-angle *pi-over-2*)) rtv-text-height))
                                       (2 (+ (* (cos (- *3pi-over-2* rtv-angle)) rtv-text-width)
                                             (* (sin (- *3pi-over-2* rtv-angle)) rtv-text-height)))
                                       (3 (* (sin (- *2pi* rtv-angle)) rtv-text-width)))))))
      ;; set things up to rotate the text around the specified origin point
      #|(#/translateXBy:yBy: transform
                           (ns:ns-point-x origin-pt)
                           (ns:ns-point-y origin-pt))|#
      (#/translateXBy:yBy: transform x-shift y-shift)
      (#/rotateByRadians: transform (gui::cgfloat rtv-angle))
      (#/saveGraphicsState context)
      (#/concat transform)
      ;;(#/lockFocus self)
      (#/drawGlyphsForGlyphRange:atPoint: rtv-layout-manager rtv-glyph-range (coerce-obj (list 0.0 0.0) 'ns:ns-point))
      ;;(#/unlockFocus self)
      (#/restoreGraphicsState context))))

(objc:defmethod (#/intrinsicContentSize ns:ns-size)
                ((self rotated-text-view))
  (ns:make-ns-size (rtv-min-width self) (rtv-min-height self)))


;; test method for rotated-view objects
#|
(defun show-rot (att-str &key (rot :up) (h-just :center) (v-just :center))
  (declare (special *rwin* *rwc* *rlabel*))
  ;; display any string, including attributed strings (see attributed-strings.lisp) in new window
  (on-main-thread
   (when (and (boundp '*rwin*) (ccl::objc-instance-p *rwin*) (boundp '*rwc*) (ccl::objc-instance-p *rwc*))
     (#/close *rwc*)
     (#/release *rlabel*)
     (#/release *rwin*)
     (#/release *rwc*))
   (setf *rlabel* (make-instance 'rotated-text-view
                    :direction rot
                    :string att-str
                    :h-just h-just
                    :v-just v-just))
   (setf *rwin* (make-instance 'ns:ns-window
                 :title "Rotated String Test"
                 :content-subviews (list *rlabel*)))
   (let ((cv (#/superview *rlabel*)))
     (constrain-to-natural-size *rlabel* :rel :>=)
     (constrain-size-relative-to cv *rlabel* :rel :>=)
     (anchor cv *rlabel* '(:top :bottom :left :right) :margin 0))
   (setf *rwc* (make-instance 'ns:ns-window-controller
                :window *rwin*))
   (#/showWindow: *rwc* (%null-ptr))))
|#

(provide :text-views)