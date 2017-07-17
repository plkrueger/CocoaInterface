;; squiggle-doc.lisp

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

;; This is a fairly direct translation to Lisp from Apple's example code: 
;;     http://developer.apple.com/mac/library/samplecode/Squiggles/Introduction/Intro.html

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-document)
  (require :squiggle-classes)
  (require :squiggle)
  (require :squigg-view)
  (require :constraint-layout)
  (require :lisp-app-delegate)
  (require :text-views)
  (require :notification))

(in-package :sq)

#|
(defclass squiggle-doc (lisp-document)
  ((squiggle-list :accessor squiggle-list
                  :initform nil)
   (rotations :accessor rotations
              :initform 1
              :kvo "rotations"
              :undo "set rotations")
   (squiggle-view :accessor squiggle-view
                  :initform nil)
   (squiggle-win :accessor squiggle-win
                 :initform nil)
   (back-color-well :accessor back-color-well
                    :initform nil)
   (back-color :accessor back-color
               :initform (#/blueColor ns:ns-color)
               :kvo "backColor"
               :undo "set background color")
   (rotation-slider :accessor rotation-slider
                    :initform nil)
   (window-frame :accessor window-frame
                 :initform (list 200 200 400 400))
   (notif-handler :accessor notif-handler
                  :initform nil))
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/dealloc :void)
                ((self squiggle-doc))
  (when (notif-handler self)
    (#/release (notif-handler self))
    (setf (notif-handler self) nil))
  (call-next-method)
  (objc:remove-lisp-slots self))

(defmethod archive-slots ((obj squiggle-doc))
  (declare (ignore obj))
  (list 'squiggle-list 'rotations 'back-color 'window-frame))

(defmethod remove-squiggle ((self squiggle-doc) (sq squiggle))
  (set-undo self
            #'(lambda ()
                (add-squiggle self sq))
            "remove squiggle")
  (setf (squiggle-list self) (delete sq (squiggle-list self)))
  (#/setNeedsDisplay: (squiggle-view self) #$YES))

(defmethod add-squiggle ((self squiggle-doc) (sq squiggle))
  (set-undo self
            #'(lambda ()
                (remove-squiggle self sq))
            "add squiggle")
  (setf (squiggle-list self) (cons sq (squiggle-list self)))
  (#/setNeedsDisplay: (squiggle-view self) #$YES))

(defmethod start-new-squiggle ((self squiggle-doc) point)
  ;; this will be invoked on mouse-down to start a new squiggle.
  (let ((sq (make-instance 'squiggle)))
    (add-point sq point)
    (add-squiggle self sq)))

(defmethod continue-squiggle ((self squiggle-doc) point)
  ;; when the mouse is dragged, continue by adding a point and 
  ;; invalidate the view so it redraws
  (add-point (first (squiggle-list self)) point)
  (#/setNeedsDisplay: (squiggle-view self) #$YES))

;; Methods to manage windows

(defnotification ((self squiggle-doc) (slot rotations) new-val)
  (declare (ignore new-val))
  (when (and (slot-boundp self 'squiggle-view)
             (squiggle-view self))
    (#/setNeedsDisplay: (squiggle-view self) #$YES)))

(defnotification ((self squiggle-doc) (slot back-color) new-val)
  (declare (ignore new-val))
  (when (and (slot-boundp self 'squiggle-view)
             (squiggle-view self))
    (#/setNeedsDisplay: (squiggle-view self) #$YES)))

(defmethod frame-changed ((self squiggle-doc) notif-name win notif-info)
  (declare (ignore notif-name notif-info))
  ;; called when the window size is changed
  (let* ((frame (coerce-obj (window-frame self) 'ns:ns-rect))
         (old-size (ns:make-ns-size (ns:ns-rect-width frame) (ns:ns-rect-height frame)))
         (new-frame (#/frame win))
         (new-size (ns:make-ns-size (ns:ns-rect-width new-frame) (ns:ns-rect-height new-frame))))
    (unless (equal-size-p old-size new-size)
      (set-undo self #'(lambda ()
                         (setf (window-frame-size win) old-size)
                         (setf (window-frame self) frame))
                "window size change"))
    (setf (window-frame self) new-frame)))
  
;; Support printing

(defmethod print-lines ((self squiggle-doc) lines-per-page)
  (declare (ignore lines-per-page))
  nil)

(defmethod print-graphic ((self squiggle-doc) pr-view rect)
  (draw-in-view-rect pr-view 
                     self 
                     (#/bounds (squiggle-view self))
                     rect))

;; Specify the method to be used to build a window

(defmethod make-squiggle-win ((wc lisp-window-controller))
  (let* ((doc (#/document wc))
         (squig-view (make-instance 'squigg-view
                       :document doc))
         (slider (make-instance 'ns:ns-slider
                   :continuous t
                   :max-value 100.0
                   :min-value 1.0))
         (back-label (make-instance 'label-view
                       :title "Background Color"))
         (color-well (make-instance 'ns:ns-color-well
                       :bordered t
                       :frame-size (list 40 20)
                       :color :blue))
         (win (make-instance 'ns:ns-window
                :resizable t
                :frame (window-frame doc)
                :content-subviews (list squig-view slider back-label color-well)))
         (cv (#/contentView win)))

    (setf (squiggle-view doc) squig-view)
    (setf (squiggle-win doc) win)
    (setf (rotation-slider doc) slider)
    (setf (back-color-well doc) color-well)

    (bind slider "value" doc "rotations")
    (bind color-well "value" doc "backColor")

    ;; Add view size constraints
    (constrain-to-natural-size color-well)
    (constrain (= (height slider) (height color-well)))
    (constrain (>= (width slider) (height slider))) ;; forces slider to be horizontal

    ;; Add view position constraints
    (anchor cv squig-view (list :top :left :right))
    (anchor cv slider (list :left))
    (anchor cv color-well (list :right :bottom))
    (constrain (= (top slider) (+ 6 (bottom squig-view))))
    (order-views :views (list :border slider 12 back-label 2 color-well :border)
                 :orientation :h
                 :align :center-y)

    ;; Set up things so that the squiggle doc gets notified when the window's frame changes size so
    ;; that it can save that value off and restore it when a squiggle-doc is reopened.
    (setf (notif-handler doc) (make-instance 'notification-handler
                                :from win
                                :notifications (list #$NSWindowDidResizeNotification
                                                     #$NSWindowDidMoveNotification)
                                :target doc
                                :func #'frame-changed))
    
    (values win (list squig-view slider back-label color-well win))))

(defun init-squiggle-app (app)
  ;; The function that gets called to initialize the application
  ;; It creates and installs a main menu.
  (let ((*app-name-for-menus* "Squiggle"))
    (#/setMainMenu: app (standard-main-menu))
    (set-windows-menu))
  nil)

(defmethod window-build-funcs ((self squiggle-doc))
  (list #'make-squiggle-win))

(provide :squiggle-doc)
