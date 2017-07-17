;;; speech-controller.lisp

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
  (require :demo-packages)
  (require :coerce-obj)
  (require :window-utils)
  (require :constraint-layout)
  (require :button)
  (require :radio-button)
  (require :scroll-view)
  (require :window-controller)
  (require :text-views))

(in-package :spc)

(defclass speech-data ()
  ((speech-text :accessor speech-text)
   (speech-synth :accessor speech-synth 
                 :initform (make-instance 'ns:ns-speech-synthesizer))
   (voices :accessor voices 
           :initform (coerce-obj (#/availableVoices ns:ns-speech-synthesizer) 'list))))

(defmethod initialize-instance :after ((self speech-data) 
                                       &key &allow-other-keys)
  (#/setVoice: (speech-synth self) 
               (coerce-obj (first (voices self)) 'ns:ns-string))
  ;; we do the following so that ccl:terminate will be called before we are
  ;; garbage collected and we can release the speech synth object we created
  (ccl:terminate-when-unreachable self))
  
(defmethod ccl:terminate ((self speech-data))
  (when (speech-synth self)
    (#/release (speech-synth self))))

;; Action methods called as a result of pushing some button

(defmethod start-stop-button-pushed ((self speech-data) title tag)
  (declare (ignore title))
  (with-slots (speech-text speech-synth) self
    (ecase tag
      (0 ;; start talking button
        (let ((stxt (#/string speech-text)))
          (when (or (eql stxt (%null-ptr)) (zerop (#/length stxt)))
            (setf stxt #@"I have nothing to say"))
          (#/startSpeakingString: speech-synth stxt)))
      (1 ;; stop talking button
       (#/stopSpeaking speech-synth)))))

(defmethod set-voice ((self speech-data) title tag)
  (declare (ignore title))
  ;; method called when a voice radio-button is pushed
  (#/setVoice: (speech-synth self) 
               (coerce-obj (nth tag (voices self)) 'ns:ns-string)))

(defmethod make-speech-window ((lc lisp-window-controller))
  (let* ((spc-data (make-instance 'speech-data))
         (scr-txt (make-instance vscrolled-text-view))
         (txt-view (#/documentView scr-txt))
         (voice-label (make-instance 'label-view
                        :title "VOICES"))
         (button-box (make-instance 'button-box-view
                       :titles (list "Start Talking" "Stop Talking")
                       :orientation :h
                       :spacing 30
                       :target spc-data
                       :action-func #'start-stop-button-pushed))
         (titles (mapcar #'(lambda (v)
                             (coerce-obj (#/objectForKey: (#/attributesForVoice: ns:ns-speech-synthesizer
                                                                                 (coerce-obj v 'ns:ns-string))
                                                          #&NSVoiceName)
                                         'string))
                         (voices spc-data)))
         (radio-box (make-instance 'radio-button-array-view
                      :titles titles
                      :equal-width t
                      :target spc-data
                      :action-func #'set-voice))
         (win (make-instance 'ns:ns-window 
                :title "What should I say?"
                :resizable t
                :content-subviews (list scr-txt voice-label button-box radio-box)))
         (cview (#/contentView win)))

    ;; Link the speech-data object to the text object that contains the string to say
    (setf (speech-text spc-data) txt-view)

    ;; Fix the position of all the views relative to the content and each other
    (anchor cview scr-txt (list :leading :top :trailing))
    (anchor cview radio-box (list :leading :bottom :trailing))
    (order-views :orientation :v
                 :align :center-x
                 :views (list scr-txt button-box 20 voice-label radio-box))

    ;; Set a minimum height for the scrolled text box so it never disappears
    (constrain (>= (height scr-txt) 20))

    ;; We can't maintain a constant area for the radio box because the equality that would be required
    ;; can't be expressed as any legal constraint. But we can approximate that by requiring that as the
    ;; height decreases, the width must increase, or vice versa. We do that by asserting that the sum of
    ;; the box height and width must equal 700. This starts to fail if the box height gets
    ;; too small, so we'll make sure that doesn't happen either.
    (constrain (= (height radio-box) (- 700 (width radio-box))))
    (constrain (>= (height radio-box) 90))

    ;; Make sure the window is never made so narrow that the buttons disappear
    (constrain (>= (width cview) (+ (width button-box) 40)))

    ;; return the window and all instantiated objects
    (values win (list spc-data scr-txt voice-label button-box radio-box win))))
  
;; test function

(defun test-speech ()
  (on-main-thread 
   (let ((wc (make-instance 'lisp-window-controller
               :build-method #'make-speech-window)))
     (show-window wc)
     wc)))

(provide :speech-controller)