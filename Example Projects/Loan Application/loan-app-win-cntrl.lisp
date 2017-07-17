;;; loan-app-win-cntrl.lisp

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loan-app-classes)
  (require :constraint-layout)
  (require :window-controller)
  (require :text-views)
  (require :radio-button))

(in-package :loans)

;; The loan-app-win-controller class

#|
(defclass loan-app-win-controller (lisp-window-controller)
  ((orig-date-text :accessor orig-date-text)
   (loan-text :accessor loan-text)
   (int-text :accessor int-text)
   (dur-text :accessor dur-text)
   (pay-text :accessor pay-text)
   (int-slider :accessor int-slider)
   (dur-slider :accessor dur-slider)
   (pay-slider :accessor pay-slider))
  (:metaclass ns:+ns-object))
|#

;; Action methods that are called when controls do something
(defmethod set-compute-mode ((self loan-app-win-controller) title cm)
  (declare (ignore title))
  ;; called when one of the loan mode radio buttons is pushed
  ;; We use the button tag as the compute mode
  (with-slots (loan-text int-text dur-text pay-text int-slider 
               dur-slider pay-slider) self
    (let ((loan (#/document self)))
      (unless (eql cm (compute-mode loan))
        (case (compute-mode loan)
          ;; enable things disabled for the previous compute mode
          (0 (#/setEnabled: loan-text #$YES))
          (1 (#/setEnabled: int-text #$YES)
             (#/setEnabled: int-slider #$YES))
          (2 (#/setEnabled: dur-text #$YES)
             (#/setEnabled: dur-slider #$YES))
          (3 (#/setEnabled: pay-text #$YES)
             (#/setEnabled: pay-slider #$YES)))
        (setf (compute-mode loan) cm)
        (case cm
          ;; disable things as appropriate for the new compute mode
          (0 (#/setEnabled: loan-text #$NO))
          (1 (#/setEnabled: int-text #$NO)
             (#/setEnabled: int-slider #$NO))
          (2 (#/setEnabled: dur-text #$NO)
             (#/setEnabled: dur-slider #$NO))
          (3 (#/setEnabled: pay-text #$NO)
             (#/setEnabled: pay-slider #$NO)))
        (compute-new-loan-values loan)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the build-function for the loan window

(defmethod make-loan-window ((lc loan-app-win-controller))
  (with-slots (loan-text int-text dur-text pay-text int-slider dur-slider pay-slider) lc
    (let* ((loan (#/document lc))
           (df (#/autorelease (make-instance ns:ns-date-formatter
                                :date-style :short
                                :time-style :none)))
           (mf (#/autorelease (make-instance ns:ns-number-formatter
                                :number-style :currency
                                :minimum-fraction-digits 2
                                :maximum-fraction-digits 2
                                :generates-decimal-numbers t
                                :always-shows-decimal-separator t)))
           (intf (#/autorelease (make-instance ns:ns-number-formatter
                                  :number-style :percent
                                  :minimum-fraction-digits 2
                                  :maximum-fraction-digits 2
                                  :minimum-integer-digits 1
                                  :maximum-integer-digits 2
                                  :minimum 0
                                  :maximum .50
                                  :always-shows-decimal-separator t)))
           (orig-dt-tf (make-instance 'labeled-text-field
                         :title "Origination Date"
                         :allows-undo nil
                         :label-pos :top
                         :width 75
                         :alignment :center
                         :formatter df))
           (first-payment-tf  (make-instance 'labeled-text-field
                                :title "First Payment"
                                :allows-undo nil
                                :editable nil
                                :selectable nil
                                :label-pos :top
                                :width 75
                                :alignment :center
                                :formatter df))
           (loan-amount-tf (make-instance 'labeled-text-field
                             :title "Loan Amount"
                             :allows-undo nil
                             :label-pos :top
                             :width 90
                             :alignment :right
                             :formatter mf
                             :enabled nil))
           (tot-int-tf  (make-instance 'labeled-text-field
                          :title "Total Interest Paid:"
                          :allows-undo nil
                          :editable nil
                          :selectable nil
                          :label-pos :left
                          :width 100
                          :alignment :right
                          :formatter mf))
           (ann-int-rt-tf (make-instance ns:ns-text-field
                            :allows-undo nil
                            :width 90
                            :alignment :right
                            :formatter intf))
           (ann-int-rt-label (make-instance 'label-view
                               :title "Annual Interest Rate %"))
           (loan-dur-tf (make-instance ns:ns-text-field
                          :allows-undo nil
                          :width 90
                          :alignment :right))
           (loan-dur-label (make-instance 'label-view
                               :title "Loan Duration (months)"))
           (month-pmt-tf (make-instance ns:ns-text-field
                           :allows-undo nil
                           :width 90
                           :alignment :right
                           :formatter mf))
           (month-pmt-label (make-instance 'label-view
                               :title "Monthly Payment"))
           (int-sl (make-instance ns:ns-slider
                     :continuous t
                     :enabled t
                     :max-value 0.5
                     :min-value 0.0))
           (dur-sl (make-instance ns:ns-slider
                     :continuous t
                     :enabled t
                     :max-value 600.0
                     :min-vlaue 0.0))
           (pay-sl (make-instance ns:ns-slider
                     :continuous t
                     :enabled t
                     :max-value 20000.0
                     :min-vlaue 0.0))
           (radio-box (make-instance radio-button-box-view
                        :titles '("Loan Amount" "Interest Rate" "Loan Duration" "Monthly Payment")
                        :target lc
                        :action-func #'set-compute-mode))
           (rb-box (make-instance ns:ns-box
                     :title "Compute"
                     :title-position :at-top
                     :content-view radio-box
                     :content-view-margins '(4 4)))
           (small-font (#/labelFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)))
           (warn1 (make-instance label-view
                    :title "Above max duration needed to pay loan"
                    :font small-font))
           (warn2 (make-instance label-view
                    :title "Below min duration needed to pay loan"
                    :font small-font))
           (warn3 (make-instance label-view
                    :title "Below min payment needed for first month's interest"
                    :font small-font))
           (win (make-instance ns:ns-window
                  :title "Loan Calculator"
                  :resizable t
                  :content-subviews (list orig-dt-tf first-payment-tf loan-amount-tf tot-int-tf
                                          ann-int-rt-tf month-pmt-tf loan-dur-tf
                                          int-sl dur-sl pay-sl 
                                          rb-box
                                          ann-int-rt-label loan-dur-label month-pmt-label
                                          warn1 warn2 warn3 win)))
           (cv (#/contentView win)))

      ;; we want to enable and disable the four text fields that depend on the current loan
      ;; computation mode, so make pointers to these views available to the loan-app-win-controller
      (setf loan-text (text-field loan-amount-tf))
      (setf int-text ann-int-rt-tf)
      (setf dur-text loan-dur-tf)
      (setf pay-text month-pmt-tf)

      ;; set slider fields
      (setf int-slider int-sl)
      (setf dur-slider dur-sl)
      (setf pay-slider pay-sl)

      ;; To make the warnings show up when we want, bind their "hidden" property
      (bind warn1 "hidden" loan "maxDur")
      (bind warn2 "hidden" loan "minDur")
      (bind warn3 "hidden" loan "minPay")

      ;; bind all the relevant value fields in controls
      (bind first-payment-tf "value" loan "firstPayment")
      (bind loan-amount-tf "value" loan "loanAmt")
      (bind ann-int-rt-tf "value" loan "interestRate")
      (bind loan-dur-tf "value" loan "loanDuration")
      (bind month-pmt-tf "value" loan "monthlyPayment")
      (bind orig-dt-tf "value" loan "originationDate")
      (bind tot-int-tf "value" loan "totalInterest")
      (bind int-sl "value" loan "interestRate")
      (bind dur-sl "value" loan "loanDuration")
      (bind pay-sl "value" loan "monthlyPayment")

      ;; constrain sizes
      (constrain (= (width rb-box) (+ 8 (width radio-box))))
      (constrain (= (height rb-box) (+ 18 (height radio-box))))
      (constrain (= (leading radio-box) (+ 4 (leading rb-box))))
      (constrain-to-natural-size loan-dur-tf)
      (constrain-to-natural-size month-pmt-tf)
      (constrain-to-natural-size ann-int-rt-tf)
      (constrain-to-natural-height int-sl)
      (constrain-to-natural-height dur-sl)
      (constrain-to-natural-height pay-sl)

      ;; constrain the objects in the window

      ;; top row of objects
      (anchor cv orig-dt-tf '(:top :leading))
      (anchor cv first-payment-tf '(:top :center-x))
      (anchor cv loan-amount-tf '(:top :trailing))

      ;; sliders and related text fields
      (order-views :views (list :border int-sl ann-int-rt-tf :border)
                   :orientation :h
                   :align :center-y)
      (order-views :views (list :border dur-sl loan-dur-tf :border)
                   :orientation :h
                   :align :center-y)
      (order-views :views (list :border pay-sl month-pmt-tf :border)
                   :orientation :h
                   :align :center-y)

      ;; align centers of radio box and total-interest text field
      (align-views :views (list rb-box tot-int-tf)
                   :align :center-y)
      (constrain (>= (leading tot-int-tf) (+ 6 (trailing rb-box))))

      ;; Then vertically
      (order-views :views (list orig-dt-tf 30 int-sl 30 dur-sl 30 pay-sl rb-box :border)
                   :align :leading
                   :orientation :v)
      (align-views :views (list loan-amount-tf ann-int-rt-tf loan-dur-tf month-pmt-tf tot-int-tf)
                   :align :trailing)

      ;; constrain label positions
      (constrain (= (top int-sl) (+ 3 (bottom ann-int-rt-label))))
      (align-views :views (list int-sl ann-int-rt-label)
                   :align :center-x)
      (constrain (= (top dur-sl) (+ 3 (bottom loan-dur-label))))
      (align-views :views (list dur-sl loan-dur-label)
                   :align :center-x)
      (constrain (= (top pay-sl) (+ 3 (bottom month-pmt-label))))
      (align-views :views (list pay-sl month-pmt-label)
                   :align :center-x)
      
      ;; constrain warning label positions
      (constrain (= (top warn1) (+ *no-space* (bottom loan-dur-tf))))
      (align-views :views (list warn1 loan-dur-tf)
                   :align :trailing)
      (constrain (= (top warn2) (+ *no-space* (bottom loan-dur-tf))))
      (align-views :views (list warn2 loan-dur-tf)
                   :align :trailing)
      (constrain (= (top warn3) (+ *no-space* (bottom month-pmt-tf))))
      (align-views :views (list warn3 month-pmt-tf)
                   :align :trailing)

      (values win (list orig-dt-tf first-payment-tf loan-amount-tf tot-int-tf
                        ann-int-rt-tf month-pmt-tf int-sl dur-sl pay-sl radio-box rb-box
                        ann-int-rt-label loan-dur-label month-pmt-label
                        warn1 warn2 warn3 win)))))

(provide :loan-app-win-cntrl)
