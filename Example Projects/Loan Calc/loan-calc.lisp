;;; loan-calc.lisp

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
  (require :lisp-controller)
  (require :window-utils)
  (require :date)
  (require :decimal)
  (require :constraint-layout)
  (require :window-controller)
  (require :text-views)
  (require :radio-button))

(in-package :lnc)

;; The loan-window-controller class

(defclass loan-window-controller (lisp-window-controller)
  ((loan :accessor loan
         :initarg :loan
         :kvo "loan")
   (loan-text :accessor loan-text)
   (int-text :accessor int-text)
   (dur-text :accessor dur-text)
   (pay-text :accessor pay-text)
   (int-slider :accessor int-slider)
   (dur-slider :accessor dur-slider)
   (pay-slider :accessor pay-slider))
  (:metaclass ns:+ns-object))

;; Action methods that are called when controls do something

(defmethod set-compute-mode ((self loan-window-controller) title cm)
  (declare (ignore title))
  ;; called when one of the loan mode radio buttons is pushed
  ;; We use the button tag as the compute mode
  (with-slots (loan loan-text int-text dur-text pay-text int-slider 
                    dur-slider pay-slider) self
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
      (compute-new-loan-values loan))))

(objc:defmethod (#/windowWillClose: :void) 
                ((self loan-window-controller) (notif :id))
  (declare (ignore notif))
  (when (loan self)
    ;; Tell the loan that the window is closing
    ;; It will #/autorelease this window-controller)
    (window-closed (loan self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Below relates to the loan data model

(defconstant $max-interest-rate$ .5)

;; Some loan utility functions

;; all equations are derived from a basic loan equation for an N month loan:
;; LoanPrinciple = MonthlyPayment * ( (1 / (1 + MonthlyInterest)) + 
;;                                    (1 / (1 + MonthlyInterest)^2) + ... + 
;;                                    (1 / (1 + MonthlyInterest)^N) )
;; which can be manipulated to derive:
;; MonthlyPayment = LoanPrinciple * (MonthlyInterest + (MonthlyInterest / ((1 + MonthlyInterest)^LoanDuration - 1)))

(defun pay-to-loan-ratio (mo-int loan-dur)
  ;; Just computes the MonthlyPayment/Loan ratio from the basic loan equation given above
  (if (zerop mo-int) 0 
    (+ mo-int (/ mo-int (1- (expt (1+ mo-int) loan-dur))))))

;; The loan class

(defclass loan ()
  ((loan-amount :accessor loan-amount
                :kvo "loanAmt"
                :initform 1000000)
   (interest-rate :accessor interest-rate
                  :kvo "interestRate"
                  :initform 0.04)
   (loan-duration :accessor loan-duration
                  :kvo "loanDuration"
                  :initform 12)
   (desired-loan-duration :accessor desired-loan-duration
                          :initform 12)
   (monthly-payment :accessor monthly-payment
                    :kvo "monthlyPayment"
                    :initform 10000)
   (origination-date :accessor origination-date
                     :kvo "originationDate"
                     :initform (now))
   (first-payment :accessor first-payment
                  :kvo "firstPayment"
                  :initform (next-month (now)))
   (pay-schedule :accessor pay-schedule
                 :initform nil)
   (total-interest :accessor total-interest
                   :kvo "totalInterest"
                   :initform 0)
   (window-controller :accessor window-controller
                      :initform nil)
   (compute-mode :accessor compute-mode
                 :initform 0)
   (hide-max-dur :accessor hide-max-dur
                 :kvo "maxDur"
                 :initform t)
   (hide-min-dur :accessor hide-min-dur
                 :kvo "minDur"
                 :initform t)
   (hide-min-pay :accessor hide-min-pay
                 :kvo "minPay"
                 :initform t)
   (computing-new-loan :accessor computing-new-loan
                       :initform nil)))

;; Lisp methods for our class

(defmethod initialize-instance :after ((self loan) 
                                       &key &allow-other-keys)
  (setf (window-controller self)
        (make-instance 'loan-window-controller
          :build-method #'make-loan-window
          :loan self))
  (compute-new-loan-values self)
  (show-window (window-controller self)))

(defmethod window-closed ((self loan))
  ;; called by the window-controller to tell us that the window closed
  (when (window-controller self)
    (#/autorelease (window-controller self))
    (setf (window-controller self) nil)))

(defmethod close-loan ((self loan))
  ;; tell the window to close
  (when (window-controller self)
    (close-window (window-controller self))))

(defmethod set-pay-schedule ((self loan))
  ;; create a detailed payment schedule for the loan using daily compounding of interest 
  ;; Payments are on the same date of each month, but the number of days between payments
  ;; varies because the number of days in each month varies.
  ;; We compute accurate interest compounded daily for the actual number of days.
  (let ((monthly-interest (/ (interest-rate self) 12))
        (payment (monthly-payment self))
        (sched nil)
        (display-min-pay-banner nil))
    (prog1
        (do* ((begin (loan-amount self) end)
              (begin-date (first-payment self) end-date)
              (end-date (next-month begin-date) (next-month begin-date))
              (int (round (* begin monthly-interest))
                   (round (* begin monthly-interest)))
              (end (- (+ begin int) payment) (- (+ begin int) payment)))
             ((not (plusp end)) 
              (progn
                (push (list (short-date-string begin-date) 
                            (/ begin 100)
                            (/ int 100)
                            (/ payment 100)
                            (short-date-string end-date) 
                            (/ end 100)
                            int)
                      sched)
                (setf (pay-schedule self) (nreverse sched))))
          (when (>= end begin)
            ;; oops, with this combination of values the loan will never 
            ;; be paid off, so let's set a minimum payment required
            ;; Display a field that tells user the minimum payment was reached 
            (setf display-min-pay-banner t)
            (setf (monthly-payment self) (1+ int))
            ;; now patch up our loop variables and keep going
            (setf payment (monthly-payment self))
            (setf end (1- begin)))
          ;; put the last payment into the list
          (push (list (short-date-string begin-date) 
                      (/ begin 100)
                      (/ int 100)
                      (/ payment 100)
                      (short-date-string end-date) 
                      (/ end 100)
                      int)
                sched))
      (setf (total-interest self) (compute-total-interest self))
      (if display-min-pay-banner
        (progn
          ;; Set a condition that says the minimum payment was reached 
          (setf display-min-pay-banner t)
          (setf (hide-min-pay self) nil))
        ;; otherwise reset that condition
        (setf (hide-min-pay self) t))
      ;; If we happen to be computing the interest rate, then 
      ;; the combination of loan-amount and monthly payment will
      ;; determine a maximum interest rate. This, in turn, 
      ;; determines a maximum loan duration. If the duration was set
      ;; longer than this by the user, we will reset the 
      ;; lone duration value to the maximum needed.
      ;; If, on the other hand, the monthly payment is set so low that
      ;; the interest rate approaches 0, then we may have to adjust the
      ;; loan duration up to the minimum needed to pay the loan.
      ;; Let's start by resetting our two "duration" conditions and then we'll
      ;; set them if conditions dictate.
      ;; Reset a condition that indicates the max duration was reached 
      (setf (hide-max-dur self) t)
      ;; Reset a condition that indicates the min duration was reached 
      (setf (hide-min-dur self) t)
      (let ((duration-diff (- (desired-loan-duration self) (list-length (pay-schedule self)))))
        (unless (or (eql (compute-mode self) 2) (zerop duration-diff))
          ;; i.e. we're not calling this function just to determine the loan duration
          ;; and we have to adjust the loan duration
          (if (plusp duration-diff)
            (progn
              ;; change the loan-duration value to what it must be
              (setf (loan-duration self) (list-length (pay-schedule self)))
              (when (> duration-diff 2)
                ;; If we're one-off just fix it and don't post a message
                ;; This can occur almost anytime because of numerical issues
                ;; Display a field that tells user the max duration was reached 
                (setf (hide-max-dur self) nil)))
            (progn
              ;; change the loan-duration value to what it must be
              (setf (loan-duration self) (list-length (pay-schedule self)))
              (when (< duration-diff -2)
                ;; If we're one-off just fix it and don't post a message
                ;; This can occur almost anytime because of numerical issues
                ;; Display a field that tells user the min duration was reached 
                (setf (hide-min-dur self) nil)))))))))

(defmethod print-pay-schedule ((self loan) &optional (strm t))
  (format strm 
          "~:{~%On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
          (pay-schedule self)))

(defmethod compute-int-rate ((self loan))
  ;; Find a monthly interest rate that makes the rest of the values work.
  ;; There isn't an algebraic solution for the interest rate, so let's search for it.
  ;; Find a suitable search range and do a binary search for it. Even for large interest 
  ;; rates the number of search iterations should be minimal.

  (with-slots (loan-amount monthly-payment desired-loan-duration interest-rate) self
  
    ;; First we'll check to see whether the monthly payment is greater than the loan amount.
    ;; If so we'll set the interest rate directly so that the loan is paid off in one month.
    ;; This avoids some ugly arithmetic overflow things that can happen when interest rates
    ;; go off the charts
    ;; The probability is very high that the interest rate computed here will result in a
    ;; loan duration that is one month more than that desired. So we'll just reduce the 
    ;; desired duration by one month before beginning.
    (let ((max-monthly-rate (/ $max-interest-rate$ 12))
          (effective-duration (1- desired-loan-duration)))
      (if (>= monthly-payment loan-amount)
        (min max-monthly-rate (1- (/ monthly-payment loan-amount)))
        (let ((imin (max 0 (min max-monthly-rate
                                (/ (- (* monthly-payment effective-duration) loan-amount) 
                                   (* effective-duration loan-amount)))))
              ;; imin is basically a rate that would result in the first month's interest as 
              ;; the average interest paid for all months. Since we know it must be greater 
              ;; than this, we have a guaranteed lower bound. But we cap it at our allowed 
              ;; monthly maximum interest.
              (imax (min max-monthly-rate 
                         (- (/ monthly-payment loan-amount) .000008333)))
              ;; imax is a rate that would result in the first month's interest being 
              ;; minimally smaller than the payment. Since we must pay off in a finite
              ;; duration, this is a guaranteed maximum. We cap it the allowed maximum 
              ;; monthly rate.
              (target-p-l-ratio (/ monthly-payment loan-amount)))
          (unless (>= imax imin)
            (error "Max int = ~8,4f, Min int = ~8,4f" imax imin))
          (do* ((i (/ (+ imin imax) 2) 
                   (/ (+ imin imax) 2))
                (p-l-ratio (pay-to-loan-ratio i effective-duration) 
                           (pay-to-loan-ratio i effective-duration)))
               ((<= (- imax imin) .000001) imax)
            (if (>= target-p-l-ratio p-l-ratio)
              (setf imin i)
              (setf imax i))))))))

(defmethod compute-total-interest ((self loan))
  (reduce #'+ (pay-schedule self) 
          :key #'seventh 
          :initial-value 0))


(defmethod compute-new-loan-values ((self loan))
  ;; For the sake of expediency we assume monthly componding

  ;; First check to see if we're already running this method. If so
  ;; just get out. 
  (when (computing-new-loan self)
    (return-from compute-new-loan-values))
  (setf (computing-new-loan self) t)
  (with-slots (compute-mode interest-rate loan-duration desired-loan-duration monthly-payment 
                            loan-amount pay-schedule) self
    (case compute-mode
      (0
       ;; compute the loan amount
       (unless (or (zerop interest-rate)
                   (zerop desired-loan-duration)
                   (zerop monthly-payment))
         (setf loan-amount 
               (round (/ monthly-payment 
                         (pay-to-loan-ratio (/ interest-rate 12)
                                            desired-loan-duration))))
         (set-pay-schedule self)))
      (1
       ;; compute the interest rate
       (unless (or (zerop loan-amount)
                   (zerop desired-loan-duration)
                   (zerop monthly-payment))
         (setf interest-rate 
               (* 12.0 (/ (floor (* 1000000 (compute-int-rate self)))
                        1000000)))
         (set-pay-schedule self)))
      (2
       ;; compute the loan duration
       (unless (or (zerop interest-rate)
                   (zerop loan-amount)
                   (zerop monthly-payment))
         (set-pay-schedule self)
         (setf loan-duration
               (list-length pay-schedule))
         (setf desired-loan-duration
                (list-length pay-schedule))))
      (3
       ;; compute the monthly payment
       (unless (or (zerop interest-rate)
                   (zerop loan-amount)
                   (zerop desired-loan-duration))
         (setf monthly-payment
               (round (* loan-amount 
                         (pay-to-loan-ratio (/ interest-rate 12) 
                                            desired-loan-duration))))
         (set-pay-schedule self)))))
  (setf (computing-new-loan self) nil))

(defmethod bound-slot-modified ((self loan) slot-name)
  ;; will only be called if a slot is modified as the result of a KVO binding
  ;; any other change will not result in calling this method
  (when (eq slot-name 'origination-date)
    ;; also set the first-payment date
    (setf (first-payment self) (next-month (origination-date self))))
  (when (eq slot-name 'loan-duration)
    ;; also set the desired-loan-duration
    (setf (desired-loan-duration self) (loan-duration self)))
  (setf (pay-schedule self) nil)
  (compute-new-loan-values self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-loan-window ((lc loan-window-controller))
  (with-slots (loan loan-text int-text dur-text pay-text int-slider dur-slider pay-slider) lc
    (let* ((df (#/autorelease (make-instance ns:ns-date-formatter
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
                         :label-pos :top
                         :width 75
                         :alignment :center
                         :formatter df))
           (first-payment-tf  (make-instance 'labeled-text-field
                                :title "First Payment"
                                :editable nil
                                :selectable nil
                                :label-pos :top
                                :width 75
                                :alignment :center
                                :formatter df))
           (loan-amount-tf (make-instance 'labeled-text-field
                             :title "Loan Amount"
                             :label-pos :top
                             :width 90
                             :alignment :right
                             :formatter mf
                             :enabled nil))
           (tot-int-tf  (make-instance 'labeled-text-field
                          :title "Total Interest Paid:"
                          :editable nil
                          :selectable nil
                          :label-pos :left
                          :width 100
                          :formatter mf
                          :alignment :right))
           (ann-int-rt-tf (make-instance ns:ns-text-field
                            :width 90
                            :alignment :right
                            :formatter intf))
           (ann-int-rt-label (make-instance 'label-view
                               :title "Annual Interest Rate %"))
           (loan-dur-tf (make-instance ns:ns-text-field
                          :width 90
                          :alignment :right))
           (loan-dur-label (make-instance 'label-view
                               :title "Loan Duration (months)"))
           (month-pmt-tf (make-instance ns:ns-text-field
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
                                          warn1 warn2 warn3)))
           (cv (#/contentView win)))

      ;; we want to enable and disable the four text fields that depend on the current loan
      ;; computation mode, so make pointers to these views available to the loan-window-controller
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
                        ann-int-rt-label loan-dur-tf loan-dur-label month-pmt-label
                        warn1 warn2 warn3 win)))))


;; test by
(defun test-loan ()
  ;; up to caller to #/release the returned loan instance
  ;; but only after window is closed or crash will occur
  (on-main-thread
   (make-instance 'loan)))

(provide :loan-calc)