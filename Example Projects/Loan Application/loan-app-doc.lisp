;; loan-app-doc.lisp

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
  (require :loan-app-win-cntrl))

(in-package :loans)

;; demonstration code for creating a Cocoa document class in lisp that can be required
;; and loaded into a standard running CCL IDE (i.e. does not require a stand-alone program).

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

#|
(defclass loan-app-doc (lisp-document)
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
                   :initform 0.0)
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
                       :initform nil))
  (:metaclass ns:+ns-object))
|#

;; Normal lisp methods for our class

(defmethod initialize-instance :after ((self loan-app-doc)
                                       &key &allow-other-keys)
  ;; uses default intial values, some of which may be modifed to make what is shown initially self-consistent
  (compute-new-loan-values self))

(defmethod archive-slots ((self loan-app-doc))
  (declare (ignore self))
  (list 'loan-amount 'interest-rate 'loan-duration 'desired-loan-duration
        'monthly-payment 'origination-date 'first-payment))

(defmethod document-did-open ((self loan-app-doc))
  (compute-new-loan-values self))

(defmethod get-loan-state ((self loan-app-doc))
  ;; returns a list of loan state values suitable for use by set-loan-state
  (list (loan-amount self)
        (interest-rate self)
        (loan-duration self)
        (desired-loan-duration self)
        (monthly-payment self)
        (origination-date self)
        (first-payment self)
        (hide-max-dur self)
        (hide-min-dur self)
        (hide-min-pay self)))

(defmethod set-loan-state ((self loan-app-doc) state-list)
  (setf (loan-amount self) (pop state-list))
  (setf (interest-rate self) (pop state-list))
  (setf (loan-duration self) (pop state-list))
  (setf (desired-loan-duration self) (pop state-list))
  (setf (monthly-payment self) (pop state-list))
  (setf (origination-date self) (pop state-list))
  (setf (first-payment self) (pop state-list))
  (setf (hide-max-dur self) (pop state-list))
  (setf (hide-min-dur self) (pop state-list))
  (setf (hide-min-pay self) (pop state-list))
  (setf (pay-schedule self) nil)
  (compute-new-loan-values self))

(defmethod create-undo ((self loan-app-doc) slot-name)
    (let ((st (get-loan-state self)))
      (set-undo self 
                #'(lambda ()
                    (create-undo self nil)
                    (set-loan-state self st))
                (when slot-name
                  (format nil "set ~a" (string-for-slot-name slot-name))))))

(defmethod set-pay-schedule ((self loan-app-doc))
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

(defmethod print-pay-schedule ((self loan-app-doc) &optional (strm t))
  (format strm 
          "~:{~%On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
          (pay-schedule self)))

(defmethod compute-int-rate ((self loan-app-doc))
  ;; Find a monthly interest rate that makes the rest of the values work.
  ;; There isn't an algebraic solution for the interest rate, so let's search for it.
  ;; Find a suitable search range and do a binary search for it. Even for large interest 
  ;; rates the number of search iterations should be minimal.

  (with-slots (loan-amount monthly-payment desired-loan-duration interest-rate) self
  
    ;; First we'll check to see whether the monthly payment is great than the loan amount.
    ;; If so we'll set the interest rate directly so that the loan is paid off in one month.
    ;; This avoids some ugly arithmetic overflow things that can happen when interest rates
    ;; go off the charts
    ;; The probabiity is very high that the interest rate computed here will result in a
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

(defmethod compute-total-interest ((self loan-app-doc))
  (reduce #'+ (pay-schedule self) 
          :key #'seventh 
          :initial-value 0))

(defmethod compute-new-loan-values ((self loan-app-doc))
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

(defun string-for-slot-name (slot-name)
  (case slot-name
    (loan-amount "loan amount")
    (interest-rate "interest rate")
    (loan-duration "loan duration")
    (monthly-payment "monthly payment")
    (origination-date "origination date")
    (t "")))

(defmethod bound-slot-will-be-modified ((self loan-app-doc) slot-name)
  ;; Create an undo to restore the current state before changing the slot value.
  (create-undo self slot-name))

(defmethod bound-slot-modified ((self loan-app-doc) slot-name)
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

;; Printing Support

(defmethod print-lines ((self loan-app-doc) lines-per-page)
  (declare (ignore lines-per-page))
  (let ((start-lines (list (format nil 
                                   "Loan ID: ~a" 
                                   (coerce-obj (#/displayName self) 'string))
                           (format nil 
                                   "Amount: $~$"
                                   (/ (loan-amount self) 100))
                           (format nil 
                                   "Origination Date: ~a"
                                   (date-string (origination-date self)))
                           (format nil 
                                   "Annual Interest Rate: ~7,4F%"
                                   (* 100 (interest-rate self)))
                           (format nil 
                                   "Loan Duration: ~D month~:P"
                                   (loan-duration self))
                           (format nil 
                                   "Monthly Payment: $~$"
                                   (/ (monthly-payment self) 100))
                           ""))
        (pay-lines nil))
    (dolist (sched-line (pay-schedule self))
      (push (format nil
                    "~1{On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
                    sched-line)
            pay-lines))
    (nconc start-lines (nreverse pay-lines))))

(defun init-loan-app (app)
  ;; The function that gets called to initialize the application
  ;; It creates and installs a main menu.
  (let ((*app-name-for-menus* "Loan"))
    (#/setMainMenu: app (standard-main-menu))
    (set-windows-menu)))

(defmethod window-build-funcs ((self loan-app-doc))
  (list #'make-loan-window))

(defmethod document-window-controller-classes ((self loan-app-doc))
  (list (find-class 'loan-app-win-controller)))
    
(provide :loan-app-doc)
