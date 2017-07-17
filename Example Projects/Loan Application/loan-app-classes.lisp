;; loan-app-classes 

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
  (require :lisp-document))

(in-package :package-def-package)
(defpackage :loan-application
  (:nicknames :loans)
  (:use :iu :ccl :common-lisp :lv))

(in-package :loans)

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

(provide :loan-app-classes)