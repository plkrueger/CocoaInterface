;; loan-pr-view.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :date))

(in-package :lnd)

;; provides a view class used to print loan information

(defclass loan-print-view (ns:ns-view)
  ((loan :accessor loan 
         :initarg :loan)
   (attributes :accessor attributes 
               :initform (make-instance ns:ns-mutable-dictionary))
   (page-line-count :accessor page-line-count
                    :initform 0)
   (line-height :accessor line-height)
   (page-num :accessor page-num
             :initform 0)
   (page-rect :accessor page-rect))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self loan-print-view)
                                       &key &allow-other-keys)
  ;; assure loan still exists if user closes the window while we are printing
  (#/retain (loan self))
  (let* ((font (#/fontWithName:size: ns:ns-font #@"Courier" 8.0)))
    (setf (line-height self) (* (+ (#/ascender font) (abs (#/descender font))) 1.5))
    (#/setObject:forKey: (attributes self) font #$NSFontAttributeName)))

(objc:defmethod (#/dealloc :void)
                ((self loan-print-view))
  (#/release (loan self))
  (#/release (attributes self))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc:defmethod (#/knowsPageRange: :<BOOL>) 
                ((self loan-print-view) (range (:* #>NSRange)))
  ;; compute printing parameters and set the range
  (let* ((pr-op (#/currentOperation ns:ns-print-operation))
         (pr-info (#/printInfo pr-op))
         (image-rect (#/imageablePageBounds pr-info))
         (pg-rect (ns:make-ns-rect 0 
                                   0 
                                   (ns:ns-rect-width image-rect)
                                   (ns:ns-rect-height image-rect))))
    (setf (page-rect self) pg-rect)
    (#/setFrame: self pg-rect)
    (setf (page-line-count self) (floor (ns:ns-rect-height pg-rect) 
                                        (line-height self)))
    ;; start on page 1
    (setf (ns:ns-range-location range) 1)
    ;; compute the number of pages for 9 header lines on page 1 and 2 header
    ;; lines on subsequet pages plus a line per payment
    (let* ((pay-lines-on-p-1 (- (page-line-count self) 7))
           (other-pages-needed (ceiling (max 0 (- (list-length (pay-schedule (loan self)))
                                                  pay-lines-on-p-1))
                                        (page-line-count self))))
      (setf (ns:ns-range-length range)
            (1+ other-pages-needed))))
  #$YES)

(objc:defmethod (#/rectForPage: #>NSRect)
                ((self loan-print-view) (pg #>NSInteger))
  (setf (page-num self) (1- pg))
  (page-rect self))

(objc:defmethod (#/isFlipped #>BOOL)
                ((self loan-print-view))
  ;; we compute coords from upper left
  #$YES)

(objc:defmethod (#/drawRect: :void)
                ((self loan-print-view) (r #>NSRect))
  (with-slots (loan attributes page-line-count line-height page-num page-rect) self
    (ns:with-ns-rect (line-rect (ns:ns-rect-x r) 
                                (- (ns:ns-rect-y r) line-height)
                                (ns:ns-rect-width r) 
                                line-height)
      (labels ((draw-next-line (str)
                 (incf (ns:ns-rect-y line-rect) line-height)
                 (#/drawInRect:withAttributes: 
                  (coerce-obj str 'ns:ns-string)
                  line-rect
                  attributes))
               (draw-next-payment (sched-line)
                 (draw-next-line 
                  (format nil
     "~1{On ~a balance = $~$ + interest of $~$ - payment of $~$ = ~a balance of $~$~}"
                          sched-line))))
        (when (zerop page-num)
          ;; print all the basic loan info
          (draw-next-line (format nil 
                                  "Loan ID: ~a" 
                                  (coerce-obj (#/displayName loan) 'string)))
          (draw-next-line (format nil 
                                  "Amount: $~$"
                                  (/ (loan-amount loan) 100)))
          (draw-next-line (format nil 
                                  "Origination Date: ~a"
                                  (date-string (origination-date loan))))
          (draw-next-line (format nil 
                                  "Annual Interest Rate: ~7,4F%"
                                  (* 100 (interest-rate loan))))
          (draw-next-line (format nil 
                                  "Loan Duration: ~D month~:P"
                                  (loan-duration loan)))
          (draw-next-line (format nil 
                                  "Monthly Payment: $~$"
                                  (/ (monthly-payment loan) 100)))
          ;; draw spacer line
          (incf (ns:ns-rect-y line-rect) line-height))
        ;; print the appropriate schedule lines for this page
        (let* ((lines-per-page (- page-line-count (if (zerop page-num) 7 0)))
               (start-indx (if (zerop page-num) 0 (+ (- page-line-count 7) 
                                                     (* lines-per-page (1- page-num)))))
               (end-indx (min (length (pay-schedule loan)) 
                              (+ start-indx lines-per-page 1))))
          (dolist (sched-line (subseq (pay-schedule loan) start-indx end-indx))
            (draw-next-payment sched-line)))))))

(provide :loan-pr-view)