;; controller-test.lisp

;; Test window that displays lisp lists using an NSTableView

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :lisp-controller)
  (require :ns-string-utils)
  (require :list-utils)
  (require :nslog-utils)
  (require :constraint-layout)
  (require :date)
  (require :window-controller))

(in-package :ct)

(defclass lisp-controller-test (lisp-window-controller)
   ((lisp-ctrl :accessor lisp-ctrl))
  (:metaclass ns:+ns-object))

(defun make-dated-list ()
  (list (now) 0 (random 20) (random 30)))

(defmethod selected-cell ((self lisp-controller-test) controller root row-num col-num obj)
  (declare (ignore controller root))
  (cond ((and (minusp row-num) (minusp col-num))
         (ns-log "Nothing selected"))
        ((minusp row-num)
         (ns-log (format nil "Selected column ~s with title ~s" col-num obj)))
        ((minusp col-num)
         (ns-log (format nil "Selected row ~s: ~s" row-num obj)))
        (t
         (ns-log (format nil "Selected ~s in row ~s,  col ~s" obj row-num col-num)))))

(defmethod edited-cell ((self lisp-controller-test) controller root row-num col-title obj old-val new-val)
  (declare (ignore controller root obj))
  (ns-log-format "Changed ~s in row ~s, ~a to ~s"
                 old-val row-num col-title new-val))

(defmethod added-row ((self lisp-controller-test) controller root parent new-row)
  (declare (ignore controller root))
  (ns-log-format "Added ~s to ~s" new-row parent))

(defmethod removed-row ((self lisp-controller-test) controller root parent old-row)
  (declare (ignore controller root))
  (ns-log-format "Removed row ~s from ~s " old-row parent))

(defmethod make-ctest-window ((wc lisp-controller-test))
  (let* ((lc (make-instance 'lisp-controller
               :col-ids (list "ColA" "ColB" "ColC" "ColD")
               :root nil
               :initform '(make-dated-list)
               :sort-key #'second
               :sort-pred #'>
               :col-keys (list #'first #'second #'third #'fourth)
               :func-owner wc
               :select-func #'selected-cell
               :edited-func #'edited-cell
               :added-func #'added-row
               :removed-func #'removed-row
               :allows-multiple-selection t
               :allows-column-selection t))
         (add-button (make-instance ns:ns-button
                       :title "+"
                       :action "insert:"
                       :target lc
                       :bezel-style :round-rect
                       :button-type :momentary-light))
         (del-button (make-instance ns:ns-button
                       :title "-"
                       :action "remove:"
                       :target lc
                       :bezel-style :round-rect
                       :button-type :momentary-light))
         (df (#/autorelease (make-instance ns:ns-date-formatter
                              :date-style :short
                              :time-style :none)))
         (nf (#/autorelease (make-instance ns:ns-number-formatter)))
         (col-a (#/autorelease (make-instance ns:ns-table-column
                                 :column-title "Column A"
                                 :identifier "ColA"
                                 :min-width 60
                                 :editable t
                                 :formatter df)))
         (col-b (#/autorelease (make-instance ns:ns-table-column
                                 :column-title "Column B"
                                 :identifier "ColB"
                                 :min-width 60
                                 :editable t
                                 :formatter nf)))
         (col-c (#/autorelease (make-instance ns:ns-table-column
                                 :column-title "Column C"
                                 :identifier "ColC"
                                 :min-width 60
                                 :editable t)))
         (col-d (#/autorelease (make-instance ns:ns-table-column
                                 :column-title "Column D"
                                 :identifier "ColD"
                                 :min-width 60
                                 :editable t)))
         (tv (make-instance 'ns:ns-table-view
               :columns (list col-a col-b col-c col-d)
               :data-source lc
               :delegate lc
               :allows-column-resizing t
               :column-autoresizing-style :uniform))
         (sv (make-instance ns:ns-scroll-view
               :autohides-scrollers t
               :has-horizontal-scroller t
               :has-vertical-scroller t
               :document-view tv))
         (win (make-instance 'ns:ns-window
                :title "Lisp Controller Test"
                :resizable t
                :content-subviews (list sv add-button del-button)))
         (cview (#/contentView win)))

    ;; set the lisp-ctrl field in the window controller
    (setf (lisp-ctrl wc) lc)

    ;; set the view for the lisp-controller
    (setf (view lc) tv)

    (bind add-button "enabled" lc "canInsert")
    (bind del-button "enabled" lc "canRemove")

    (anchor cview sv (list :leading :top :trailing))
    (order-views :orientation :v
                 :views (list sv add-button :margin)
                 :align :leading)
    (order-views :orientation :h
                 :views (list add-button del-button)
                 :align :center-y)

    ;; Return the window, the list of objects created
    (values win (list lc add-button del-button tv sv win))))

(defun test-controller ()
  (on-main-thread
   (let ((wc (make-instance 'lisp-controller-test
               :build-method #'make-ctest-window)))
     (show-window wc)
     wc)))
                  
(provide :controller-test1)