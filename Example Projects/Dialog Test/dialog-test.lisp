;; dialog-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-initialize)
  (require :menu-utils)
  (require :constraint-layout)
  (require :window-utils))

(in-package :package-def-package)

(defpackage :dialog-test
  (:nicknames :dtst)
  (:use :ccl :common-lisp :lv :iu))

(in-package :dtst)

(defclass dt-app-delegate (ns:ns-object)
  ((win-ctrl :accessor win-ctrl
             :initform nil))
  (:metaclass ns:+ns-object))

(objc::defmethod (#/dealloc :void)
                 ((self dt-app-delegate))
  (#/release (win-ctrl self))
  (call-next-method)
  (objc:remove-lisp-slots self))

(objc::defmethod (#/openDialog: :void)
                 ((self dt-app-delegate) (sender :id))
  (declare (ignore sender))
  (let* ((panel (#/init (#/alloc ns:ns-open-panel)))
         (pcl (#/class panel))
         (sc1 (#/superclass pcl))
         (sc2 (#/superclass sc1))
         (sc3 (#/superclass sc2)))
    (ns-log-format "Hi there")
    (ns-log-format "Open Panel Class = ~s" pcl)
    (ns-log-format "Superclass 1 = ~s" sc1)
    (ns-log-format "Superclass 2 = ~s" sc2)
    (ns-log-format "Superclass 3 = ~s" sc3)
    (#/runModal panel)))

;; app initialization function
(defun init-dialog-test (app)
  (handler-case
      (progn
        (let ((app-del (make-instance 'dt-app-delegate)))
          (#/setDelegate: app app-del)
          (let ((*app-name-for-menus* "DialogTest"))
            (#/setMainMenu: app (standard-main-menu))
            (set-windows-menu))
          (let* ((tst-button (#/autorelease (make-instance 'ns:ns-button
                                              :title "Do It"
                                              :bezel-style :round-rect
                                              :button-type :momentary-push-in
                                              :target app-del
                                              :action "openDialog:")))
                 (win (make-instance ns:ns-window
                        :resizable t
                        :title "Dialog Test"
                        :content-subviews (list tst-button)))
                 (cv (#/contentView win)))
            (center-in-view cv tst-button)
            (setf (win-ctrl app-del) (show-window-in-controller win)))))
    (condition (c)
               (ns-log-format "condition: ~s" c))))
  
(provide :dialog-test)
    