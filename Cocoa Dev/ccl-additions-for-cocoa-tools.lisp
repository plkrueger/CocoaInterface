;; ccl-additions-for-cocoa-tools

;; This file includes additional functions and methods that arguably should be located
;; elsewhere. Given my reluctance to mess with the standard files they are here instead.

;; should be in cocoa-editor.lisp
;; This is needed because the IDE search function tries to discover what the default
;; directory should be by looking at where open hemlock windows come from. Unfortunately
;; it tries to call this on all open windows with documents, not just hemlock windows, 
;; so we need this hack to avoid a runtime error from in inapplicable method.
;; This should be defined in cocoa-editor.lisp with other versions of this method.

(defmethod gui::document-pathname ((doc ns:ns-document))
  nil)

;; should be in l1-clos-boot.lisp or maybe in objc-clos.lisp
;; This is just an extension of the methods that already exist to objc-class-object

(defmethod ccl::class-prototype ((class objc::objc-class-object))
  (or (ccl::%class.prototype class)
      (setf (ccl::%class.prototype class) (allocate-instance class))))