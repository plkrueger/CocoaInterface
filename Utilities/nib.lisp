;; nib.lisp
;; Start with some corrected functions from .../ccl/examples/cocoa/nib-loading/HOWTO.html


;; this isn't used here, but almost anyone who wants the rest of this file will want this too
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :file-directory-utils)
  (require :interactive-app))

(in-package :iu)

;; Note that if the key :retain-top-objs is nil, then callers of this function are 
;; responsible for retaining top-level objects if they're going to be around for a while.
;; Caller is always resposible for releasing any retained objects when no longer needed.
;; If :bundle key is not provided then nib-path must be a complete path to the nib. If
;; :bundle key is provided, then nib-path can be just the name of the nib, which need not
;; include the .nib extension.

(defun load-nibfile (nib-path &key (nib-owner #&NSApp) (retain-top-objs nil) (bundle ns:ns-bundle))
  (let* ((nib-name (if (subtypep (type-of nib-path) 'ns:ns-string)
                     (#/retain nib-path)
                     (ccl::%make-nsstring (namestring nib-path))))
         (result nil)
         (toplevel-objects nil))
    (rlet ((objs-ref :id (%null-ptr)))
      (when (setf result
                  (#/loadNibNamed:owner:topLevelObjects: bundle nib-name nib-owner objs-ref))
        (let ((object-array (pref objs-ref :id)))
          (when (typep object-array 'ns:ns-array)
            (setf toplevel-objects (coerce-obj object-array 'list))))
        (when retain-top-objs
          (dolist (obj toplevel-objects)
            (#/retain obj)))))
    (#/release nib-name)
    (values toplevel-objects result)))

(defun load-window-nibfile (nib-path &key
                                     (window-controller (#/alloc ns:ns-window-controller))
                                     (nib-owner nil))
  ;; Initializes a window-controller to the specified nib and returns it
  ;; Makes the window-controller the window owner if none is specified
  (let* ((nib-name (if (subtypep (type-of nib-path) 'ns:ns-string)
                     (#/retain nib-path)
                     (ccl::%make-nsstring (namestring nib-path))))
         (wc (#/initWithWindowNibPath:owner: window-controller
                                             nib-name
                                             (or nib-owner window-controller))))
    (#/release nib-name)
    ;; Note that both dict and objects-array are temporary (i.e. autoreleased objects)
    ;; so don't need to be released by us
    wc))

(defun compile-xib (xib-path &optional (nib-path (parse-namestring ".nib")))
  ;; Compiles a .xib to a .nib file format that can be loaded into Lisp.
  ;; If no .nib path is specifed, the same name as the .xib will be used (with a .nib extension)
  ;; Note that ibtool will overwrite the .nib file if it currently exists
  (let* ((xib (truename xib-path))
         (nib (and xib (merge-pathnames nib-path xib))))
    (unless xib 
      (error "File ~s does not exist." xib-path))
    (let ((result (iu::shell-command 
                   (format nil 
                           ;;"ibtool --errors --warnings --notices --output-format human-readable-text --compile ~a ~a"
                           "ibtool --compile ~s ~s"
                           (namestring nib)
                           (namestring xib)))))
      (if (string= "" result)
        nib
        result))))

#| testing stuff

(setf pn (iu::find-in-krueger-contrib "hello.xib"))
(iu::compile-xib pn (parse-namestring "hello2.nib"))
  which should return 
#P"/Applications/ccl/contrib/cocoa-ide/krueger/InterfaceProjects/Hello World/hello2.nib"
  if it returns some string of errors, make sure developer tools are installed and that ibtool exists

(load-nibfile *)
   should bring up Hello World window

|#

(provide :NIB)