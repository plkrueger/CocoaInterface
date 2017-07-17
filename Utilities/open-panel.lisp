;; open-panel.lisp

;; creates and runs an NSOpenPanel and returns a list of strings corresponding to the
;; item(s) selected

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
  (require :interface-packages)
  (require :coerce-obj)
  (require :ns-object-utils))

(in-package :iu)

;; The following creates an ns-open-panel.
;; Normally I would have made the functionality in make-open-panel something that you could do
;; with an initialize-instance :after method. In fact it is implemented that way in objc-initialize.lisp.
;; However, I've discovered that if an nsOpenPanel is retained that this results in 
;; some sort of funky condition where a spate of core graphics (CG) trace messages are logged
;; to the system log. So instead we need to create an open panel using the class method (rather than #/init)
;; and then use it immediately rather than keeping it around with retain (which works ok, but results in
;; lots of log messages).
(defun make-open-panel (&key
                        (allows-multiple-selection nil allows-multiple-selection-p)
                        (can-choose-directories nil can-choose-directories-p)
                        (can-choose-files nil can-choose-files-p)
                        (resolves-aliases nil resolves-aliases-p)
                        (accessory-view nil accessory-view-p)  ;; ns-view object
                        (allowed-file-types (%null-ptr)) ;; list of strings of allowed file types
                        (allows-other-file-types nil allows-other-file-types-p)
                        (can-create-directories nil can-create-directories-p)
                        (can-select-hidden-extension nil can-select-hidden-extension-p)
                        (delegate nil delegate-p) ;; NSOpenSavePanelDelegate object
                        (directory-url nil directory-url-p)  ;; ns:ns-url
                        (extension-hidden nil extension-hidden-p)
                        (message nil message-p)
                        (name-field-label nil name-field-label-p)
                        (name-field-string-value nil name-field-string-value-p)
                        (prompt nil prompt-p)
                        (shows-hidden-files nil shows-hidden-files-p)
                        (title nil title-p) ;; string
                        (treats-file-packages-as-directories nil treats-file-packages-as-directories-p)
                        (floating-panel nil floating-panel-p)
                        (works-when-modal nil works-when-modal-p))
  (on-main-thread 
   (let ((self (#/openPanel ns:ns-open-panel)))
     (when floating-panel-p
       (#/setFloatingPanel: self floating-panel))
     (when works-when-modal-p
       (#/setWorksWhenModal: self works-when-modal))
     (when accessory-view-p
       (#/setAccessoryView: self accessory-view))
     (#/setAllowedFileTypes: self (if allowed-file-types
                                      (coerce-obj allowed-file-types 'ns:ns-array)
                                      (%null-ptr)))
     (when allows-other-file-types-p
       (#/setAllowsOtherFileTypes: self allows-other-file-types))
     (when can-create-directories-p
       (#/setCanCreateDirectories: self can-create-directories))
     (when can-select-hidden-extension-p
       (#/setCanSelectHiddenExtension: self can-select-hidden-extension))
     (when delegate-p
       (#/setDelegate: self delegate))
     (when directory-url-p
       (#/setDirectoryURL: self directory-url))
     (when extension-hidden-p
       (#/setExtensionHidden: self extension-hidden))
     (when message-p
       (#/setMessage: self (coerce-obj message 'ns:ns-string)))
     (when name-field-label-p
       (#/setNameFieldLabel: self (coerce-obj name-field-label 'ns:ns-string)))
     (when name-field-string-value-p
       (#/setNameFieldStringValue: self (coerce-obj name-field-string-value 'ns:ns-string)))
     (when prompt-p
       (#/setPrompt: self (coerce-obj prompt 'ns:ns-string)))
     (when shows-hidden-files-p
       (#/setShowsHiddenFiles: self shows-hidden-files))
     (when title-p
       (#/setTitle: self (coerce-obj title 'ns:ns-string)))
     (when treats-file-packages-as-directories-p
       (#/setTreatsFilePackagesAsDirectories: self treats-file-packages-as-directories))
     (when allows-multiple-selection-p
       (#/setAllowsMultipleSelection: self allows-multiple-selection))
     (when can-choose-directories-p
       (#/setCanChooseDirectories: self can-choose-directories))
     (when can-choose-files-p
       (#/setCanChooseFiles: self can-choose-files))
     (when resolves-aliases-p
       (#/setResolvesAliases: self resolves-aliases))
     self)))

;; The following creates and runs an open panel
(defun open-panel (&key (dir nil)
                        (choose-dirs nil)
                        (choose-files t)
                        (multiple nil)
                        (types nil)
                        (prompt nil))
  (on-main-thread 
   (let ((op (#/openPanel ns:ns-open-panel))
         (ns-prompt (and prompt (coerce-obj prompt 'ns:ns-string)))
         (ns-dir (and dir (coerce-obj (truename dir) 'ns:ns-string)))
         (button))
     (#/setCanChooseDirectories: op (if choose-dirs #$YES #$NO))
     (#/setCanChooseFiles: op (if choose-files #$YES #$NO))
     (#/setAllowsMultipleSelection: op (if multiple #$YES #$NO))
     (when prompt
       (#/setPrompt: op ns-prompt))
     (when types
       (#/setAllowedFileTypes: op (if (listp types)
                                    (lisp-to-ns-array types)
                                    (lisp-to-ns-array (list (string types))))))
     (when dir
       (#/setDirectoryURL: op 
                           (#/fileURLWithPath:isDirectory:
                            ns:ns-url 
                            ns-dir
                            #$YES)))
     (setf button (#/runModal op))
     (let ((found (when (eq button #$NSOKButton)
                    (coerce-obj (#/URLs op) 'list))))
       (if multiple found (first found))))))
    
(provide :open-panel)