;; save-panel.lisp

;; creates and runs an NSSavePanel and returns a string corresponding to the
;; item selected

#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

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
  (require :ns-object-utils))

(in-package :iu)

(defun save-panel (&key (dir nil)
                        (type nil)
                        (name nil)
                        (prompt nil))
  (let ((sp (#/savePanel ns:ns-save-panel))
        (ns-prompt (and prompt (ccl::%make-nsstring prompt)))
        (ns-dir (and dir (ccl::%make-nsstring (truename dir))))
        (button))
    (when dir
      (#/setDirectoryURL: sp 
                          (#/fileURLWithPath:isDirectory:
                           ns:ns-url 
                           ns-dir
                           #$YES)))
    (when prompt
      (#/setPrompt: sp ns-prompt))
    (when type
      (#/setAllowedFileTypes: sp (lisp-to-ns-array (list (string type)))))
    (when name
      (#/setNameFieldStringValue: sp (lisp-to-temp-nsstring name)))
    (setf button (ccl::call-in-event-process #'(lambda ()
                                                 (#/runModal sp))))
    (prog1 (when (eq button #$NSOKButton)
             (ns-to-lisp-list (#/filenames sp) :element-class (find-class 'string)))
      (when dir (#/release ns-dir))
      (when ns-prompt (#/release ns-prompt)))))
    
(provide :save-panel) 