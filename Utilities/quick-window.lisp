;; quick-window.lisp

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
  (require :ns-string-utils))

(in-package :iu)

(defun make-quick-window (&key (titled t)
                               (title "Test Quick Window")
                               (closable t)
                               (resizable nil)
                               (miniaturizable t)
                               (editable-text nil)
                               (text "Quick Window Text")
                               (width 190)
                               (height 60)
                               (content-view nil))
  ;; makes and returns a window that the caller owns and must release when done
  (on-main-thread
   (let ((win (#/initWithContentRect:styleMask:backing:defer: 
               (#/alloc ns:ns-window)
               (ns:make-ns-rect 0 0 width height)
               (+ (if titled #$NSTitledWindowMask 0)
                  (if closable #$NSClosableWindowMask 0)
                  (if resizable #$NSResizableWindowMask 0)
                  (if miniaturizable #$NSMiniaturizableWindowMask 0))
               #$NSBackingStoreBuffered
               #$NO))
         (label (#/initWithFrame: (#/alloc ns:ns-text-view) (ns:make-ns-rect 0 0 width height))))
     (when (and titled title)
       (#/setTitle: win (lisp-to-temp-nsstring title)))
     (if content-view
         (#/setContentView: win content-view)
         (progn
           ;; text argument can be either a string or attributed-string instance
           (#/setBackgroundColor: label (#/backgroundColor win))
           (#/setAttributedString: (#/textStorage label) (lisp-to-ns-object text :rich-text))
           (#/setEditable: label (if editable-text #$YES #$NO))
           (#/addSubview: (#/contentView win) label)))
     (#/autorelease label)
     (values win
             (or content-view label)))))
  
(defun show-quick-window (win)
  (on-main-thread
   (let ((wc (#/initWithWindow: (#/alloc ns:ns-window-controller) win)))
     (#/showWindow: wc (%null-ptr))
     wc)))

(provide :quick-window)