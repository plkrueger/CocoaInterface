;; window-utils.lisp

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
  (require :objc-initialize)
  (require :constraint-layout)
  (require :scroll-view))

(in-package :iu)

(defmethod show-window-in-controller ((win ns:ns-window))
   (on-main-thread
    (let ((wc (#/initWithWindow: (#/alloc ns:ns-window-controller) win)))
      (#/flushWindow win)
      (#/showWindow: wc (%null-ptr))
      wc)))

(defmethod window-titled ((title string))
  (find title
        (coerce-obj (#/windows #$NSApp) 'list)
        :key #'(lambda (w)
                 (coerce-obj (#/title w) 'string))
        :test #'string=
        :from-end t))

(defmethod (setf window-frame-size) (sz (self ns:ns-window))
  (let* ((current-frame (#/frame self))
         (ns-sz (coerce-obj sz 'ns:ns-size)))
    (#/setFrame:display: self
                         (ns:make-ns-rect (ns:ns-rect-x current-frame) (ns:ns-rect-y current-frame)
                                          (ns:ns-size-width ns-sz) (ns:ns-size-height ns-sz))
                         #$YES)))

(defmacro with-stream-window ((text-strm &optional (win-title "Progress Log")) &rest forms)
  (let ((v (gensym))
        (win (gensym))
        (cv (gensym)))
    `(let* ((,v (make-instance 'lv::vscrolled-text-view
                  :selectable nil))
            (,win (make-instance ns:ns-window
                    :resizable t
                    :title ,win-title
                    :content-subviews (list ,v)))
            (,cv (#/contentView ,win))
            (,text-strm (lv::view-stream ,v)))
       (lv::anchor ,cv ,v (list :top :bottom :leading :trailing))
       (show-window-in-controller ,win)
       ,@forms)))

(provide :window-utils)