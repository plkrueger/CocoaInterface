;; table-utils.lisp

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

#|
Implements functionality to link all objects needed for a fully functional ns-outline-view. Provides 
something almost equivalent to what might be done in interface builder.

This makes it easier to dynamically configure a table view object and dynamically link it into a view.

This should be used in conjunction with a lisp-controller which provides data for the table view.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :objc-initialize))

(in-package :lv)

(defmethod table-scroll-view ((self ns:ns-table-view))
  (#/enclosingScrollView self))

(defmethod table-controller ((self ns:ns-table-view))
  (let ((lc (#/dataSource self)))
    (and (not (eql lc (%null-ptr)))
         (typep lc 'lisp-controller)
         lc)))

(defmethod table-width ((self ns:ns-table-view))
  (ns:ns-rect-width (#/frame self)))

(provide :table-utils)
