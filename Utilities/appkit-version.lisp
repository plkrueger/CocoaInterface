;; appkit-version.lisp
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

;; defines constants corresponding to different Apple Cocoa AppKit verstions
;; All this is nice to tell you if the Objective-C runtime supports certain features
;; but it won't tell you what version of appkit was compiled into the Lisp
;; Objective-C bridge code.  For that you may need to test dynamically to see
;; whether the features you want are supported. 8^(

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages))

(in-package :iu)

(let ((versions '(10 (0 (0 577))
                     (1 (0 620))
                     (2 (0 633)
                        (3 633.6))
                     (3 (0 743)
                        (2 743.14)
                        (3 743.2)
                        (5 743.24)
                        (7 743.33)
                        (9 743.36))
                     (4 (0 824)
                        (1 824.1)
                        (3 824.23)
                        (4 824.33)
                        (7 824.41))
                     (5 (0 949)
                        (2 949.27)
                        (3 949.33))
                     (6 (0 1038)
                        (4 1038.32)))))

  (defun app-kit-version  (major minor &optional (inc 0))
    (let* ((major (find major versions :test #'eql :key #'first))
           (minor (find minor (rest major) :test #'<= :key #'first)))
      (find inc (rest minor) :test #'<= :key #'first)))

  (defun satisfies-app-kit-version (major minor &optional (inc 0))
    (>= #&NSAppKitVersionNumber (app-kit-version major minor inc)))

)



  