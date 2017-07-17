;; demo-packages.lisp

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

Defines all packages for all demo codes so this can be loaded once before everything
else and keep names straight. This requires :interface-packages as well, so demo code
need never do this itself.

|#

(assert (>= #$NSAppKitVersionNumber #$NSAppKitVersionNumber10_7)
        nil
        "CCL version 1.9 and OSX 10.7 or higher are required to run all demo code")

;; The following requires will be needed in all demos, so just do them here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :objc-initialize)
  (require :dev-tools))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :hello

(defpackage :cards
  (:use :ccl :common-lisp :iu :lv :lc)
  (:export 
   ;; cards.lisp
   test-deal))

(defpackage :controller-test
  (:nicknames :ct)
  (:use :ccl :common-lisp :iu :lv :lc)
  (:export 
   ;;test-controller.lisp
   test-controller))

(defpackage :hello
  (:use :ccl :common-lisp :iu :lv)
  (:export
   ;; hello.lisp
   hello))

(defpackage :loan-calc
  (:nicknames :lnc)
  (:use :iu :ccl :common-lisp :lv)
  (:export 
   ;; loan-calc.lisp
   test-loan))

(defpackage :loan-document
  (:nicknames :lnd)
  (:use :iu :ccl :common-lisp :lv))

(defpackage :menu-test 
  (:nicknames :mt)
  (:use :iu :ccl :common-lisp :lv)
  (:export 
   ;; menu-test.lisp
   test-menu
   test-menu2))

(defpackage :package-view
  (:nicknames :pv)
  (:use :ccl :common-lisp :iu :lv :lc)
  (:export 
   ;;package-view.lisp
   test-package))

(defpackage :simplesum 
  (:nicknames :ss)
  (:use :iu :ccl :common-lisp :lv)
  (:export 
   ;; simplesum.lisp
   test-sum))

(defpackage :speech-controller 
  (:nicknames :spc)
  (:use :iu :ccl :common-lisp :lv)
  (:export 
   ;; speech-controller.lisp
   test-speech))