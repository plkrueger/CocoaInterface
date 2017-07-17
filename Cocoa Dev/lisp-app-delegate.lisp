;; lisp-app-delegate.lisp

;; Templates for various application delegates that might be used.
;; Which one to use depends on the applications needs and objectives

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
  (require :objc-initialize))

(in-package :ad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-IDE-app-delegate
;;
;; This class inherits from gui::lisp-application-delegate. You would
;; want to use some variation of this when you want your application to
;; be able to run under the IDE or with all IDE capabilities in a stand-alone
;; application, but need to add additional global capabilities that can't be 
;; obtained by simply adding new functions to the gui::lisp-application-delegate
;; class directly. That is, you need to shadow existing capabilites (say by 
;; redefining an Objective-C method or adding to some existing function)

(defclass lisp-ide-app-delegate (gui::ide-application-delegate)
    ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/applicationWillFinishLaunching: :void)
    ((self lisp-IDE-app-delegate) notification)
  (call-next-method notification)
  (application-will-finish-launching self))

(defmethod application-will-finish-launching ((self lisp-IDE-app-delegate))
  ;; Do things here that you want done when your app is
  ;; just starting up. If your app bundle is loaded under
  ;; the IDE at runtime, then this function will be called directly
  ;; by the lisp-doc-controller instance that loads the bundle. If
  ;; the app is part of a stand-alone application, then the 
  ;; #/applicationWillFinishLaunching: method above will invoke this.
)

(defmethod application-will-finish-launching ((self ns:ns-object))
  ;; For any other delegate type (i.e. one that doesn't define
  ;; this method for itself) we want to do nothing.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple-lisp-app-delegate (ns::ns-object)

(defclass simple-lisp-app-delegate (ns:ns-object)
    ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/initialize :void) 
                ((self +simple-lisp-app-delegate))
  ;; in this method do things that only need to be done once, before any
  ;; instance of this class is actually created. These will be done
  ;; both when a stand-alone app is run and when the app is loaded under
  ;; the IDE. But note that reloading under the IDE will NOT re-execute
  ;; this method.
  )

(objc:defmethod (#/applicationWillFinishLaunching: :void)
    ((self simple-lisp-app-delegate) notification)
  (declare (ignore notification))
  ;; In this method do things that need to be done before the event loop
  ;; of the app is started. These things are also done if the app is
  ;; loaded under the IDE. Here we explicitly call application-will-finish-launching.
 (application-will-finish-launching self))

(defmethod application-will-finish-launching ((self simple-lisp-app-delegate))
  ;; Do things here that you want done when your app is
  ;; running under the ide. If your app bundle is loaded under
  ;; the IDE at runtime, then this function will be called directly
  ;; by the lisp-doc-controller instance that loads the bundle. If
  ;; the app is part of a stand-alone application, then the 
  ;; #/applicationWillFinishLaunching: method above will invoke this.
  
)

;; Define objective-C methods that are called from menu items or from the
;; application object that are global (i.e. not specific to some window
;; or document). For example:

(objc:defmethod (#/applicationShouldOpenUntitledFile: #>BOOL)
                ((self simple-lisp-app-delegate) sender)
  (declare (ignore sender))
  #$NO)
  

  
