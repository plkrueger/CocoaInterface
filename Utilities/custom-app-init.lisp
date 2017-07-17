;; custom-app-init.lisp 

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

;; Override the default init-cocoa-ide so that it no longer requires that the bundle have a 
;; mainnibfile.

(in-package :gui)

(defun init-cocoa-ide ()
  ;; adapted from CCL's init-cocoa-ide
  ;; Either loads a main nib file or calls a specified main function (or both)
  (with-autorelease-pool
      (#/standardUserDefaults ns:ns-user-defaults)
      (let* ((bundle (open-main-bundle))
	     (dict (#/infoDictionary  bundle))
	     (classname (#/objectForKey: dict #@"NSPrincipalClass"))
	     (mainnibname (#/objectForKey: dict  #@"NSMainNibFile"))
             (mainfuncname (#/objectForKey: dict  #@"CLMainFunc"))
             (appdelegate  (#/objectForKey: dict  #@"CCLDelegateClass"))
	     (progname (#/objectForKey: dict #@"CFBundleName")))
	(if (%null-ptr-p classname)
	  (error "problems loading bundle: can't determine application class name"))
        (if (and (%null-ptr-p mainnibname) (%null-ptr-p mainfuncname))
           (error "problems loading bundle: can't determine either nib name or main func name for application"))
	(unless (%null-ptr-p progname)
          (#/setProcessName: (#/processInfo ns:ns-process-info) progname))
	(let* ((appclass (#_NSClassFromString classname))
	       (app (#/sharedApplication appclass)))
          (unless (%null-ptr-p mainnibname)
            (#/loadNibNamed:owner: ns:ns-bundle mainnibname  app))
          (unless (%null-ptr-p mainfuncname)
            (funcall (read-from-string (iu::coerce-obj mainfuncname 'string)) app))
          (unless (or (%null-ptr-p appdelegate) (not (%null-ptr-p (#/delegate app))))
            (#/setDelegate: app (#/init (#/alloc (#_NSClassFromString appdelegate)))))
	  app))))