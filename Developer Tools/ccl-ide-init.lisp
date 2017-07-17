;; ccl-ide-init.lisp

;; Create a symbolic link to this file in your home directory
;; in the terminal app type: ln -s <path toCocoaInterface> /Users/<user>/CocoaInterface

;; Note that creating an OS X alias will not work for reasons I don't fully understand

;; Either put the CocoaInterface directory that you got from github into your home directory or 
;; create a symbolic link to it there or modify the code below to point to wherever you put it.
;; Here we assume that there is a CocoaInterface directory or symbolic link to it with the same name
;; within your home directory
;; Puts pathname translations for my user interface code into search path so that we can just require it
(setf (logical-pathname-translations "cocoa-pk")
      '(("**;*.*" "home:CocoaInterface;**;*.*")))
(push (pathname "cocoa-pk:**;") *module-search-path*)

;; Load quicklisp
;; Something useful that I do that you might want
;; You need the quicklisp directory (or an alias to it) in your home directory
#|
(require :setup "home:quicklisp;setup.lisp")
|#

;; Load and install the Developer tools
(require :install-app-tools)

(import 'iu::coerce-obj :common-lisp-user) ;; useful to have in the listener