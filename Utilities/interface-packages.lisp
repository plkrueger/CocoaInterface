;; interface-packages.lisp

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

Defines all interface packages so this can be loaded once before everything
else and keep names straight. This also makes the code more portable to other
Lisp implementations that do not do package redefinitions when the same
package is defined in multiple files.

|#

;; so we can use symbols for export/import without having them interned in :common-lisp-user
(defpackage :package-def-package)
(in-package :package-def-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :interface-utilities

(defpackage :interface-utilities
  (:nicknames :iu)
  (:use :ccl :common-lisp)
  (:import-from :gui 
                cgfloat)
  (:export

   ;; alert.lisp
   alert
   with-errors-alerted

   ;; app-kit-version.lisp
   app-kit-version
   satisfies-app-kit-version

   ;; assoc-array.lisp
   assoc-array
   assoc-aref
   mapcar-assoc-array
   mapcar-hash-keys

   ;; attributed-strings.lisp
   add-attribute
   add-attributes
   all-font-names
   append-string
   apply-font-traits
   att-ns-str
   attribute-at-index
   attributed-string-equal
   attributed-substring
   attributes-at-index
   format-attributes
   insert-attributed-string-at-index
   lisp-str
   print-all-font-names
   remove-attribute
   replace-chars-in-range
   set-attributed-string
   set-attributes
   subscript-range
   superscript-range
   unscript-range
   url-at-index

   ;; binding-utils.lisp
   bind
   bound-slot-modified
   bound-slot-will-be-modified
   bound-to
   link-path-components
   linking-views
   valid-binding-p
   valid-bindings
   when-observed

   ;; class-convert.lisp
   class-actions
   class-outlets
   dot-h-to-lisp
   lisp-to-dot-h
   save-dot-h
   save-lisp-from-dot-h
   scan-file-for-classes
   scan-file-for-module

   ;; coder.lisp
   decode-lisp-object-for-key
   encode-lisp-object-for-key

   ;; coerce-obj.lisp
   assoc-list
   coerce-obj
   lisp-date
   on-error-return
   random-color

   ;; combo-box-source.lisp
   combo-box-source-items

   ;; date.lisp
   +days
   abbrev-day-of-wk
   add-day
   date-list
   date-string
   day-char
   day-of-wk
   day-set
   day-set-bit
   days-between
   days-from
   days-to-sec
   do-dates
   do-interval-dates
   do-months
   dt
   dt-diff
   dt-yr
   has-day-p
   in-dayset-p
   inc-date
   inc-days
   inc-months
   inc-years
   intl-string-dt
   last-dt-of-month
   lisp-to-ns-date
   mmdd-string
   mmddyy-list
   mmyy-string
   months-between
   next-day
   next-month
   next-year
   now
   ns-to-lisp-date
   num-days-in-dayset
   prev-day
   prev-month
   prev-year
   random-date
   remove-day
   same-day-p
   short-date-string
   short-time-string
   string-to-date
   string-to-ns-date
   time-string
   today-date
   years-between
   yr-string

   ;; decimal.lisp
   lisp-from-ns-decimal
   lisp-to-ns-decimal
   float-from-ns-decimal

   ;; dev-tools.lisp
   add-key-info
   added-class-key-values
   added-class-keywords
   init-keys
   key-doc-string
   key-name
   key-values
   recursive-find-if
   reload-documentation

   ;; doc-controller-hash.lisp
   doc-controller-for-class
   ensure-class-name
   set-doc-controller-for-class

   ;; file-directory-utils.lisp
   find-in-ccl
   find-in-contrib
   find-in-krueger-contrib

   ;; file-monitor.lisp
   monitor-file
   resume-monitoring
   suspend-monitoring
   unmonitor-file

   ;; hist-dt.lisp
           ;;; Encoding hist-dates
   hist-date
   hist-day
   intl-string-to-date
           ;;; Decoding hist-dates
   day-of-week
   dt-year
   hist-date-day
   hist-date-hr-min-sec
   hist-date-time-secs
   hist-date-yr-month-day
   hist-day-yr-month-day
   make-date-list
   mmddyr-list
   short-day-of-wk
           ;;; Converting hist-dates to/from common lisp dates
   hist-date-to-lisp-date
   lisp-date-to-hist-date
           ;;; Constructing commonly used hist-dates
   end-of-month
   time-now
   todays-date
           ;;; Computing with hist-dates
   -dates
   -days
   -months
   -years
   day-span
   date+
   date-
   day=
   days+
   days-
   weeks+
   weeks-
   months+
   months-
   quarters+
   quarters-
   years+
   years-
           ;;; Iterators for hist-dates
   do-for-dates
   do-for-interval-dates
   do-for-months
           ;;; Formatting hist-dates as strings
   string-date
   string-date-short
   string-date-time
   string-date-time-short
   string-day-of-wk
   string-day-of-wk-short
   string-intl-date
   string-mmdd
   string-mmyy
   string-quarter
   string-time
   string-yr

   ;; install-executable
   with-errors-reported

   ;; interactive-app.lisp
   ccl-result
   eval-in-subordinate-ccl
   in-subordinate-ccl
   make-ccl-stream
   pop-error
   print-from-app
   process-close
   remote-let
   shell-command
   start-trace
   stop-trace
   trace-output
   with-ccl-stream
   write-line-to-app

   ;; iu-classes.lisp
   app-stream
   attributed-string
   combo-box-source
   kvo-observer
   lisp-bundle
   lisp-doc-controller
   lisp-document
   lisp-menu-target
   lisp-ptr-wrapper
   lpw-depth
   lpw-lisp-ptr
   lpw-parent
   nib-link
   ns-func
   ns-misc
   ns-sym
   ts-qnode
   ts-queue

   ;; lisp-bundle.lisp
   bundle-for-class
   bundle-loaded
   bundle-logical-host
   lisp-bundle-with-path
   load-bundle

   ;; lisp-doc-controller.lisp
   close-document
   close-open-documents
   doc-controller-for-doc-type
   doc-controller-for-menu-item
   docs-of-type
   make-doc-controller
   open-documents

   ;; lisp-document.lisp
   document-did-open
   document-is-nib-file-owner
   document-window-controller-classes
   font-name
   font-size
   num-graphic-pages
   print-graphic
   print-lines
   print-view-class
   window-build-funcs
   window-nib-names

   ;; lisp-notification.lisp
   add-observer
   notify-ht
   post-notification
   remove-observer

   ;; list-utils.lisp
   add-to-list-at
   columnize
   delete-from-list
   every-n-sublists
   find-cdr
   sequential-sublists
   sort-list-in-place

   ;; menu-utils.lisp
   *app-name-for-menus*
   add-to-main-menu
   app-menu
   clear-key
   clear-saved-menus
   delete-menu
   find-menu-item-with-name
   install-menuitems-after
   key-for-menuitem
   make-and-install-menu
   make-and-install-menuitems-after
   make-menu
   make-popup-button
   make-simple-menu
   menu-item-for-key
   merge-simple-menu-items
   pop-up-select
   remove-all-but
   remove-from-main-menu
   remove-menuitems
   restore-starting-menu
   save-main-menu
   save-mi-list-with-key
   set-app-menu
   set-sub-menu
   set-windows-menu
   standard-app-menu
   standard-edit-menu
   standard-file-menu
   standard-format-menu
   standard-help-menu
   standard-main-menu
   standard-view-menu
   standard-window-menu
   starting-menu
   uninstall-menu

   ;; nib.lisp
   compile-xib
   load-nibfile

   ;; nib-link.lisp
   *nib-links*
   find-link-to
   find-nib
   find-nib-in-ccl
   find-nib-in-contrib
   find-nib-in-krueger-contrib
   is-link-to
   unlink

   ;; notification.lisp
   notification-handler
   received-notification

   ;; ns-object-utils.lisp
   archive-slots
   class-conforms-to-protocol
   class-name-string
   do-objc-array
   equal-size-p
   find-ns-classes
   lisp-to-ns-array
   lisp-to-ns-dict
   lisp-to-ns-func
   lisp-to-ns-object
   lisp-to-ns-misc
   lisp-to-ns-plist-dict
   lisp-to-ns-point
   lisp-to-ns-rect
   lisp-to-ns-sym
   ns-rect-to-list
   ns-size-to-list
   ns-to-lisp-array
   ns-to-lisp-assoc
   ns-to-lisp-classname
   ns-to-lisp-hash-table
   ns-to-lisp-list
   ns-to-lisp-object
   obj-if-not-null
   objc-displayable
   objc-to-std-instance
   on-main-thread
   print-ns-object
   recursive-map
   std-instance-to-objc
   while-converting
   while-unconverting

   ;; ns-string-utils.lisp
   find-func
   find-substring
   internable-string-p
   lisp-object-to-ns-data
   lisp-str-to-ns-data
   lisp-to-temp-nsstring
   non-empty-string
   ns-data-to-lisp-object
   ns-data-to-lisp-str
   ns-to-lisp-string
   nsstring-to-class
   nsstring-to-func
   nsstring-to-sym
   read-from-nsstring

   ;; nslog-utils.lisp
   interleave
   log-4floats
   log-float
   log-rect
   log-size
   ns-error
   ns-log
   ns-log-format

   ;; objc-initialize.lisp

   ;;open-panel.lisp
   open-panel

   ;; or-semaphore.lisp
   or-semaphore
   reinitialize-or-semaphore
   signal-or-semaphore
   stop-or-semaphore
   timed-wait-on-or-semaphore
   wait-on-or-semaphore

   ;; path-trans.lisp
   *log-bindings*
   did-change-value-for-key
   lisp-to-objc-keypathname
   log-bindings
   make-ptr-wrapper
   objc-to-lisp-keypathname
   will-change-value-for-key
   wrapper-for

   ;; preferences.lisp
   set-user-default
   user-default

   ;; quick-window.lisp
   make-quick-window

   ;; save-panel.lisp
   save-panel

   ;; selector-utils.lisp
   find-selector-match
   get-selector
   responds-to-selector

   ;; thread-safe-queue.lisp
   empty-queue-p
   pop-queue
   push-queue

   ;; undo.lisp
   set-undo
   undo-target
   update-with-undo

   ;; utility.lisp
   delete-from-seq
   do-sequence

   ;; window-controller.lisp
   close-window
   lisp-window-controller
   load-window
   show-window
   update-window

   ;; window-utils.lisp
   show-window-in-controller
   window-frame-size
   window-titled
   with-stream-window

   ;; words.lisp
   replace-substr
   words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :lisp-controller

(defpackage :lisp-controller
  (:nicknames :lc)
  (:use :ccl :common-lisp :iu)
  (:import-from :gui 
                cgfloat)
  (:export

   ;; lisp-controller.lisp
   added-func
   add-child-func
   can-add-child
   can-insert
   can-remove
   children-func
   content-class
   count-func
   current-selection
   delete-func
   edited-func
   func-owner
   gen-root
   ht-key
   ht-value
   lisp-controller 
   lisp-controller-changed
   modified-bound-value
   objects
   reader-func
   removed-func
   root
   root-type
   select-func
   selection
   unbind
   undo-doc
   undo-name
   view
   writer-func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :lisp-views

(defpackage :lisp-views
  (:nicknames :lv)
  (:use :ccl :common-lisp :lc :iu)
  (:import-from :gui 
                cgfloat)
  (:export

   ;; button.lisp
   button-box-view
   button-controller
   lisp-button

   ;; constraint-layout.lisp
   *border-space*
   *default-space*
   *no-space*
   align-views
   analyze-constraints
   anchor
   anchor-window
   attribute-convert
   center-in-view
   center-relative-to
   compression-resistance-priority
   constrain
   constrain-size
   constrain-size-relative-to
   constrain-to-array
   constrain-to-natural-height
   constrain-to-natural-size
   constrain-to-natural-width
   constraints
   constraints-affecting-layout
   distribute-relative-to
   distribute-views
   expansion-resistance-priority
   flip-rel
   log-constraints
   make-constraint
   make-constraints-for
   make-invisible-spacer
   order-views
   orientation-convert
   prioritize-expansion-resistance
   prioritize-compression-resistance
   priority-convert
   relation-convert
   remove-constraints
   visualize-constraints

   ;; lisp-app-pr-view.lisp
   page-num

   ;; lv-classes.list
   label-view
   labeled-text-field
   lisp-app-doc-print-view
   text-field
   vscrolled-text-view

   ;; organized-box-view.lisp
   add-box-subviews
   organized-box-view
   resizable-box-view

   ;; radio-button.lisp
   radio-button-array-view
   radio-button-box-view
   radio-button-controller

   ;; scroll-view.lisp
   make-scroll-view

   ;; table-utils.lisp
   table-controller
   table-scroll-view
   table-width

   ;; text-views.lisp
   rotated-text-view
   form-view
   label-view
   make-text-field
   make-text-view
   make-vscrolled-text-view
   text-field-of

   ;; view-utils.lisp
   add-subviews
   common-superview
   joint-superview
   natural-size
   natural-view-height
   natural-view-width
   superview
   view-height
   view-p
   view-tree
   view-width
   zero-size-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :app-dev

(defpackage :app-dev
  (:nicknames :ad)
  (:use :iu :lc :ccl :lv :common-lisp)
  (:export

   ;; lisp-app-delegate.lisp
   application-will-finish-launching
   lisp-IDE-app-delegate
   simple-lisp-app-delegate
   
   ;; lisp-app-doc.lisp
   lisp-app-doc

   ;; lisp-app-win-controller
   lisp-app-win-controller))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :ogl

(defpackage :ogl
  (:use :iu :ccl :common-lisp)
  (:export

   ;; lisp-open-gl-view.lisp
   data-source
   draw-in-view-rect
   is-flipped
   lisp-open-gl-view

   ;; opengl-utils.lisp
   do-foreign-array
   ogl-vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :dev

(defpackage :dev
  (:use :iu :ccl :lv :lc :common-lisp)
  (:export
   
   ;; dev-tools-interface.lisp
   *dev-tools-window-controller*
   reload-doc-window))

(provide :interface-packages)

