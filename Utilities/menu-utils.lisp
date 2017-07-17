;; menu-uitls.lisp

#|
The MIT license.

Copyright (c) 2010-2013 Paul L. Krueger

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
  (require :iu-classes)
  (require :selector-utils)
  (require :ns-object-utils)
  (require :nib))

(in-package :iu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp-menu-target
;; A class defining objects that can be targets of menuitem actions and which
;; then invoke some lisp function

#|
(defclass lisp-menu-target (ns:ns-object)
  ((lisp-func :accessor lisp-func :initarg :lisp-func))
  (:metaclass ns:+ns-object))
|#

(objc:defmethod (#/doIt: :void)
                ((self lisp-menu-target) (sender :id))
  (funcall (lisp-func self) sender))

;; Utility functions that allow lisp programs to add menus to the existing menubar

(defun find-menu-item-with-name (menu-item-name &optional (menu nil))
  (let* ((men (or menu (#/mainMenu #&NSApp)))
         (ns-mi-name (ccl::%make-nsstring menu-item-name))
         (item (#/itemWithTitle: men ns-mi-name)))
    (or (obj-if-not-null item)
        (some #'(lambda (mi)
                  (let ((sub-menu (obj-if-not-null (#/submenu mi))))
                    (and sub-menu (find-menu-item-with-name menu-item-name sub-menu))))
              (ns-to-lisp-list (#/itemArray men))))))

;; For the following function menu-item-specs should be a list where each
;; element of the list is itself a list of the form:
;;  (menu-item-name menu-item-action menu-item-key-equivalent menu-item-target)
;; where each of these except the menu-item-target must be acceptable as an 
;; argument to the function "string". Menu-item-target must be an Ojbective-C object,
;; but can be nil to signify no target (the default)

;; Used "menumadness" sample code from Apple Developer website to know what to do ...
;;    NSMenu *newMenu;
;;    NSMenuItem *newItem;
;;
;;    // Add the submenu
;;    newItem = [[NSMenuItem allocWithZone:[NSMenu menuZone]] initWithTitle:@"Flashy" action:NULL keyEquivalent:@""];
;;    newMenu = [[NSMenu allocWithZone:[NSMenu menuZone]] initWithTitle:@"Flashy"];
;;    [newItem setSubmenu:newMenu];
;;    [newMenu release];
;;    [[NSApp mainMenu] addItem:newItem];
;;    [newItem release];
;;  Basically you need to create both a menuitem and a menu for any menu displayed in the menubar
;;  and link them together. Then add the menuitem to the main menu (i.e. menubar)

(defun make-and-install-menu (menu-name &rest menu-item-specs)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (menuitem (#/initWithTitle:action:keyEquivalent: 
                    (#/allocWithZone: ns:ns-menu-item 
                                      (#/menuZone ns:ns-menu))
                    ns-menu-name
                    (%null-ptr)
                    #@""))
         (menu (#/initWithTitle: (#/allocWithZone: 
                                  ns:ns-menu (#/menuZone ns:ns-menu))
                                 ns-menu-name))
         (main-menu (#/mainMenu #&NSApp)))
    (dolist (mi menu-item-specs)
      (destructuring-bind (mi-title mi-selector &optional (mi-key "") mi-target) mi
        (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
               (action-selector (get-selector (string mi-selector)))
               (ns-key (ccl::%make-nsstring (string mi-key)))
               (men-item (#/addItemWithTitle:action:keyEquivalent: menu 
                                                                   ns-title 
                                                                   action-selector
                                                                   ns-key)))
          (when mi-target
            (#/setTarget: men-item mi-target))
          (#/release ns-title)
          (#/release ns-key))))
    ;; Link up the new menuitem and new menu
    (#/setSubmenu: menuitem menu)
    (#/release menu)
    ;; Now tell the main menu to make this a sub-menu
    (#/addItem: main-menu menuitem)
    (#/release ns-menu-name)
    (#/release menuitem)
    menu))

(defun uninstall-menu (menu-name)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menu-item (#/itemWithTitle: main-menu ns-menu-name)))
    (when (not (eql menu-item (%null-ptr)))
      (#/removeItem: main-menu menu-item))))

(defun install-menuitems-after (menu-name menu-item-name &rest menu-items)
  ;; installs a set of menu-items after a menu-item (specified by menu-name) 
  ;; in a specified top-level menu (specified by menu-name).
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menuitem (or (#/itemWithTitle: main-menu ns-menu-name) 
                       (error "~s is not a valid menu title" menu-name)))
         (sub-menu (#/submenu menuitem))
         (ns-menu-item-name (coerce-obj menu-item-name 'ns:ns-string))
         (insert-index (#/indexOfItemWithTitle: sub-menu ns-menu-item-name)))
    (dolist (mi menu-items)
      (#/insertItem:atIndex: sub-menu mi (incf insert-index)))))

;; The following function inserts one or more new menu-items immediately 
;; following a specified menu-item.
;; If the menu-item-name is "" the insertion will be after the first divider
;; (which has a blank name). If the menu-item-name does not exist in the menu,
;;  the items will be placed at the beginning of the menu.
;; menu-item-specs are as defined above for the make-and-install-menu function.
;; menu-name and menu-item-name arguments must be acceptable as argument to the 
;; "string" function.
(defun make-and-install-menuitems-after (menu-name menu-item-name &rest menu-item-specs)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menuitem (or (#/itemWithTitle: main-menu ns-menu-name) 
                       (error "~s is not a valid menu title" menu-name)))
         (sub-menu (#/submenu menuitem))
         (ns-menu-item-name (ccl::%make-nsstring menu-item-name))
         (insert-index (#/indexOfItemWithTitle: sub-menu ns-menu-item-name))
         (installed-items nil))
    (dolist (mi menu-item-specs)
      (destructuring-bind (mi-title mi-selector &optional (mi-key "") mi-target) mi
        (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
               (action-selector (get-selector (string mi-selector)))
               (ns-key (ccl::%make-nsstring (string mi-key)))
               (men-item (#/insertItemWithTitle:action:keyEquivalent:atIndex: 
                          sub-menu 
                          ns-title 
                          action-selector
                          ns-key
                          (incf insert-index))))
          (push men-item installed-items)
          (when mi-target
            (#/setTarget: men-item mi-target))
          (#/release ns-title)
          (#/release ns-key))))
    (#/release ns-menu-item-name)
    (#/release ns-menu-name)
    ;; returned value is suitable as argument for remove-menuitems
    (cons sub-menu installed-items)))

(defun remove-menuitems (menu-and-items-list)
  (let ((menu (first menu-and-items-list)))
    (dolist (mi (cdr menu-and-items-list))
      (#/removeItem: menu mi))))

(defun mask-to-key-mod-str (mask)
  (format nil "~:[cmd-~;~]~:[ctrl-~;~]~:[option-~;~]~:[shift-~;~]"
          (zerop (logand #$NSCommandKeyMask mask))
          (zerop (logand #$NSControlKeyMask mask))
          (zerop (logand #$NSAlternateKeyMask mask))
          (zerop (logand #$NSShiftKeyMask mask))))

(defun describe-menu (&optional (menu (#/mainMenu #&NSApp)) (indent 0))
  (let ((in-str (make-string indent :initial-element #\space))
        (items (coerce-obj (#/itemArray menu) 'list)))
    (format t "~%~a~a"
            in-str
            (coerce-obj (#/title menu) 'string))
    (dolist (it items)
      (let ((submenu (#/submenu it)))
        (when (#/isAlternate it)
          (format t "~%~a  **next is alternate to previous**" in-str))
        (cond ((#/isSeparatorItem it)
               (format t "~%~a  Separator" in-str))
              ((not (eql submenu (%null-ptr)))
               (describe-menu submenu (+ indent 2)))
              (t
               (format t "~%~a  Title: ~s Action: ~s Target: ~s" 
                       in-str
                       (coerce-obj (#/title it) 'string)
                       (coerce-obj (#_NSStringFromSelector (#/action it)) 'string)
                       (#/target it))
               (let ((ke (coerce-obj (#/keyEquivalent it) 'string)))
                 (unless (string= ke "")
                   (format t " KeyEquiv: ~a~a"
                           (mask-to-key-mod-str (#/keyEquivalentModifierMask it))
                           ke)))))))))

;; The following functions let you create your own menus, using standard menu items or whole menus if
;; that is appropriate using keyword identifiers and easily adding your own items or menus as desired.
;; The core is the make-menu macro which creates a menu from a tile and a list of menu items. If you want
;; to use a standard menu-item as defined in menu-item-for-key, just use the associated keyword. If you
;; want to add a custom menu-item of your own in the middle somewhere, just put any lisp form that
;; creates that menu-item in the appropriate position. You can use make-menu to create your own
;; sub-menus for any custom menu-item as well.

(defun lookup-or-eval-item (item-spec)
  (if (keywordp item-spec)
    (menu-item-for-key item-spec)
    item-spec))

(defmacro make-menu (title &rest menu-item-specs)
  `(make-instance ns:ns-menu
     :title ,title
     :menu-items (mapcar #'lookup-or-eval-item (list ,@menu-item-specs))))

;; This function provides access to commonly used menu-items. It provides examples of the way in which
;; you can create your own custom menu-items.

(defvar *app-name-for-menus* "Application")

(defun menu-item-for-key (menu-item-key)
  (ecase menu-item-key
    ;; main menu item keys
    (:app (#/autorelease (make-instance ns:ns-menu-item
                           :title *app-name-for-menus*
                           :submenu (#/autorelease (standard-app-menu)))))
    (:file (#/autorelease (make-instance ns:ns-menu-item
                            :title "File"
                            :submenu (#/autorelease (standard-file-menu)))))
    (:edit (#/autorelease (make-instance ns:ns-menu-item
                            :title "Edit"
                            :submenu (#/autorelease (standard-edit-menu)))))
    (:window (#/autorelease (make-instance ns:ns-menu-item
                              :title "Window"
                              :submenu (#/autorelease (standard-window-menu)))))
    (:help (#/autorelease (make-instance ns:ns-menu-item
                            :title "Help"
                            :submenu (#/autorelease (standard-help-menu)))))
    ;; Generic separator item
    (:sep (#/separatorItem ns:ns-menu-item))
    ;; app sub-menu item keys
    (:about (#/autorelease (make-instance ns:ns-menu-item
                             :title (concatenate 'string "About " *app-name-for-menus*)
                             :action "orderFrontStandardAboutPanel:"
                             :target #&NSApp)))
    (:pref (#/autorelease (make-instance ns:ns-menu-item
                            :title "Preferences…"
                            :action "showPreferences:"
                            :target (%null-ptr)
                            :key-equivalent ";" ;; non-standard beause CCL uses cmd-, as interrupt
                            :key-equivalent-modifier-mask :command)))
    (:services (#/autorelease (make-instance ns:ns-menu-item
                                :title "Services"
                                :target (%null-ptr)
                                :submenu (#/autorelease (make-instance ns:ns-menu
                                                          :title "Services")))))
    (:hide-app (#/autorelease (make-instance ns:ns-menu-item
                                :title (concatenate 'string "Hide" *app-name-for-menus*)
                                :action "hide:"
                                :target #&NSApp
                                :key-equivalent "H"
                                :key-equivalent-modifier-mask :command)))
    (:hide-others  (#/autorelease (make-instance ns:ns-menu-item
                                    :title "Hide Others"
                                    :action "hideOtherApplications:"
                                    :target #&NSApp
                                    :key-equivalent "H"
                                    :key-equivalent-modifier-mask (list :command :option))))
    (:show-all (#/autorelease (make-instance ns:ns-menu-item
                                :title "Show All"
                                :action "unhideAllApplications:"
                                :target #&NSApp)))
    (:quit (#/autorelease (make-instance ns:ns-menu-item
                            :title "Quit"
                            :action "terminate:"
                            :key-equivalent "q"
                            :key-equivalent-modifier-mask :command
                            :target #&NSApp)))
    ;; file sub-menu keys
    (:new (#/autorelease (make-instance ns:ns-menu-item
                           :title "New"
                           :action "newDocument:"
                           :target (%null-ptr)
                           :key-equivalent "n"
                           :key-equivalent-modifier-mask :command)))
    (:open (#/autorelease (make-instance ns:ns-menu-item
                            :title "Open…"
                            :action "openDocument:"
                            :target (%null-ptr)
                            :key-equivalent "o"
                            :key-equivalent-modifier-mask :command)))
    (:clear-recent (#/autorelease (make-instance ns:ns-menu-item
                                    :title "Clear Menu"
                                    :action "clearRecentDocuments:"
                                    :target (%null-ptr))))
    (:open-recent (#/autorelease (make-instance ns:ns-menu-item
                                   :title "Open Recent"
                                   :submenu (make-menu "Open Recent" :sep :clear-recent))))
    (:close (#/autorelease (make-instance ns:ns-menu-item
                             :title "Close"
                             :action "performClose:"
                             :target (%null-ptr)
                             :key-equivalent "w"
                             :key-equivalent-modifier-mask :command)))
    (:close-all (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Close All"
                                 :alternate t
                                 :action "closeAll:"
                                 :target (%null-ptr)
                                 :key-equivalent "w"
                                 :key-equivalent-modifier-mask (list :command :option))))
    (:save (#/autorelease (make-instance ns:ns-menu-item
                            :title "Save"
                            :action "saveDocument:"
                            :target (%null-ptr)
                            :key-equivalent "s"
                            :key-equivalent-modifier-mask :command)))
    (:duplicate (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Duplicate"
                                 :action "duplicateDocument:"
                                 :target (%null-ptr))))
    (:save-as (#/autorelease (make-instance ns:ns-menu-item
                               :title "Save As…"
                               :action "saveDocumentAs:"
                               :target (%null-ptr)
                               :key-equivalent "S"
                               :key-equivalent-modifier-mask :command)))
    (:export-as (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Export As…"
                                 :action "exportDocument:"
                                 :target (%null-ptr))))
    (:save-all (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Save All"
                                 :action "saveAllDocuments:"
                                :target (%null-ptr))))
    (:revert-to-saved (#/autorelease (make-instance ns:ns-menu-item
                                       :title "Revert to Saved"
                                       :action "revertDocumentToSaved:"
                                       :target (%null-ptr))))
    (:print (#/autorelease (make-instance ns:ns-menu-item
                             :title "Print…"
                             :action "printDocument:"
                             :target (%null-ptr)
                             :key-equivalent "p"
                             :key-equivalent-modifier-mask :command)))
    (:page-setup (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Page Setup…"
                                  :action "runPageLayout:"
                                  :target (%null-ptr)
                                  :key-equivalent "P"
                                  :key-equivalent-modifier-mask :command)))
    ;; edit sub-menu keys
    (:undo (#/autorelease (make-instance ns:ns-menu-item
                            :title "Undo"
                            :action "undo:"
                            :target (%null-ptr)
                            :key-equivalent "z"
                            :key-equivalent-modifier-mask :command)))
    (:redo (#/autorelease (make-instance ns:ns-menu-item
                            :title "Redo"
                            :action "redo:"
                            :target (%null-ptr)
                            :key-equivalent "Z"
                            :key-equivalent-modifier-mask :command)))
    (:cut (#/autorelease (make-instance ns:ns-menu-item
                           :title "Cut"
                           :action "cut:"
                           :target (%null-ptr)
                           :key-equivalent "x"
                           :key-equivalent-modifier-mask :command)))
    (:copy (#/autorelease (make-instance ns:ns-menu-item
                            :title "Copy"
                            :action "copy:"
                            :target (%null-ptr)
                            :key-equivalent "c"
                            :key-equivalent-modifier-mask :command)))
    (:paste (#/autorelease (make-instance ns:ns-menu-item
                             :title "Paste"
                             :action "paste:"
                             :target (%null-ptr)
                             :key-equivalent "v"
                             :key-equivalent-modifier-mask :command)))
    (:paste-and-match-style (#/autorelease (make-instance ns:ns-menu-item
                                             :title "Paste and Match Style"
                                             :action "pasteAndMatchStyle:"
                                             :target (%null-ptr)
                                             :key-equivalent "v"
                                             :key-equivalent-modifier-mask (list :command :option))))
    (:delete (#/autorelease (make-instance ns:ns-menu-item
                              :title "Delete"
                              :action "delete:"
                              :target (%null-ptr))))
    (:clear (#/autorelease (make-instance ns:ns-menu-item
                             :title "Clear"
                             :action "clear:"
                             :target (%null-ptr))))
    (:select-all (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Select All"
                                  :action "selectAll:"
                                  :target (%null-ptr)
                                  :key-equivalent "a"
                                  :key-equivalent-modifier-mask :command)))
    (:find-group (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Find"
                                  :submenu (make-menu "Find" :find :find-next :find-previous))))
         ;; note that all the find command variations defined here use the new 10.7 performTextFinderAction: 
         ;; rather than the old performFindPanelAction:. The CCL IDE uses the old form in its menus.
    (:find (#/autorelease (make-instance ns:ns-menu-item
                            :title "Find…"
                            :action "performTextFinderAction:"
                            :target (%null-ptr)
                            :tag #$NSTextFinderActionShowFindInterface
                            :key-equivalent "f"
                            :key-equivalent-modifier-mask :command)))
    (:find-next (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Find Next"
                                 :action "performTextFinderAction:"
                                 :target (%null-ptr)
                                 :tag #$NSTextFinderActionNextMatch
                                 :key-equivalent "g"
                                 :key-equivalent-modifier-mask :command)))
    (:find-previous (#/autorelease (make-instance ns:ns-menu-item
                                     :title "Find Previous"
                                     :action "performTextFinderAction:"
                                     :target (%null-ptr)
                                     :tag #$NSTextFinderActionPreviousMatch
                                     :key-equivalent "G"
                                     :key-equivalent-modifier-mask :command)))
    (:spelling (#/autorelease (make-instance ns:ns-menu-item
                                :title "Spelling"
                                :action "checkSpelling:"
                                :target (%null-ptr))))
    ;; format sub-menu keys
    (:show-fonts (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Show Fonts"
                                  :action "showFontPanel:" ;; made up, must be defined by user
                                  :target (%null-ptr)
                                  :key-equivalent "t"
                                  :key-equivalent-modifier-mask :command)))
    (:show-colors (#/autorelease (make-instance ns:ns-menu-item
                                   :title "Show Colors"
                                   :action "showColorPanel:" ;; made up, must be defined by user
                                   :target (%null-ptr)
                                   :key-equivalent "C"
                                   :key-equivalent-modifier-mask :command)))
    (:bold (#/autorelease (make-instance ns:ns-menu-item
                            :title "Bold"
                            :action "boldText:" ;; made up, must be defined by user
                            :target (%null-ptr)
                            :key-equivalent "b"
                            :key-equivalent-modifier-mask :command)))
    (:italic (#/autorelease (make-instance ns:ns-menu-item
                              :title "Italic"
                              :action "italicText:" ;; made up, must be defined by user
                              :target (%null-ptr)
                              :key-equivalent "i"
                              :key-equivalent-modifier-mask :command)))
    (:underline (#/autorelease (make-instance ns:ns-menu-item
                                 :title "Underline"
                                 :action "underlineText:" ;; made up, must be defined by user
                                 :target (%null-ptr)
                                 :key-equivalent "u"
                                 :key-equivalent-modifier-mask :command)))
    (:bigger (#/autorelease (make-instance ns:ns-menu-item
                              :title "Bigger"
                              :action "biggerText:" ;; made up, must be defined by user
                              :target (%null-ptr)
                              :key-equivalent "="
                              :key-equivalent-modifier-mask (list :command :shift))))
    (:smaller (#/autorelease (make-instance ns:ns-menu-item
                               :title "Smaller"
                               :action "smallerText:" ;; made up, must be defined by user
                               :target (%null-ptr)
                               :key-equivalent "-"
                               :key-equivalent-modifier-mask (list :command :shift))))
    (:copy-style (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Copy Style"
                                  :action "copyStyle:" ;; made up, must be defined by user
                                  :target (%null-ptr)
                                  :key-equivalent "c"
                                  :key-equivalent-modifier-mask (list :command :option))))
    (:paste-style (#/autorelease (make-instance ns:ns-menu-item
                                   :title "Paste Style"
                                   :action "pasteStyle:" ;; made up, must be defined by user
                                   :target (%null-ptr)
                                   :key-equivalent "v"
                                   :key-equivalent-modifier-mask (list :command :option))))
    (:align-left (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Align Left"
                                  :action "alignLeft:" ;; made up, must be defined by user\
                                  :target (%null-ptr)
                                  :key-equivalent "{"
                                  :key-equivalent-modifier-mask :command)))
    (:center (#/autorelease (make-instance ns:ns-menu-item
                              :title "Center"
                              :action "alignCenter:" ;; made up, must be defined by user
                              :target (%null-ptr)
                              :key-equivalent "|"
                              :key-equivalent-modifier-mask :command)))
    (:justify (#/autorelease (make-instance ns:ns-menu-item
                               :title "Justify"
                               :action "justifyText:"
                               :target (%null-ptr)))) ;; made up, must be defined by user
    (:align-right (#/autorelease (make-instance ns:ns-menu-item
                                   :title "Align Right"
                                   :action "alignRight:" ;; made up, must be defined by user
                                   :target (%null-ptr)
                                   :key-equivalent "}"
                                   :key-equivalent-modifier-mask :command)))
    (:show-ruler (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Show Ruler"
                                  :action "showRuler:"
                                  :target (%null-ptr)))) ;; made up, must be defined by user
    (:copy-ruler (#/autorelease (make-instance ns:ns-menu-item
                                  :title "Copy Ruler"
                                  :action "copyRuler:" ;; made up, must be defined by user
                                  :target (%null-ptr)
                                  :key-equivalent "c"
                                  :key-equivalent-modifier-mask (list :command :control))))
    (:paste-ruler (#/autorelease (make-instance ns:ns-menu-item
                                   :title "Paste Ruler"
                                   :action "pasteRuler:" ;; made up, must be defined by user
                                   :target (%null-ptr)
                                   :key-equivalent "v"
                                   :key-equivalent-modifier-mask (list :command :control))))
    ;; view sub-menu keys
    (:show-hide-toolbar (#/autorelease (make-instance ns:ns-menu-item
                                         :title "Show/Hide Toolbar"
                                         :action "showHideToolbar:" ;; made up, must be defined by user
                                         :target (%null-ptr)
                                         :key-equivalent "t"
                                         :key-equivalent-modifier-mask (list :command :option))))
    (:customize-toolbar (#/autorelease (make-instance ns:ns-menu-item
                                         :title "Customize Toolbar"
                                         :action "customizeToolbar:"
                                         :target (%null-ptr))))

    ;; App specific menus should be inserted between the view and window menus, if present

    ;; window sub-menu keys
    (:minimize (#/autorelease (make-instance ns:ns-menu-item
                                :title "Minimize"
                                :action "performMiniaturize:"
                                :target (%null-ptr)
                                :key-equivalent "m"
                                :key-equivalent-modifier-mask :command)))
    (:minimize-all (#/autorelease (make-instance ns:ns-menu-item
                                    :title "Minimize All"
                                    :alternate t
                                    :action "miniaturizeAll:"
                                    :target (%null-ptr)
                                    :key-equivalent "m"
                                    :key-equivalent-modifier-mask (list :command :option))))
    (:zoom (#/autorelease (make-instance ns:ns-menu-item
                            :title "Zoom"
                            :action "zoom:"
                            :target (%null-ptr)
                            :key-equivalent "m"
                            :key-equivalent-modifier-mask :command)))
    (:all-to-front (#/autorelease (make-instance ns:ns-menu-item
                                    :title "Bring All to Front"
                                    :action "arrangeInFront:"
                                    :target (%null-ptr))))
    (:arrange-in-front (#/autorelease (make-instance ns:ns-menu-item
                                        :title "Arrange in Front"
                                        :alternate t
                                        :action "arrangeInFront:"
                                        :target #$NSApp)))))

;; use the functions below as models for creating your own custom menu. These use only standard items that
;; can be included using keywords, but in place of any keyword you can substitute any list form that evals
;; to an ns:ns-menu-item object.

(defun standard-main-menu ()
  ;; Makes and returns a default main menu matching Apple guidelines as of March 2013.
  ;; The caller may modify as desired (either the code or the returned menu).
  ;; If you intend to use the services menu you must call (#/setServicesMenu: #&NSApp services-menu)
  (make-menu "" :app :file :edit :window :help))

(defun standard-app-menu ()
  ;; Makes and returns a default main menu matching Apple guidelines as of March 2013.
  ;; The caller may modify as desired (either the code or the returned menu).
  ;; If you intend to use the services menu you must call (#/setServicesMenu: #&NSApp services-menu)
  (make-menu *app-name-for-menus* :about :sep :pref :services :sep :hide-app :hide-others :show-all :sep :quit))

(defun standard-file-menu ()
  (make-menu "File" :new :open :sep :close 
             :close-all :save :duplicate :revert-to-saved :sep :print))

(defun standard-edit-menu ()
  (make-menu "Edit" :undo :redo :sep :cut :copy :paste :delete :select-all :sep :find-group :sep))

(defun standard-format-menu ()
  (make-menu "Format" :show-fonts :show-colors))

(defun standard-view-menu ()
  (make-menu "View" :show-hide-toolbar :customize-toolbar))

(defun standard-window-menu ()
  (make-menu "Window" :minimize :minimize-all :zoom :sep))

(defun standard-help-menu ()
   (make-menu "Help" ))

(defun make-simple-menu (menu-name item-list target action)
  ;; This is a bit different from other menus because here we assume that every menuitem
  ;; will have the same action and same target.
  ;; It is presumed that to show this menu you might call something like:
  ;; (#/popUpMenuPositioningItem:atLocation:inView: ...)
  ;; When the action is called it should use the index of the menuitem (e.g. its
  ;; position within the menu as returned by menuitem-position) to determine what
  ;; choice was made or equivalently retrieve the item's tag, which we set to its
  ;; relative position in the list.
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (menu (#/initWithTitle: (#/allocWithZone: 
                                  ns:ns-menu (#/menuZone ns:ns-menu))
                                 ns-menu-name))
         (tag -1))
    (dolist (mi-title item-list)
      (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
             (action-selector (get-selector (string action)))
             (men-item (#/addItemWithTitle:action:keyEquivalent: menu 
                                                                 ns-title 
                                                                 action-selector
                                                                 #@"")))
        (#/setTag: men-item (incf tag))
        (when (and target (not (eql target (%null-ptr))))
          (#/setTarget: men-item target))
        (#/release ns-title)))
    menu))

(defun set-windows-menu ()
  (let* ((win-menuitem (find-menu-item-with-name "Window"))
         (win-menu (and win-menuitem (#/submenu win-menuitem))))
    (when win-menu
      (#/setWindowsMenu: #$NSApp win-menu))))

(defun menuitem-position (menuitem menu)
  (#/indexOfItem: menu menuitem))

(defun set-sub-menu (menu-item-name item-list target action)
  ;; add a simple sub-menu for a named menu-item (i.e. where each item in the list 
  ;; invokes the same action. Presumably that action will check the title or tag of
  ;; the sender to determine which action was called. The tag will be set to the
  ;; index of the item in the item-list.
  (let ((mi (find-menu-item-with-name menu-item-name)))
    (when mi
      (#/setSubmenu: mi (make-simple-menu menu-item-name item-list target action)))))

(defun merge-simple-menu-items (menu item-list target action)
  ;; Makes the menu-items in menu reflect the item-list.
  ;; Deletes or renames items in the menu if necessary.
  ;; Adds new items to the menu if necessary.
  ;; Changes target and or action if necessary.
  ;; This might be used in a #/menuNeedsUpdate: method in a menu delegate object
  ;; to update the menu's items just before it is displayed.
  (do* ((action-selector (if action 
                           (get-selector (string action))
                           (%null-ptr)))
        (mi-list (ns-to-lisp-list (#/itemArray menu) :element-class ns:ns-object)
                 (rest mi-list))
        (new-items item-list
                   (rest new-items))
        (mi (first mi-list)
            (first mi-list))
        (item (first new-items)
              (first new-items))
        (item-ns-str (when item (lisp-to-temp-nsstring item))
                     (when item (lisp-to-temp-nsstring item)))
        (tag 0
             (1+ tag)))
       ((and (null mi-list) (null new-items)))
    (cond ((null mi)
           (let ((new-item (#/addItemWithTitle:action:keyEquivalent: menu
                                                                     item-ns-str
                                                                     action-selector
                                                                     #@"")))
             (when (obj-if-not-null target)
               (#/setTarget: new-item target))
             (#/setTag: new-item tag)))
          ((null item)
           (#/removeItem: menu mi))
          ((#/isEqualToString: (#/title mi) item-ns-str)
           ;; they're in sync, but update the targe and action just to be safe
           (when (obj-if-not-null target)
             (#/setTarget: mi target))
           (#/setAction: mi action-selector))
          (t
           ;; different menu items, alter the existing one to match what we want
           (#/setTitle: mi item-ns-str)
           (when (obj-if-not-null target)
             (#/setTarget: mi target))
           (#/setAction: mi action-selector)))))
           
(defun install-menu-from-nib-after (nib-path menu-name &key (bundle ns:ns-bundle) (nib-owner #&NSApp))
  ;; read a menu from a nib file and add it into the main menu after the item
  ;; named menu-name. The menu in NIB should not itself be a main menu or it
  ;; will just replace the existing main menu when it is loaded. To create such
  ;; a nib, start with a blank template and add an NSMenu object to it.
  (let* ((nib-menu (find ns:ns-menu 
                         (load-nibfile nib-path :nib-owner nib-owner :bundle bundle)
                         :key #'class-of))
         (ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (insert-index (1+ (#/indexOfItemWithTitle: main-menu ns-menu-name)))
         (new-menuitem (make-instance ns:ns-menu-item)))
    (when nib-menu
      (#/setSubmenu: new-menuitem nib-menu)
      (#/setTitle: new-menuitem (#/title nib-menu))
      (#/insertItem:atIndex: main-menu new-menuitem insert-index))
    (#/release new-menuitem)
    (#/release ns-menu-name)))

(defun make-popup-button (parent-view pull-down-p title menu-items &key (install nil))
  (let* ((parent-frame (#/frame parent-view))
         (pop-up-rect (ns:make-ns-rect 0 (- (ns:ns-rect-height parent-frame) 30) 150 20))
         (pop-up (#/initWithFrame:pullsDown: (#/alloc ns:ns-pop-up-button)
                                             pop-up-rect
                                             (if pull-down-p #$YES #$NO))))
    (#/addItemsWithTitles: pop-up (lisp-to-ns-array (if pull-down-p
                                                      (append (list title) menu-items)
                                                      menu-items)))
    (unless pull-down-p
      (when title
        (#/setTitle: pop-up (lisp-to-temp-nsstring title))))
    (#/sizeToFit pop-up)
    ;; Make the pop-up a sub-view of the parent-view
    (when install
      (#/addSubview: parent-view pop-up))
    pop-up))

(defun pop-up-select (parent-view title items lisp-action-func &rest other-args)
  ;; creates a pop-up button with the items shown and attaches it to the 
  ;; view. When an item is selected it runs the lisp-action-func passing it
  ;; two arguments: the item selected and its index and also any additional
  ;; arguements found in other-args.
  (let* ((pop-up (make-popup-button parent-view t title items))
         (target (make-instance 'lisp-menu-target
                   :lisp-func #'(lambda (sender)
                                  (let ((indx (1- (#/indexOfItem: pop-up (#/selectedItem sender)))))
                                    (#/removeFromSuperview pop-up)
                                    (apply lisp-action-func 
                                           (nth indx items)
                                           indx
                                           other-args))))))
    (#/setAction: pop-up (ccl::@selector "doIt:"))
    (#/setTarget: pop-up target)
    (#/addSubview: parent-view pop-up)))

;; The idea here is to permit the mixing and matching of arbitrary sets of menuitems in the
;; main menu. Typically there might be two sets, one for the standard CCL menus and one for 
;; those of some application loaded under the IDE. The lisp-application tools add another set
;; consisting of a single menuitem (the "Dev" menu) that stays in the displayed main menu at
;; all times. We also want to be able to find the save key for any menu-items associated
;; with it.

(let ((saved-menu-hash (make-hash-table :test #'eql))
      (mi-hash (make-hash-table :test #'eql))
      (starting-menu nil)
      (app-menu nil))

  (defun clear-key (key)
    (let ((mi-list (gethash key saved-menu-hash)))
      (dolist (mi mi-list)
        (setf (gethash mi mi-hash) nil)
        (#/release mi)))
    (setf (gethash key saved-menu-hash) nil))

  (defun save-main-menu ()
    (let* ((current-main-menu (#/mainMenu #&NSApp))
           (mi-list (gethash current-main-menu saved-menu-hash)))
      (unless starting-menu
        (setf starting-menu current-main-menu))
      (if mi-list
        ;; We've already saved this menu, so ignore this request.
        ;; If it is necessary to re-save the menuitems for a particular menu the
        ;; function clear-key should be called with the key first and then save-main-menu
        ;; can be called.
        (return-from save-main-menu nil)
        ;; otherwise retain the current main menu object so that if it is ever replaced
        ;; it won't disappear
        (#/retain current-main-menu))
      ;; Next gather up the set of menuitems and make that the value of the key
      ;; that is the main menu object in the hash table
      (setf mi-list (ns-to-lisp-list (#/itemArray current-main-menu)
                                     :element-class ns:ns-object))
      (dolist (mi mi-list)
        (setf (gethash mi mi-hash) current-main-menu)
        (#/retain mi))
      (setf (gethash current-main-menu saved-menu-hash) mi-list)))

  (defun save-mi-list-with-key (mi-list key)
    ;; lets an application save a list of menuitems that can be added to or removed from
    ;; the main menu using the functions add-to-main-menu and remove-from-main-menu. The key
    ;; used here would also be provided when calling those functions. If the key is an
    ;; objective-C object then it MUST be an NSMenu. Keys are tested with #'eql so a string
    ;; may not be the best choice for a key.
    (let ((old-mi-list (gethash key saved-menu-hash)))
      (when (and (null old-mi-list) (ccl::objc-object-p key))
        (#/retain key))
      (when old-mi-list
        ;; we're going to change the existing list, so release all the old ones
        (dolist (mi old-mi-list)
          (#/release mi)))
      (dolist (mi mi-list)
        (setf (gethash mi mi-hash) key)
        (#/retain mi))
      (setf (gethash key saved-menu-hash) mi-list)))

  (defun add-to-main-menu (key &optional (index-or-title nil))
    ;; If not already included in the displayed main menu, add all the menuitems
    ;; in the list for that key. Since menuitems can only be in one menu at a time
    ;; we check to see whether they are still in the menu specified by the key
    ;; if so we'll remove them from that menu and add them to the current main menu.
    ;; If index-or-title is a string, insert just prior to the item with that title
    (let* ((current-main-menu (#/mainMenu #&NSApp))
           (current-mi-list (ns-to-lisp-list (#/itemArray current-main-menu)
                                             :element-class ns:ns-object))
           (indx (and index-or-title (if (numberp index-or-title)
                                       index-or-title
                                       (position index-or-title
                                                 current-mi-list 
                                                 :test #'string=
                                                 :key #'(lambda (mi)
                                                          (ns-to-lisp-string (#/title mi)))))))
           (to-be-added-mi-list (gethash key saved-menu-hash))
           (need-to-be-added-mi-list (remove-if #'(lambda (mi)
                                                    (member mi current-mi-list))
                                                to-be-added-mi-list)))
      (dolist (mi need-to-be-added-mi-list)
        ;; if it is in another menu, remove it
        (let ((current-menu (#/menu mi)))
          (when (not (eql (%null-ptr) current-menu))
            (#/removeItem: current-menu mi)))
        (if indx
          (progn
            (#/insertItem:atIndex: current-main-menu mi indx)
            (incf indx))
          (#/addItem: current-main-menu mi)))))
      
  (defun remove-from-main-menu (key)
    ;; If  already included in the displayed main menu, remove all the menuitems
    ;; in the list for that key.
    (let* ((current-main-menu (#/mainMenu #&NSApp))
           (current-mi-list (ns-to-lisp-list (#/itemArray current-main-menu)
                                             :element-class ns:ns-object))
           (to-be-removed-mi-list (gethash key saved-menu-hash))
           (need-to-be-removed-mi-list (intersection to-be-removed-mi-list current-mi-list)))
      (dolist (mi need-to-be-removed-mi-list)
        (#/removeItem: current-main-menu mi))))

  (defun remove-all-but (key)
    ;; Remove all the menuitems that aren't associated with the key from the main menu
    (let* ((current-main-menu (#/mainMenu #&NSApp))
           (current-mi-list (ns-to-lisp-list (#/itemArray current-main-menu)
                                             :element-class ns:ns-object))
           (to-stay-mi-list (gethash key saved-menu-hash))
           (need-to-be-removed-mi-list (set-difference current-mi-list to-stay-mi-list)))
      (dolist (mi need-to-be-removed-mi-list)
        (#/removeItem: current-main-menu mi))))

  (defun delete-menu (key)
    ;; once delete-menu has been called, key should no longer be used
    (remove-from-main-menu key)
    (dolist (mi (gethash key saved-menu-hash))
      (#/release mi))
    (setf (gethash key saved-menu-hash) nil)
    (when (ccl::objc-object-p key) 
      (#/release key)))
  
  (defun clear-saved-menus ()
    ;; Release all the saved objects and set up a new hash table
    (maphash #'(lambda (key mi-list)
                 (when (ccl::objc-object-p key)
                   (#/release key))
                 (dolist (mi mi-list)
                   (#/release mi)))
             saved-menu-hash)
    (setf saved-menu-hash (make-hash-table :test #'eql)))

  (defun starting-menu ()
    starting-menu)

  (defun app-menu ()
    app-menu)

  (defun set-app-menu (menu)
    (setf app-menu menu))

  (defun restore-starting-menu ()
    (when starting-menu
      (remove-all-but starting-menu)
      (add-to-main-menu starting-menu)))

  (defun key-for-menuitem (mi)
    (do* ((menu-item mi
                     (#/itemAtIndex: super-menu
                                     (#/indexOfItemWithSubmenu: super-menu mi-menu)))
          (mi-menu (#/menu menu-item)
                   (#/menu menu-item))
          (super-menu (#/supermenu mi-menu)
                      (#/supermenu mi-menu))
          (hash-key (gethash menu-item mi-hash nil)
                    (gethash menu-item mi-hash nil)))
         ((or hash-key (eql super-menu (%null-ptr))) hash-key)))

  (defun smh ()
    saved-menu-hash)

)

#|
(defun test-menu ()
  (iu::make-and-install-menu "New App Menu" 
                             '("Menu Item1" "doFirstThing")
                             '("Menu Item2" "doSecondThing")))

(defun test2-menu ()
  (iu::make-and-install-menuitems-after "File" "New"
                                        '("New myDoc" "newMyDoc")))

(iu::pop-up-select w "Pick it:" (list "a" "b" "1" "2") #'(lambda (it indx) (format t "~%~a is at ~s" it indx)))
                                         
|#

(provide :menu-utils)