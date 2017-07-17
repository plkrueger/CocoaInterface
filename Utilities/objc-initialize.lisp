;; objc-initialize.lisp

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

WARNING: This will replace CCL-defined initialize-instance :after methods for ns:ns-window and ns:ns-view
which are defined in cocoa-window.lisp. While I made every effort to ensure compatibility with those 
methods and have experienced no difficulty when running with my replacements, if you have other code that
depends on specific keywords defined in those routines, you may have to either modify your code or not
load this file.

This file provides initialize-instance :after methods for many standard Objective-C classes, 
especially ns:ns-view subclasses, but it is not a comprehensive list. The norm in Lisp is to 
call make-instance and provide all necessary initialization values as keywords. In Objective-C
the norm is to provide a few initWith... methods that supply only a limited number of initialization 
parameters. The developer is then expected to customize those objects by using any number of set... 
methods. We could certainly do the same in Lisp, but to make life more natural I have created these 
initialize-instance :after methods. The idea is that you can do something like the following:
   (make-instance '<ns-object-class-name> {:key val}*)
Most common sorts of argument types needed for the Objective-C methods are automatically converted from
corresponding Lisp types provided in the make-instance call. Many sorts of constants are automatically
converted from keyword equivalents (see the convert... methods for those values).

If their superclass also defines the initialize-instance :after method then of course it will be called as well. 
Each method defines keywords corresponding to set... methods that are specific to that class. So in an 
initialize-instance :after method for any object you can use keywords for that class or any of its superclasses.

Many supporting functions named convert-... will convert keyword arguments. If the meaning of them is not
clear, you should consult Apple documentation for additional information.

The function initialize-keys is provided to give you a list of valid keywords for any object that you might
want to initialize.

Note that these methods will NOT be called if you create instances using normal Objective-C forms such as
  (#/init (#/alloc <class>))
They will only be invoked if you specifically call make-instance ...

Final suggestions. If you execute any make-instance forms from the Lisp listener for these classes 
(especially classes associated withwindows or views) you should wrap the call with a (on-main-thread ... )
form (defined in ns-object-utils.lisp). This assures that the objects are created on the main thread, which
is increasingly necessary for Objective-C objects created at runtime. Objects returned by these methods will
typically be owned by the caller and you must explicitly (#/release <object) when you are done with it to 
avoid a memory leak. Note that a memory leak is much better than trying to reference a ptr to an object that
was released prematurely. Such a reference is likely to cause a runtime exception.

|#

(assert (>= #$NSAppKitVersionNumber #$NSAppKitVersionNumber10_7))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :coerce-obj)
  (require :selector-utils)
  (require :list-utils)
  (require :file-directory-utils))

(in-package :iu)

;; convert- ... functions

(defun convert-animation-behavior (ab-key)
  (if (numberp ab-key)
    ab-key
    (ecase ab-key
      (:default #$NSWindowAnimationBehaviorDefault)
      (:none #$NSWindowAnimationBehaviorNone)
      (:document-window #$NSWindowAnimationBehaviorDocumentWindow)
      (:utility-window #$NSWindowAnimationBehaviorUtilityWindow)
      (:alert-panel #$NSWindowAnimationBehaviorAlertPanel))))

(defun convert-bezel-style (style-key)
  (if (numberp style-key)
    style-key
    (ecase style-key
      (:rounded #$NSRoundedBezelStyle)
      (:regular-square #$NSRegularSquareBezelStyle)
      (:thick-square #$NSThickSquareBezelStyle)
      (:thicker-square #$NSThickerSquareBezelStyle)
      (:disclosure #$NSDisclosureBezelStyle)
      (:shadowless-square #$NSShadowlessSquareBezelStyle)
      (:circular #$NSCircularBezelStyle)
      (:textured-square #$NSTexturedSquareBezelStyle)
      (:help-button #$NSHelpButtonBezelStyle)
      (:small-square #$NSSmallSquareBezelStyle)
      (:textured-rounded #$NSTexturedRoundedBezelStyle)
      (:round-rect #$NSRoundRectBezelStyle)
      (:recessed #$NSRecessedBezelStyle)
      (:rounded-disclosure #$NSRoundedDisclosureBezelStyle)
      (:inline #$NSInlineBezelStyle)
      (:small-icon-button #$NSSmallIconButtonBezelStyle))))

(defun convert-border (border-key)
  (if (numberp border-key)
    border-key
    (ecase border-key
      (:line #$NSLineBorder)
      (:bezel #$NSBezelBorder)
      (:groove #$NSGrooveBorder)
      (:none #$NSNoBorder))))

(defun convert-box-type (bt-key)
  (if (numberp bt-key)
    bt-key
    (ecase bt-key
      (:primary #$NSBoxPrimary)
      (:secondary #$NSBoxSecondary)
      (:separator #$NSBoxSeparator)
      (:old-style #$NSBoxOldStyle)
      (:custom #$NSBoxCustom))))

(defun convert-button-type (type-key)
  (if (numberp type-key)
    type-key
    (ecase type-key
      (:momentary-light #$NSMomentaryLightButton)
      (:push-on-push-off #$NSPushOnPushOffButton)
      (:toggle #$NSToggleButton)
      (:switch #$NSSwitchButton)
      (:radio #$NSRadioButton)
      (:momentary-change #$NSMomentaryChangeButton)
      (:on-off #$NSOnOffButton)
      (:momentary-push-in #$NSMomentaryPushInButton)
      (:momentary-push #$NSMomentaryPushButton))))

(defun convert-collection-behavior (c-key)
  (cond ((listp c-key)
         (apply #'logior (mapcar #'convert-collection-behavior c-key)))
        ((numberp c-key)
         c-key)
        (t
         (ecase c-key
           (:default #$NSWindowCollectionBehaviorDefault)
           (:can-join-all-spaces #$NSWindowCollectionBehaviorCanJoinAllSpaces)
           (:move-to-active-space #$NSWindowCollectionBehaviorMoveToActiveSpace)
           (:managed #$NSWindowCollectionBehaviorManaged)
           (:transient #$NSWindowCollectionBehaviorTransient)
           (:stationary #$NSWindowCollectionBehaviorStationary)
           (:participates-in-cycle #$NSWindowCollectionBehaviorParticipatesInCycle)
           (:ignores-cycle #$NSWindowCollectionBehaviorIgnoresCycle)
           (:full-screen-primary #$NSWindowCollectionBehaviorFullScreenPrimary)
           (:full-screen-auxiliary #$NSWindowCollectionBehaviorFullScreenAuxiliary)))))

(defun convert-column-resizing (cr-key)
  (if (numberp cr-key)
    cr-key
    (ecase cr-key
      (:no-resizing #$NSTableColumnNoResizing)
      (:auto-resizing #$NSTableColumnAutoresizingMask)
      (:user-resizing #$NSTableColumnUserResizingMask))))

(defun convert-cursor (cursor-key)
  ;; not all applicable to scroll-views, but included for completeness and use elsewhere
  (if (numberp cursor-key)
    cursor-key
    (ecase cursor-key
      (:arrow (#/arrowCursor ns:ns-cursor))
      (:closed-hand (#/closedHandCursor ns:ns-cursor))
      (:contextual-menu (#/contextualMenuCursor ns:ns-cursor))
      (:crosshair (#/crosshairCursor ns:ns-cursor))
      (:current (#/currentCursor ns:ns-cursor))
      (:current-system (#/currentSystemCursor ns:ns-cursor))
      (:disappearing-item (#/disappearingItemCursor ns:ns-cursor))
      (:drag-copy (#/dragCopyCursor ns:ns-cursor))
      (:drag-link (#/dragLinkCursor ns:ns-cursor))
      (:ibeam (#/IBeamCursor ns:ns-cursor))
      (:ibeam-for-vertical (#/IBeamCursorForVerticalLayout ns:ns-cursor))
      (:open-hand (#/openHandCursor ns:ns-cursor))
      (:op-not-allowed (#/operationNotAllowedCursor ns:ns-cursor))
      (:pointing-hand (#/pointingHandCursor ns:ns-cursor))
      (:resize-down (#/resizeDownCursor ns:ns-cursor))
      (:resize-left (#/resizeLeftCursor ns:ns-cursor))
      (:resize-left-right (#/resizeLeftRightCursor ns:ns-cursor))
      (:resize-right (#/resizeRightCursor ns:ns-cursor))
      (:resize-up (#/resizeUpCursor ns:ns-cursor))
      (:resize-up-down (#/resizeUpDownCursor ns:ns-cursor)))))

(defun convert-date-style (ds-key)
  (if (numberp ds-key)
    ds-key
    (ecase ds-key
      (:none #$NSDateFormatterNoStyle)
      (:short #$NSDateFormatterShortStyle)
      (:medium #$NSDateFormatterMediumStyle)
      (:long #$NSDateFormatterLongStyle)
      (:full #$NSDateFormatterFullStyle))))

(defun convert-elasticity (e-key)
  (if (numberp e-key)
    e-key
    (ecase e-key
      (:automatic #$NSScrollElasticityAutomatic)
      (:none #$NSScrollElasticityNone)
      (:allowed #$NSScrollElasticityAllowed))))

(defun convert-find-bar-position (find-key)
  (if (numberp find-key)
    find-key
    (ecase find-key
      (:above-ruler #$NSScrollViewFindBarPositionAboveHorizontalRuler)
      (:above-content #$NSScrollViewFindBarPositionAboveContent)
      (:below-content #$NSScrollViewFindBarPositionBelowContent))))

(defun convert-focus-ring-type (fr-key)
  (if (numberp fr-key)
    fr-key
    (ecase fr-key
      (:default #$NSFocusRingTypeDefault)
      (:none #$NSFocusRingTypeNone)
      (:exterior #$NSFocusRingTypeExterior))))

(defun convert-grid-style (s-key)
  (if (numberp s-key)
    s-key
    (ecase s-key
      (:none #$NSTableViewGridNone)
      (:solid-vertical #$NSTableViewSolidVerticalGridLineMask)
      (:solid-horizontal #$NSTableViewSolidHorizontalGridLineMask)
      (:dashed-horizontal #$NSTableViewDashedHorizontalGridLineMask))))

(defun convert-grid-styles (style)
  (if (listp style)
    (apply #'logior (mapcar #'convert-grid-style style))
    (convert-grid-style style)))

(defun convert-image-position (i-key)
#|
Together, NSCellHasImageOnLeftOrBottom, NSCellHasImageHorizontal, and NSCellHasOverlappingImage 
control the position of the cell’s image and text. To place the image above, set none of them. 
To place the image below, set NSCellHasImageOnLeftOrBottom. To place the image to the right, 
set NSCellHasImageHorizontal. To place the image to the left, set NSCellHasImageHorizontal and 
NSCellHasImageOnLeftOrBottom. To place the image directly over, set NSCellHasOverlappingImage.
|#
  (if (numberp i-key)
    i-key
    (ecase i-key
      (:no-image #$NSNoImage)
      (:image-only #$NSImageOnly)
      (:image-left #$NSImageLeft)
      (:image-right #$NSImageRight)
      (:image-below #$NSImageBelow)
      (:image-above #$NSImageAbove)
      (:image-overlaps #$NSImageOverlaps))))

(defun convert-key-modifier-mask (mod-key)
  (cond ((listp mod-key)
         (apply #'logior (mapcar #'convert-key-modifier-mask mod-key)))
        ((numberp mod-key)
         mod-key)
        (t
         (ecase mod-key
           (:shift #$NSShiftKeyMask)
           (:control #$NSControlKeyMask)
           (:alternate #$NSAlternateKeyMask)
           (:option #$NSAlternateKeyMask) ;; same as alt key
           (:command #$NSCommandKeyMask)))))

(defun convert-knob-style (k-key)
  (if (numberp k-key)
    k-key
    (ecase k-key
      (:default #$NSScrollerKnobStyleDefault)
      (:dark #$NSScrollerKnobStyleDark)
      (:light #$NSScrollerKnobStyleLight))))

(defun convert-number-style (ns-key)
  (if (numberp ns-key)
    ns-key
    (ecase ns-key
      (:none #$NSNumberFormatterNoStyle)
      (:decimal #$NSNumberFormatterDecimalStyle)
      (:currency #$NSNumberFormatterCurrencyStyle)
      (:percent #$NSNumberFormatterPercentStyle)
      (:scientific #$NSNumberFormatterScientificStyle)
      (:spell-out #$NSNumberFormatterSpellOutStyle))))

(defun convert-padding-position (pp-key)
  (if (numberp pp-key)
    pp-key
    (ecase pp-key
      (:before-prefix #$NSNumberFormatterPadBeforePrefix)
      (:after-prefix #$NSNumberFormatterPadAfterPrefix)
      (:before-suffix #$NSNumberFormatterPadBeforeSuffix)
      (:after-suffix #$NSNumberFormatterPadAfterSuffix))))

(defun convert-resizing-mask (rs-key)
  (cond ((listp rs-key)
         (apply #'logior (mapcar #'convert-resizing-mask rs-key)))
        ((numberp rs-key)
         rs-key)
        (t
         (ecase rs-key
           (:not-sizable #$NSViewNotSizable)
           (:min-x-margin #$NSViewMinXMargin)
           (:width-sizable #$NSViewWidthSizable)
           (:max-x-margin #$NSViewMaxXMargin)
           (:min-y-margin #$NSViewMinYMargin)
           (:height-sizable #$NSViewHeightSizable)
           (:max-y-margin #$NSViewMaxYMargin)))))

(defun convert-row-size-style (rss-key)
  (if (numberp rss-key)
    rss-key
    (ecase rss-key
      (:default #$NSTableViewRowSizeStyleDefault)
      (:custom #$NSTableViewRowSizeStyleCustom)
      (:small #$NSTableViewRowSizeStyleSmall)
      (:medium #$NSTableViewRowSizeStyleMedium)
      (:large #$NSTableViewRowSizeStyleLarge))))

(defun convert-rounding-mode (rm-key)
  (if (numberp rm-key)
    rm-key
    (ecase rm-key
      (:ceiling #$NSNumberFormatterRoundCeiling)
      (:floor #$NSNumberFormatterRoundFloor)
      (:down #$NSNumberFormatterRoundDown)
      (:up #$NSNumberFormatterRoundUp)
      (:half-even #$NSNumberFormatterRoundHalfEven)
      (:half-down #$NSNumberFormatterRoundHalfDown)
      (:half-up #$NSNumberFormatterRoundHalfUp))))

(defun convert-scroller-style (style-key)
  (if (numberp style-key)
    style-key
    (ecase style-key
      (:legacy #$NSScrollerStyleLegacy)
      (:overlay #$NSScrollerStyleOverlay))))

(defun convert-selection-highlight-style (shs-key)
  (if (numberp shs-key)
    shs-key
    (ecase shs-key
      (:none #$NSTableViewSelectionHighlightStyleNone)
      (:regular #$NSTableViewSelectionHighlightStyleRegular)
      (:source-list #$NSTableViewSelectionHighlightStyleSourceList))))

(defun convert-sharing-type (st-key)
  (if (numberp st-key)
    st-key
    (ecase st-key
      (:none #$NSWindowSharingNone)
      (:read-only #$NSWindowSharingReadOnly)
      (:read-write #$NSWindowSharingReadWrite))))

(defun convert-state (st-key)
  (if (numberp st-key)
    st-key
    (ecase st-key
    (:on #$NSOnState)
    (:off #$NSOffState)
    (:mixed #$NSMixedState))))

(defun convert-table-col-resizing (tcr-key)
  (if (numberp tcr-key)
    tcr-key
    (ecase tcr-key
      (:none #$NSTableViewNoColumnAutoresizing)
      (:uniform #$NSTableViewUniformColumnAutoresizingStyle)
      (:sequential-column #$NSTableViewSequentialColumnAutoresizingStyle)
      (:reverse-sequential-column #$NSTableViewReverseSequentialColumnAutoresizingStyle)
      (:last-column-only #$NSTableViewLastColumnOnlyAutoresizingStyle)
      (:first-column-only #$NSTableViewFirstColumnOnlyAutoresizingStyle))))

(defun convert-table-drag-feedback-style (td-key)
  (if (numberp td-key)
    td-key
    (ecase td-key
      (:none #$NSTableViewDraggingDestinationFeedbackStyleNone)
      (:regular #$NSTableViewDraggingDestinationFeedbackStyleRegular)
      (:source-list #$NSTableViewDraggingDestinationFeedbackStyleSourceList))))

(defun convert-table-drag-src-op (td-key)
  (if (numberp td-key)
    td-key
    (ecase td-key
      (:none #$NSDragOperationNone)
      (:copy #$NSDragOperationCopy)
      (:link #$NSDragOperationLink)
      (:generic #$NSDragOperationGeneric)
      (:private #$NSDragOperationPrivate)
      (:move #$NSDragOperationMove)
      (:delete #$NSDragOperationDelete)
      (:every #$NSDragOperationEvery))))

(defun convert-text-alignment (al-key)
  (if (numberp al-key)
    al-key
    (ecase al-key
      (:left #$NSLeftTextAlignment)
      (:right #$NSRightTextAlignment)
      (:center #$NSCenterTextAlignment)
      (:justified #$NSJustifiedTextAlignment)
      (:natural #$NSNaturalTextAlignment))))

(defun convert-text-bezel-style (b-key)
  (if (numberp b-key)
    b-key
    (ecase b-key
    (:square #$NSTextFieldSquareBezel)
    (:rounded #$NSTextFieldRoundedBezel))))

(defun convert-text-checking-type (type)
  (ecase type
    (:all-system #$NSTextCheckingAllSystemTypes)
    (:all-custom #$NSTextCheckingAllCustomTypes)
    (:all #$NSTextCheckingAllTypes)))

(defun convert-text-checking-types (types)
  (typecase types
    (number types)
    (keyword (convert-text-checking-type types))
    (list (apply #'logior (mapcar #'convert-text-checking-type types)))
    (t types))) ;; let it error

(defun convert-text-orientation (o-key)
  ;; should be one of :horizontal, :h, :vertical, or :v
  (if (numberp o-key)
    o-key
    (ecase o-key
      ((:vertical :v) #$NSTextLayoutOrientationVertical)
      ((:horizontal :h) #$NSTextLayoutOrientationHorizontal))))

(defun convert-title-position  (tp-key)
  (if (numberp tp-key)
    tp-key
    (ecase tp-key
      (:none #$NSNoTitle)
      (:above-top #$NSAboveTop)
      (:at-top #$NSAtTop)
      (:below-top #$NSBelowTop)
      (:above-bottom #$NSAboveBottom)
      (:at-bottom #$NSAtBottom)
      (:below-bottom #$NSBelowBottom))))

(defun convert-window-backing-loc (wb-key)
  (if (numberp wb-key)
    wb-key
    (ecase wb-key
      (:default #$NSWindowBackingLocationDefault)
      (:video-memory #$NSWindowBackingLocationVideoMemory)
      (:main-memory #$NSWindowBackingLocationMainMemory))))

(defun convert-window-depth (wd-key)
  (if (numberp wd-key)
    wd-key
    (ecase wd-key
      (:rgb-24 #$NSWindowDepthTwentyfourBitRGB)
      (:rgb-64 #$NSWindowDepthSixtyfourBitRGB)
      (:rgb-128 #$NSWindowDepthOnehundredtwentyeightBitRGB))))

(defun convert-window-level (wl-key)
  ;; For some reason the defined constants are not in the CCL interface database, but I found 
  ;; http://dev.dartmouth.edu/svn/softdev/email/blitz/clients/BlitzMail/tags/start/projDev%20Mac%20CIncludes/CGWindowLevel.h 
  ;; which provided the mechanism to derive the values.
  (if (numberp wl-key)
    wl-key
    (ecase wl-key
      (:normal (#_CGWindowLevelForKey #$kCGNormalWindowLevelKey)) ;; #$NSNormalWindowLevel
      (:floating (#_CGWindowLevelForKey #$kCGFloatingWindowLevelKey)) ;; #$NSFloatingWindowLevel
      (:submenu (#_CGWindowLevelForKey #$kCGTornOffMenuWindowLevelKey)) ;; #$NSSubmenuWindowLevel (same as torn-off)
      (:torn-off-menu (#_CGWindowLevelForKey #$kCGTornOffMenuWindowLevelKey)) ;; #$NSTornOffMenuWindowLevel
      (:main-menu (#_CGWindowLevelForKey #$kCGMainMenuWindowLevelKey)) ;; #$NSMainMenuWindowLevel
      (:status (#_CGWindowLevelForKey #$kCGStatusWindowLevelKey)) ;; #$NSStatusWindowLevel
      (:modal-panel (#_CGWindowLevelForKey #$kCGModalPanelWindowLevelKey)) ;; #$NSModalPanelWindowLevel
      (:pop-up-menu (#_CGWindowLevelForKey #$kCGPopUpMenuWindowLevelKey)) ;; #$NSPopUpMenuWindowLevel
      (:screen-saver (#_CGWindowLevelForKey #$kCGScreenSaverWindowLevelKey)) ;; #$NSScreenSaverWindowLevel
      (:dock-window (#_CGWindowLevelForKey #$kCGDockWindowLevelKey))))) ;; #$NSDockWindowLevel

(defun convert-writing-direction (wd-key)
  (if (numberp wd-key)
    wd-key
    (ecase wd-key
      (:natural #$NSWritingDirectionNatural)
      (:left-to-right #$NSWritingDirectionLeftToRight)
      (:right-to-left #$NSWritingDirectionRightToLeft))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-object subclasses

(defmethod initialize-instance :after ((self ns:ns-document)
                     &key
                     (display-name nil display-name-p) ;; string or ns:ns-string
                     (draft nil draft-p)
                     (has-undo-manager nil has-undo-manager-p)
                     (print-info nil print-info-p) ;; ns:ns-print-info object
                     (undo-manager nil undo-manager-p) ;; ns:ns-undo-manager object
                     &allow-other-keys)
  (when display-name-p
    (#/setDisplayName: self (coerce-obj display-name 'ns:ns-string)))
  (when draft-p
    (#/setDraft: self draft))
  (when has-undo-manager-p
    (#/setHasUndoManager: self has-undo-manager))
  (when print-info-p
    (#/setPrintInfo: self print-info))
  (when undo-manager-p
    (#/setUndoManager: self undo-manager)))

(defmethod initialize-instance :after ((self ns:ns-menu)
                     &key
                     (allows-context-menu-plug-ins nil allows-context-menu-plug-ins-p)
                     (autoenables-items nil autoenables-items-p)
                     (delegate nil delegate-p) ;; an object that follows the NSMenuDelegate protocol
                     (font nil font-p)  ;; ns:ns-font
                     (minimum-width nil minimum-width-p)  ;; number coerceable to a double float
                     (shows-state-column nil shows-state-column-p)
                     (title nil title-p) ;; string, attributed-string, ns:ns-string
                     (menu-items nil menu-items-p) ;; list of ns:ns-menu-item
                     &allow-other-keys)
  (when allows-context-menu-plug-ins-p
    (#/setAllowsContextMenuPlugIns: self allows-context-menu-plug-ins))
  (when autoenables-items-p
    (#/setAutoenablesItems: self autoenables-items))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when font-p
    (#/setFont: self font))
  (when minimum-width-p
    (#/setMinimumWidth: self (cgfloat minimum-width)))
  (when shows-state-column-p
    (#/setShowsStateColumn: self shows-state-column))
  (when title-p
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when menu-items-p
    (dolist (mi menu-items)
      (#/addItem: self mi))))

(defmethod initialize-instance :after ((self ns:ns-menu-item)
                     &key
                     (action nil action-p) ;; string or objective-c selector
                     (alternate nil alternate-p)
                     (attributed-title nil attributed-title-p)
                     (enabled nil enabled-p)
                     (hidden nil hidden-p)
                     (image nil image-p)  ;; ns:ns-image
                     (indentation-level nil indentation-level-p) ;; integer
                     (key-equivalent nil key-equivalent-p) ;; string, attributed-string, ns:ns-string
                     (key-equivalent-modifier-mask nil key-equivalent-modifier-mask-p)
                     (mixed-state-image nil mixed-state-image-p)  ;; ns:ns-image
                     (off-state-image nil off-state-image-p)  ;; ns:ns-image
                     (on-state-image nil on-state-image-p)  ;; ns:ns-image
                     (represented-object nil represented-object-p) ;; ns-ns-object
                     (state nil state-p) ;; one of :on :off :mixed
                     (submenu nil submenu-p) ;; ns:ns-menu
                     (tag nil tag-p) ;; integer
                     (target nil target-p)  ;; ns-ns-object
                     (title nil title-p) ;; string, attributed-string, ns:ns-string
                     (tool-tip nil tool-tip-p) ;; string, attributed-string, ns:ns-string
                     (view nil view-p)  ;; ns:ns-view
                     &allow-other-keys)
  (when action-p
    (#/setAction: self (get-selector action)))
  (when alternate-p
    (#/setAlternate: self alternate))
  (when attributed-title-p
    (#/setAttributedTitle: self (coerce-obj attributed-title 'ns:ns-attributed-string)))
  (when enabled-p
    (#/setEnabled: self enabled))
  (when hidden-p
    (#/setHidden: self hidden))
  (when image-p
    (#/setImage: self image))
  (when indentation-level-p
    (#/setIndentationLevel: self indentation-level))
  (when key-equivalent-p
    (#/setKeyEquivalent: self (coerce-obj key-equivalent 'ns:ns-string)))
  (when key-equivalent-modifier-mask-p
    (#/setKeyEquivalentModifierMask: self (convert-key-modifier-mask key-equivalent-modifier-mask)))
  (when mixed-state-image-p
    (#/setMixedStateImage: self mixed-state-image))
  (when off-state-image-p
    (#/setOffStateImage: self off-state-image))
  (when on-state-image-p
    (#/setOnStateImage: self on-state-image))
  (when represented-object-p
    (#/setRepresentedObject: self represented-object))
  (when state-p
    (#/setState: self (convert-state state)))
  (when submenu-p
    (#/setSubmenu: self submenu))
  (when tag-p
    (#/setTag: self tag))
  (when target-p
    (#/setTarget: self target))
  (when title-p
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when tool-tip-p
    (#/setToolTip: self (coerce-obj tool-tip 'ns:ns-string)))
  (when view-p
    (#/setView: self view)))

(defmethod initialize-instance :after ((self ns:ns-table-column)
                     &key
                     (column-title nil column-title-p) ;; string or ns:ns-string
                     (data-cell nil data-cell-p) ;; ns:ns-cell
                     (editable nil editable-p)
                     (formatter nil formatter-p) ;; ns:ns-formatter
                     (header-cell nil header-cell-p) ;; ns:ns-cell
                     (header-tool-tip nil header-tool-tip-p) ;; string or ns:ns-string
                     (hidden nil hidden-p)
                     (identifier nil identifier-p) ;; string or ns:ns-string
                     (max-width nil max-width-p) ;; number
                     (min-width nil min-width-p) ;; number
                     (resizing-mask nil resizing-mask-p) ;; number or keyword: :no-resizing :auto-resizing :user-resizing
                     (sort-descriptor-prototype nil sort-descriptor-prototype-p) ;; ns:ns-sort-descriptor
                     (width nil width-p) ;; number
                     (wraps nil wraps-p)
                     &allow-other-keys)
  (when column-title-p
    (#/setStringValue: (#/headerCell self) (coerce-obj column-title 'ns:ns-string)))
  (when data-cell-p
    (#/setDataCell: self data-cell))
  (when editable-p
    (#/setEditable: self editable))
  (when formatter-p
    (#/setFormatter: (#/dataCell self) formatter))
  (when header-cell-p
    (#/setHeaderCell: self header-cell))
  (when header-tool-tip-p
    (#/setHeaderToolTip: self (coerce-obj header-tool-tip 'ns:ns-string)))
  (when hidden-p
    (#/setHidden: self hidden))
  (when identifier-p
    (#/setIdentifier: self (coerce-obj identifier 'ns:ns-string)))
  (when max-width-p
    (#/setMaxWidth: self (cgfloat max-width)))
  (when min-width-p
    (#/setMinWidth: self (cgfloat min-width)))
  (when resizing-mask-p
    (#/setResizingMask: self (convert-column-resizing resizing-mask)))
  (when sort-descriptor-prototype-p
    (#/setSortDescriptorPrototype: self sort-descriptor-prototype))
  (when width-p
    (#/setWidth: self (cgfloat width)))
  (when wraps-p
    (#/setWraps: (#/dataCell self) wraps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-formatter subclasses

(defmethod initialize-instance :after ((self ns:ns-date-formatter)
                     &key
                     (calendar nil calendar-p)  ;; ns:ns-calendar
                     (date-format nil date-format-p) ;; string or ns:ns-string
                     (date-style nil date-style-p) ;; number or keyword: :none :short :medium :long :full
                     (default-date nil default-date-p) ;; lisp date or ns:ns-date
                     (does-relative-date-formatting nil does-relative-date-formatting-p)
                     (era-symbols nil era-symbols-p) ;; list of strings or ns:ns-string objects
                     (gregorian-start-date nil gregorian-start-date-p)  ;; lisp date or ns:ns-date
                     (lenient nil lenient-p)
                     (locale nil locale-p) ;; ns:ns-locale
                     (long-era-symbols nil long-era-symbols-p) ;; list of strings or ns:ns-string objects
                     (month-symbols nil month-symbols-p) ;; list of strings or ns:ns-string objects
                     (pm-symbol nil pm-symbol-p) ;; string or ns:ns-string
                     (quarter-symbols nil quarter-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-month-symbols nil short-month-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-quarter-symbols nil short-quarter-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-standalone-month-symbols nil short-standalone-month-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-standalone-quarter-symbols nil short-standalone-quarter-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-standalone-weekday-symbols nil short-standalone-weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     (short-weekday-symbols nil short-weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     (standalone-month-symbols nil standalone-month-symbols-p) ;; list of strings or ns:ns-string objects
                     (standalone-quarter-symbols nil standalone-quarter-symbols-p) ;; list of strings or ns:ns-string objects
                     (standalone-weekday-symbols nil standalone-weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     (time-style nil time-style-p) ;; number or keyword: :none :short :medium :long :full
                     (time-zone nil time-zone-p) ;; ns:ns-time-zone
                     (two-digit-start-date nil two-digit-start-date-p) ;; lisp date or ns:ns-date
                     (very-short-month-symbols nil very-short-month-symbols-p) ;; list of strings or ns:ns-string objects
                     (very-short-standalone-month-symbols nil very-short-standalone-month-symbols-p) ;; list of strings or ns:ns-string objects
                     (very-short-standalone-weekday-symbols nil very-short-standalone-weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     (very-short-weekday-symbols nil very-short-weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     (weekday-symbols nil weekday-symbols-p) ;; list of strings or ns:ns-string objects
                     &allow-other-keys)
  (when calendar-p
    (#/setCalendar: self calendar))
  (when date-format-p
    (#/setDateFormat: self (coerce-obj date-format 'ns:ns-string)))
  (when date-style-p
    (#/setDateStyle: self (convert-date-style date-style)))
  (when default-date-p
    (#/setDefaultDate: self (coerce-obj default-date 'ns:ns-date)))
  (when does-relative-date-formatting-p
    (#/setDoesRelativeDateFormatting: self does-relative-date-formatting))
  (when era-symbols-p
    (#/setEraSymbols: self (coerce-obj era-symbols 'ns:ns-array)))
  (when gregorian-start-date-p
    (#/setGregorianStartDate: self (coerce-obj gregorian-start-date 'ns:ns-date)))
  (when lenient-p
    (#/setLenient: self lenient))
  (when locale-p
    (#/setLocale: self locale))
  (when long-era-symbols-p
    (#/setLongEraSymbols: self (coerce-obj long-era-symbols 'ns:ns-array)))
  (when month-symbols-p
    (#/setMonthSymbols: self (coerce-obj month-symbols 'ns:ns-array)))
  (when pm-symbol-p
    (#/setPMSymbol: self (coerce-obj pm-symbol 'ns:ns-string)))
  (when quarter-symbols-p
    (#/setQuarterSymbols: self (coerce-obj quarter-symbols 'ns:ns-array)))
  (when short-month-symbols-p
    (#/setShortMonthSymbols: self (coerce-obj short-month-symbols 'ns:ns-array)))
  (when short-quarter-symbols-p
    (#/setShortQuarterSymbols: self (coerce-obj short-quarter-symbols 'ns:ns-array)))
  (when short-standalone-month-symbols-p
    (#/setShortStandaloneMonthSymbols: self (coerce-obj short-standalone-month-symbols 'ns:ns-array)))
  (when short-standalone-quarter-symbols-p
    (#/setShortStandaloneQuarterSymbols: self (coerce-obj short-standalone-quarter-symbols 'ns:ns-array)))
  (when short-standalone-weekday-symbols-p
    (#/setShortStandaloneWeekdaySymbols: self (coerce-obj short-standalone-weekday-symbols 'ns:ns-array)))
  (when short-weekday-symbols-p
    (#/setShortWeekdaySymbols: self (coerce-obj short-weekday-symbols 'ns:ns-array)))
  (when standalone-month-symbols-p
    (#/setStandaloneMonthSymbols: self (coerce-obj standalone-month-symbols 'ns:ns-array)))
  (when standalone-quarter-symbols-p
    (#/setStandaloneQuarterSymbols: self (coerce-obj standalone-quarter-symbols 'ns:ns-array)))
  (when standalone-weekday-symbols-p
    (#/setStandaloneWeekdaySymbols: self (coerce-obj standalone-weekday-symbols 'ns:ns-array)))
  (when time-style-p
    (#/setTimeStyle: self (convert-date-style time-style))) ;; date and time use same constants
  (when time-zone-p
    (#/setTimeZone: self time-zone))
  (when two-digit-start-date-p
    (#/setTwoDigitStartDate: self (coerce-obj two-digit-start-date 'ns:ns-date)))
  (when very-short-month-symbols-p
    (#/setVeryShortMonthSymbols: self (coerce-obj very-short-month-symbols 'ns:ns-array)))
  (when very-short-standalone-month-symbols-p
    (#/setVeryShortStandaloneMonthSymbols: self (coerce-obj very-short-standalone-month-symbols 'ns:ns-array)))
  (when very-short-standalone-weekday-symbols-p
    (#/setVeryShortStandaloneWeekdaySymbols: self (coerce-obj very-short-standalone-weekday-symbols 'ns:ns-array)))
  (when very-short-weekday-symbols-p
    (#/setVeryShortWeekdaySymbols: self (coerce-obj very-short-weekday-symbols 'ns:ns-array)))
  (when weekday-symbols-p
    (#/setWeekdaySymbols: self (coerce-obj weekday-symbols 'ns:ns-array))))

(defmethod initialize-instance :after ((self ns:ns-number-formatter)
                     &key
                     (allows-floats nil allows-floats-p) 
                     (always-shows-decimal-separator nil always-shows-decimal-separator-p) 
                     (attributed-string-for-nil nil attributed-string-for-nil-p) ;; string attributed-string ns:ns-string or ns:ns-attributed-string
                     (attributed-string-for-not-a-number nil attributed-string-for-not-a-number-p) ;; string attributed-string ns:ns-string or ns:ns-attributed-string
                     (attributed-string-for-zero nil attributed-string-for-zero-p) ;; string attributed-string ns:ns-string or ns:ns-attributed-string
                     (currency-decimal-separator nil currency-decimal-separator-p) ;; string or ns:ns-string
                     (currency-grouping-separator nil currency-grouping-separator-p) ;; string or ns:ns-string
                     (currency-symbol nil currency-symbol-p) ;; string or ns:ns-string
                     (decimal-separator nil decimal-separator-p) ;; string or ns:ns-string
                     (exponent-symbol nil exponent-symbol-p) ;; string or ns:ns-string
                     (format nil format-p) ;; string or ns:ns-string
                     (format-width nil format-width-p) ;; integer
                     (generates-decimal-numbers nil generates-decimal-numbers-p) 
                     (grouping-separator nil grouping-separator-p) ;; string or ns:ns-string
                     (grouping-size nil grouping-size-p) ;; integer
                     (has-thousand-separators nil has-thousand-separators-p) 
                     (international-currency-symbol nil international-currency-symbol-p) ;; string or ns:ns-string
                     (lenient nil lenient-p) 
                     (locale nil locale-p) ;; ns:ns-locale
                     (localizes-format nil localizes-format-p) 
                     (maximum nil maximum-p) ;; number
                     (maximum-fraction-digits nil maximum-fraction-digits-p) ;; integer
                     (maximum-integer-digits nil maximum-integer-digits-p) ;; integer
                     (maximum-significant-digits nil maximum-significant-digits-p) ;; integer
                     (minimum nil minimum-p) ;; number
                     (minimum-fraction-digits nil minimum-fraction-digits-p) ;; integer
                     (minimum-integer-digits nil minimum-integer-digits-p) ;; integer
                     (minimum-significant-digits nil minimum-significant-digits-p) ;; integer
                     (minus-sign nil minus-sign-p) ;; string or ns:ns-string
                     (multiplier nil multiplier-p) ;; number
                     (negative-format nil negative-format-p) ;; string or ns:ns-string
                     (negative-infinity-symbol nil negative-infinity-symbol-p) ;; string or ns:ns-string
                     (negative-prefix nil negative-prefix-p) ;; string or ns:ns-string
                     (negative-suffix nil negative-suffix-p) ;; string or ns:ns-string
                     (nil-symbol nil nil-symbol-p) ;; string or ns:ns-string
                     (not-a-number-symbol nil not-a-number-symbol-p) ;; string or ns:ns-string
                     (number-style nil number-style-p) ;; number or keyword: :none :decimal :currency :percent :scientific :spell-out
                     (padding-character nil padding-character-p) ;; string or ns:ns-string
                     (padding-position nil padding-position-p) ;; number or keyword: :before-prefix :after-prefix :before-suffix :after-suffix
                     (partial-string-validation-enabled nil partial-string-validation-enabled-p) 
                     (percent-symbol nil percent-symbol-p) ;; string or ns:ns-string
                     (per-mill-symbol nil per-mill-symbol-p) ;; string or ns:ns-string
                     (plus-sign nil plus-sign-p) ;; string or ns:ns-string
                     (positive-format nil positive-format-p) ;; string or ns:ns-string
                     (positive-infinity-symbol nil positive-infinity-symbol-p) ;; string or ns:ns-string
                     (positive-prefix nil positive-prefix-p) ;; string or ns:ns-string
                     (positive-suffix nil positive-suffix-p) ;; string or ns:ns-string
                     (rounding-behavior nil rounding-behavior-p) ;; ns:ns-decimal-number-handler
                     (rounding-increment nil rounding-increment-p) ;; number
                     (rounding-mode nil rounding-mode-p) ;; number or keyword: :ceiling :floor :down :up :half-even :half-down :half-up
                     (secondary-grouping-size nil secondary-grouping-size-p) ;; integer
                     (text-attributes-for-negative-infinity nil text-attributes-for-negative-infinity-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-negative-values nil text-attributes-for-negative-values-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-nil nil text-attributes-for-nil-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-not-a-number nil text-attributes-for-not-a-number-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-positive-infinity nil text-attributes-for-positive-infinity-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-positive-values nil text-attributes-for-positive-values-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (text-attributes-for-zero nil text-attributes-for-zero-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (thousand-separator nil thousand-separator-p) ;; string or ns:ns-string
                     (uses-grouping-separator nil uses-grouping-separator-p) 
                     (uses-significant-digits nil uses-significant-digits-p) 
                     (zero-symbol nil zero-symbol-p) ;; string or ns:ns-string
                     &allow-other-keys)
  ;; set basic style first, permitting modifications with other keywords
  (when number-style-p
    (#/setNumberStyle: self (convert-number-style number-style)))
  (when allows-floats-p
    (#/setAllowsFloats: self allows-floats))
  (when always-shows-decimal-separator-p
    (#/setAlwaysShowsDecimalSeparator: self always-shows-decimal-separator))
  (when attributed-string-for-nil-p
    (#/setAttributedStringForNil: self (coerce-obj attributed-string-for-nil 'ns:ns-attributed-string)))
  (when attributed-string-for-not-a-number-p
    (#/setAttributedStringForNotANumber: self (coerce-obj attributed-string-for-not-a-number 'ns:ns-attributed-string)))
  (when attributed-string-for-zero-p
    (#/setAttributedStringForZero: self (coerce-obj attributed-string-for-zero 'ns:ns-attributed-string)))
  (when currency-decimal-separator-p
    (#/setCurrencyDecimalSeparator: self (coerce-obj currency-decimal-separator 'ns:ns-string)))
  (when currency-grouping-separator-p
    (#/setCurrencyGroupingSeparator: self (coerce-obj currency-grouping-separator 'ns:ns-string)))
  (when currency-symbol-p
    (#/setCurrencySymbol: self (coerce-obj currency-symbol 'ns:ns-string)))
  (when decimal-separator-p
    (#/setDecimalSeparator: self (coerce-obj decimal-separator 'ns:ns-string)))
  (when exponent-symbol-p
    (#/setExponentSymbol: self (coerce-obj exponent-symbol 'ns:ns-string)))
  (when format-p
    (#/setFormat: self (coerce-obj format 'ns:ns-string)))
  (when format-width-p
    (#/setFormatWidth: self (coerce format-width 'integer)))
  (when generates-decimal-numbers-p
    (#/setGeneratesDecimalNumbers: self generates-decimal-numbers))
  (when grouping-separator-p
    (#/setGroupingSeparator: self (coerce-obj grouping-separator 'ns:ns-string)))
  (when grouping-size-p
    (#/setGroupingSize: self (coerce grouping-size 'integer)))
  (when has-thousand-separators-p
    (#/setHasThousandSeparators: self has-thousand-separators))
  (when international-currency-symbol-p
    (#/setInternationalCurrencySymbol: self (coerce-obj international-currency-symbol 'ns:ns-string)))
  (when lenient-p
    (#/setLenient: self lenient))
  (when locale-p
    (#/setLocale: self locale))
  (when localizes-format-p
    (#/setLocalizesFormat: self localizes-format))
  (when maximum-p
    (#/setMaximum: self (coerce-obj maximum 'ns:ns-number)))
  (when maximum-fraction-digits-p
    (#/setMaximumFractionDigits: self (coerce maximum-fraction-digits 'integer)))
  (when maximum-integer-digits-p
    (#/setMaximumIntegerDigits: self (coerce maximum-integer-digits 'integer)))
  (when maximum-significant-digits-p
    (#/setMaximumSignificantDigits: self (coerce maximum-significant-digits 'integer)))
  (when minimum-p
    (#/setMinimum: self  (coerce-obj minimum 'ns:ns-number)))
  (when minimum-fraction-digits-p
    (#/setMinimumFractionDigits: self (coerce minimum-fraction-digits 'integer)))
  (when minimum-integer-digits-p
    (#/setMinimumIntegerDigits: self (coerce minimum-integer-digits 'integer)))
  (when minimum-significant-digits-p
    (#/setMinimumSignificantDigits: self (coerce minimum-significant-digits 'integer)))
  (when minus-sign-p
    (#/setMinusSign: self (coerce-obj minus-sign 'ns:ns-string)))
  (when multiplier-p
    (#/setMultiplier: self  (coerce-obj multiplier 'ns:ns-number)))
  (when negative-format-p
    (#/setNegativeFormat: self (coerce-obj negative-format 'ns:ns-string)))
  (when negative-infinity-symbol-p
    (#/setNegativeInfinitySymbol: self (coerce-obj negative-infinity-symbol 'ns:ns-string)))
  (when negative-prefix-p
    (#/setNegativePrefix: self (coerce-obj negative-prefix 'ns:ns-string)))
  (when negative-suffix-p
    (#/setNegativeSuffix: self (coerce-obj negative-suffix 'ns:ns-string)))
  (when nil-symbol-p
    (#/setNilSymbol: self (coerce-obj nil-symbol 'ns:ns-string)))
  (when not-a-number-symbol-p
    (#/setNotANumberSymbol: self (coerce-obj not-a-number-symbol 'ns:ns-string)))
  (when padding-character-p
    (#/setPaddingCharacter: self (coerce-obj padding-character 'ns:ns-string)))
  (when padding-position-p
    (#/setPaddingPosition: self (convert-padding-position padding-position)))
  (when partial-string-validation-enabled-p
    (#/setPartialStringValidationEnabled: self partial-string-validation-enabled))
  (when percent-symbol-p
    (#/setPercentSymbol: self (coerce-obj percent-symbol 'ns:ns-string)))
  (when per-mill-symbol-p
    (#/setPerMillSymbol: self (coerce-obj per-mill-symbol 'ns:ns-string)))
  (when plus-sign-p
    (#/setPlusSign: self (coerce-obj plus-sign 'ns:ns-string)))
  (when positive-format-p
    (#/setPositiveFormat: self (coerce-obj positive-format 'ns:ns-string)))
  (when positive-infinity-symbol-p
    (#/setPositiveInfinitySymbol: self (coerce-obj positive-infinity-symbol 'ns:ns-string)))
  (when positive-prefix-p
    (#/setPositivePrefix: self (coerce-obj positive-prefix 'ns:ns-string)))
  (when positive-suffix-p
    (#/setPositiveSuffix: self (coerce-obj positive-suffix 'ns:ns-string)))
  (when rounding-behavior-p
    (#/setRoundingBehavior: self rounding-behavior))
  (when rounding-increment-p
    (#/setRoundingIncrement: self (coerce-obj rounding-increment 'ns:ns-number)))
  (when rounding-mode-p
    (#/setRoundingMode: self (convert-rounding-mode rounding-mode)))
  (when secondary-grouping-size-p
    (#/setSecondaryGroupingSize: self (coerce secondary-grouping-size 'integer)))
  (when text-attributes-for-negative-infinity-p
    (#/setTextAttributesForNegativeInfinity: self (coerce-obj text-attributes-for-negative-infinity 'ns:ns-dictionary)))
  (when text-attributes-for-negative-values-p
    (#/setTextAttributesForNegativeValues: self (coerce-obj text-attributes-for-negative-values 'ns:ns-dictionary)))
  (when text-attributes-for-nil-p
    (#/setTextAttributesForNil: self (coerce-obj text-attributes-for-nil 'ns:ns-dictionary)))
  (when text-attributes-for-not-a-number-p
    (#/setTextAttributesForNotANumber: self (coerce-obj text-attributes-for-not-a-number 'ns:ns-dictionary)))
  (when text-attributes-for-positive-infinity-p
    (#/setTextAttributesForPositiveInfinity: self (coerce-obj text-attributes-for-positive-infinity 'ns:ns-dictionary)))
  (when text-attributes-for-positive-values-p
    (#/setTextAttributesForPositiveValues: self (coerce-obj text-attributes-for-positive-values 'ns:ns-dictionary)))
  (when text-attributes-for-zero-p
    (#/setTextAttributesForZero: self (coerce-obj text-attributes-for-zero 'ns:ns-dictionary)))
  (when thousand-separator-p
    (#/setThousandSeparator: self (coerce-obj thousand-separator 'ns:ns-string)))
  (when uses-grouping-separator-p
    (#/setUsesGroupingSeparator: self uses-grouping-separator))
  (when uses-significant-digits-p
    (#/setUsesSignificantDigits: self uses-significant-digits))
  (when zero-symbol-p
    (#/setZeroSymbol: self (coerce-obj zero-symbol 'ns:ns-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-responder subclasses

(defmethod initialize-instance :after ((self ns:ns-view)
                     &key
                     (accepts-touch-events nil accepts-touch-events-p)
                     (alpha-value 0 alpha-value-p)
                     (animations nil animations-p)
                     (autoresizes-subviews nil autoresizes-subviews-p)
                     (autoresizing-mask 0 autoresizing-mask-p) ;; number or list of resizing keys: see convert-resizing-mask
                     (bounds 0 bounds-p) ;; ns:ns-rectangle or list: (x y width height)
                     (bounds-origin 0 bounds-origin-p) ;; ns:ns-point or list: (x y)
                     (bounds-rotation 0 bounds-rotation-p) ;; angle in degrees
                     (bounds-size 0 bounds-size-p) ;; ns:ns-size or list: (width height)
                     (can-draw-concurrently nil can-draw-concurrently-p)
                     (focus-ring-type nil focus-ring-type-p) ;; number or keyword: see convert-focus-ring-type
                     (frame nil frame-p) ;; ns:ns-rect
                     (frame-center-rotation nil frame-center-rotation-p) ;; angle in degrees
                     (frame-origin nil frame-origin-p) ;; ns:ns-point or list: (x y)
                     (frame-rotation nil frame-rotation-p) ;; angle in degrees
                     (frame-size nil frame-size-p) ;; ns:ns-size or list: (width height)
                     (hidden nil hidden-p)
                     (layer nil layer-p) ;; CALayer
                     (posts-bounds-changed-notifications nil posts-bounds-changed-notifications-p)
                     (posts-frame-changed-notifications nil posts-frame-changed-notifications-p)
                     (shadow nil shadow-p) ;; ns:ns-shadow
                     (subviews nil subviews-p) ;; list of subviews
                     (tool-tip "" tool-tip-p) ;; string
                     (wants-resting-touches nil wants-resting-touches-p)
                     &allow-other-keys)
  (when accepts-touch-events-p
    (#/setAcceptsTouchEvents: self accepts-touch-events))
  (when alpha-value-p
    (#/setAlphaValue: self (cgfloat alpha-value)))
  (when animations-p
    (#/setAnimations: self (coerce-obj animations 'ns:ns-dictionary)))
  (when autoresizes-subviews-p
    (#/setAutoresizesSubviews: self autoresizes-subviews))
  (when autoresizing-mask-p
    (#/setAutoresizingMask: self (convert-resizing-mask autoresizing-mask)))
  (when bounds-p
    (#/setBounds: self (coerce-obj bounds 'ns:ns-rect)))
  (when bounds-origin-p
    (#/setBoundsOrigin: self (coerce-obj bounds-origin 'ns:ns-point)))
  (when bounds-rotation-p
    (#/setBoundsRotation: self (cgfloat bounds-rotation)))
  (when bounds-size-p
    (#/setBoundsSize: self (coerce-obj bounds-size 'ns:ns-size)))
  (when can-draw-concurrently-p
    (#/setCanDrawConcurrently: self can-draw-concurrently))
  (when focus-ring-type-p
    (#/setFocusRingType: self (convert-focus-ring-type focus-ring-type)))
  (when frame-p
    (#/setFrame: self (coerce-obj frame 'ns:ns-rect)))
  (when frame-center-rotation-p
    (#/setFrameCenterRotation: self (cgfloat frame-center-rotation)))
  (when frame-origin-p
    (#/setFrameOrigin: self (coerce-obj frame-origin 'ns:ns-point)))
  (when frame-rotation-p
    (#/setFrameRotation: self (cgfloat frame-rotation)))
  (when frame-size-p
    (#/setFrameSize: self (coerce-obj frame-size 'ns:ns-size)))
  (when hidden-p
    (#/setHidden: self hidden))
  (when layer-p
    (#/setLayer: self layer))
  (when posts-bounds-changed-notifications-p
    (#/setPostsBoundsChangedNotifications: self posts-bounds-changed-notifications))
  (when posts-frame-changed-notifications-p
    (#/setPostsFrameChangedNotifications: self posts-frame-changed-notifications))
  (when shadow-p
    (#/setShadow: self shadow))
  (when subviews-p
    (#/setSubviews: self (coerce-obj subviews 'ns:ns-array)))
  (when tool-tip-p
    (#/setToolTip: self (coerce-obj tool-tip 'ns:ns-string)))
  (when wants-resting-touches-p
    (#/setWantsRestingTouches: self wants-resting-touches)))

(defmethod initialize-instance :after ((self ns:ns-window)
                     &key
                     (accepts-mouse-moved-events nil accepts-mouse-moved-events-p)
                     (allows-concurrent-view-drawing nil allows-concurrent-view-drawing-p)
                     (allows-tool-tips-when-application-is-inactive nil allows-tool-tips-when-application-is-inactive-p)
                     (alpha-value 0 alpha-value-p)
                     (animation-behavior nil animation-behavior-p) ;; number or keyword: see convert-animation-behavior
                     (aspect-ratio nil aspect-ratio-p) ;; ns:ns-size or list: (width height)
                     (autodisplay nil autodisplay-p)
                     (background-color nil background-color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     (can-become-visible-without-login nil can-become-visible-without-login-p)
                     (can-hide nil can-hide-p)
                     (collection-behavior nil collection-behavior-p) ;; number, keyword, or list of keywords: see convert-collection-bahavior
                     (color-space nil color-space-p) ;; ns:ns-color-space
                     (content-aspect-ratio nil content-aspect-ratio-p) ;; ns:ns-size or list: (width height)
                     (content-max-size nil content-max-size-p) ;; ns:ns-size or list: (width height)
                     (content-min-size nil content-min-size-p)
                     (content-resize-increments nil content-resize-increments-p) ;; ns:ns-size or list: (width height)
                     (content-size nil content-size-p) ;; ns:ns-size or list: (width height)
                     (content-view nil content-view-p) ;; ns:ns-view
                     (content-subviews nil content-subviews-p) ;; a list of subviews to add to the window's content view
                     (default-button-cell nil default-button-cell-p) ;; ns:ns-button-cell
                     (delegate nil delegate-p) ;; ns:ns-object
                     (depth-limit nil depth-limit-p) ;; number or keyword: :rgb-24, :rgb-64 or :rgb-128
                     (displays-when-screen-profile-changes nil displays-when-screen-profile-changes-p)
                     (dynamic-depth-limit nil dynamic-depth-limit-p) ;; t or nil
                     (excluded-from-windows-menu nil excluded-from-windows-menu-p)
                     (frame nil frame-p) ;; ns:ns-rect or 
                     (frame-autosave-name nil frame-autosave-name-p) ;; string or ns:ns-string
                     (frame-from-string nil frame-from-string-p) ;; string or ns:ns-string
                     (frame-origin nil frame-origin-p) ;; ns-point or list: (x y)
                     (frame-top-left-point nil frame-top-left-point-p) ;; ns-point or list: (x y)
                     (frame-using-name nil frame-using-name-p) ;; string or ns:ns-string
                     (has-shadow nil has-shadow-p)
                     (hides-on-deactivate nil hides-on-deactivate-p)
                     (ignores-mouse-events nil ignores-mouse-events-p)
                     (initial-first-responder nil initial-first-responder-p) ;; ns:ns-view
                     (level nil level-p) ;; number or keyword: see convert-window-level
                     (max-size nil max-size-p) ;; ns:ns-size or list: (width height)
                     (miniwindow-image nil miniwindow-image-p) ;; ns:ns-image
                     (miniwindow-title nil miniwindow-title-p) ;; string or ns:ns-string
                     (min-size nil min-size-p) ;; ns:ns-size or list: (width height)
                     (movable nil movable-p)
                     (movable-by-window-background nil movable-by-window-background-p)
                     (one-shot nil one-shot-p)
                     (opaque nil opaque-p)
                     (preferred-backing-location nil preferred-backing-location-p)  ;; number or keyword: :default :video-memory :main-memory
                     (preserves-content-during-live-resize nil preserves-content-during-live-resize-p)
                     (prevents-application-termination-when-modal nil prevents-application-termination-when-modal-p)
                     (released-when-closed nil released-when-closed-p)
                     (represented-filename nil represented-filename-p) ;; string or ns:ns-string
                     (represented-url nil represented-url-p) ;; ns:ns-url
                     (resize-increments nil resize-increments-p) ;; ns-size or list: (width height)
                     (restorable nil restorable-p)
                     (restoration-class nil restoration-class-p) ;; class that conforms to the NSWindowRestoration protocol
                     (sharing-type nil sharing-type-p) ;; number or keyword: see convert-sharing-type
                     (shows-resize-indicator nil shows-resize-indicator-p)
                     (shows-toolbar-button nil shows-toolbar-button-p)
                     (titled t titled-p)
                     (closable t closable-p)
                     (resizable t resizable-p)
                     (miniaturizable t miniaturizable-p)
                     (title nil title-p) ;; string or ns:ns-string
                     (title-with-represented-filename nil title-with-represented-filename-p) ;; string or ns:ns-string
                     (toolbar nil toolbar-p) ;; ns:ns-toolbar
                     (window-controller nil window-controller-p) ;; ns:ns-window-controller
                     &allow-other-keys)
  (when accepts-mouse-moved-events-p
    (#/setAcceptsMouseMovedEvents: self accepts-mouse-moved-events))
  (when allows-concurrent-view-drawing-p
    (#/setAllowsConcurrentViewDrawing: self allows-concurrent-view-drawing))
  (when allows-tool-tips-when-application-is-inactive-p
    (#/setAllowsToolTipsWhenApplicationIsInactive: self allows-tool-tips-when-application-is-inactive))
  (when alpha-value-p
    (#/setAlphaValue: self (cgfloat alpha-value)))
  (when animation-behavior-p
    (#/setAnimationBehavior: self (convert-animation-behavior animation-behavior)))
  (when aspect-ratio-p
    (#/setAspectRatio: self (coerce-obj aspect-ratio 'ns:ns-size)))
  (when autodisplay-p
    (#/setAutodisplay: self autodisplay))
  (when background-color-p
    (#/setBackgroundColor: self (coerce-obj background-color 'ns:ns-color)))
  (when can-become-visible-without-login-p
    (#/setCanBecomeVisibleWithoutLogin: self can-become-visible-without-login))
  (when can-hide-p
    (#/setCanHide: self can-hide))
  (when collection-behavior-p
    (#/setCollectionBehavior: self (convert-collection-behavior collection-behavior)))
  (when color-space-p
    (#/setColorSpace: self color-space))
  (when content-aspect-ratio-p
    (#/setContentAspectRatio: self (coerce-obj content-aspect-ratio 'ns:ns-size)))
  (when content-max-size-p
    (#/setContentMaxSize: self (coerce-obj content-max-size 'ns:ns-size)))
  (when content-min-size-p
    (#/setContentMinSize: self (coerce-obj content-min-size 'ns:ns-size)))
  (when content-resize-increments-p
    (#/setContentResizeIncrements: self (coerce-obj content-resize-increments 'ns:ns-size)))
  (when content-size-p
    (#/setContentSize: self (coerce-obj content-size 'ns:ns-size)))
  (when content-view-p
    (#/setContentView: self content-view))
  (when content-subviews-p
    (let ((cv (#/contentView self)))
      (dolist (sv content-subviews)
        (#/addSubview: cv sv))))
  (when default-button-cell-p
    (#/setDefaultButtonCell: self default-button-cell))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when depth-limit-p
    (#/setDepthLimit: self (convert-window-depth depth-limit)))
  (when displays-when-screen-profile-changes-p
    (#/setDisplaysWhenScreenProfileChanges: self displays-when-screen-profile-changes))
  (when dynamic-depth-limit-p
    (#/setDynamicDepthLimit: self dynamic-depth-limit))
  (when excluded-from-windows-menu-p
    (#/setExcludedFromWindowsMenu: self excluded-from-windows-menu))
  (when frame-p
    (#/setFrame:display: self (coerce-obj frame 'ns:ns-rect) #$NO))
  (when frame-autosave-name-p
    (#/setFrameAutosaveName: self (coerce-obj frame-autosave-name 'ns:ns-string)))
  (when frame-from-string-p
    (#/setFrameFromString: self (coerce-obj frame-from-string 'ns:ns-string)))
  (when frame-origin-p
    (#/setFrameOrigin: self (coerce-obj frame-origin 'ns:ns-point)))
  (when frame-top-left-point-p
    (#/setFrameTopLeftPoint: self (coerce-obj frame-top-left-point 'ns:ns-point)))
  (when frame-using-name-p
    (#/setFrameUsingName: self (coerce-obj frame-using-name 'ns:ns-string)))
  (when has-shadow-p
    (#/setHasShadow: self has-shadow))
  (when hides-on-deactivate-p
    (#/setHidesOnDeactivate: self hides-on-deactivate))
  (when ignores-mouse-events-p
    (#/setIgnoresMouseEvents: self ignores-mouse-events))
  (when initial-first-responder-p
    (#/setInitialFirstResponder: self initial-first-responder))
  (when level-p
    (#/setLevel: self (convert-window-level level)))
  (when max-size-p
    (#/setMaxSize: self (coerce-obj max-size 'ns:ns-size)))
  (when miniwindow-image-p
    (#/setMiniwindowImage: self miniwindow-image))
  (when miniwindow-title-p
    (#/setMiniwindowTitle: self (coerce-obj miniwindow-title 'ns:ns-string)))
  (when min-size-p
    (#/setMinSize: self (coerce-obj min-size 'ns:ns-size)))
  (when movable-p
    (#/setMovable: self movable))
  (when movable-by-window-background-p
    (#/setMovableByWindowBackground: self movable-by-window-background))
  (when one-shot-p
    (#/setOneShot: self one-shot))
  (when opaque-p
    (#/setOpaque: self opaque))
  (when preferred-backing-location-p
    (#/setPreferredBackingLocation: self (convert-window-backing-loc preferred-backing-location)))
  (when preserves-content-during-live-resize-p
    (#/setPreservesContentDuringLiveResize: self preserves-content-during-live-resize))
  (when prevents-application-termination-when-modal-p
    (#/setPreventsApplicationTerminationWhenModal: self prevents-application-termination-when-modal))
  (when released-when-closed-p
    (#/setReleasedWhenClosed: self released-when-closed))
  (when represented-filename-p
    (#/setRepresentedFilename: self (coerce-obj represented-filename 'ns:ns-string)))
  (when represented-url-p
    (#/setRepresentedURL: self represented-url))
  (when resize-increments-p
    (#/setResizeIncrements: self (coerce-obj resize-increments 'ns:ns-size)))
  (when restorable-p
    (#/setRestorable: self restorable))
  (when restoration-class-p
    (#/setRestorationClass: self restoration-class))
  (when sharing-type-p
    (#/setSharingType: self (convert-sharing-type sharing-type)))
  (when shows-resize-indicator-p
    (#/setShowsResizeIndicator: self shows-resize-indicator))
  (when (or titled-p closable-p resizable-p miniaturizable-p)
    (#/setStyleMask: self (+ (if titled #$NSTitledWindowMask 0)
                             (if closable #$NSClosableWindowMask 0)
                             (if resizable #$NSResizableWindowMask 0)
                             (if miniaturizable #$NSMiniaturizableWindowMask 0))))
  (when (and title-p title) 
    ;; need the title check for back compatibility with initialize-instance :after method
    ;; defined in cocoa-window.lisp which this will replace
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when title-with-represented-filename-p
    (#/setTitleWithRepresentedFilename: self  (coerce-obj title-with-represented-filename 'ns:ns-string)))
  (when toolbar-p
    (#/setToolbar: self toolbar))
  (when shows-toolbar-button-p
    (#/setShowsToolbarButton: self shows-toolbar-button))
  (when window-controller-p
    (#/setWindowController: self window-controller)))

(defmethod initialize-instance :after ((self ns:ns-window-controller)
                     &key
                     (window nil window-p)
                     &allow-other-keys)
  (when window-p
    (#/initWithWindow: self window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-view subclasses

(defmethod initialize-instance :after ((self ns:ns-box)
                     &key
                     (border-color nil border-color-p) ;; ns:ns-color or keyword
                     (border-type nil border-type-p) ;; number or keyword
                     (border-width nil border-width-p) ;; number
                     (box-type nil box-type-p) ;; number or keyword
                     (content-view nil content-view-p) ;; ns:ns-view
                     (content-view-margins nil content-view-margins-p) ;; ns:ns-size or list of the form (width height)
                     (corner-radius nil corner-radius-p) ;; number
                     (fill-color nil fill-color-p)  ;; ns:ns-color or keyword
                     (title nil title-p) ;; string, ns:ns-string
                     (title-font nil title-font-p)  ;; ns:ns-font
                     (title-position nil title-position-p)  ;; title-position keyword
                     (transparent nil transparent-p)
                     &allow-other-keys)
  (when border-color-p
    (#/setBorderColor: self (coerce-obj border-color 'ns:ns-color)))
  (when border-type-p
    (#/setBorderType: self (convert-border border-type)))
  (when border-width-p
    (#/setBorderWidth: self (cgfloat border-width)))
  (when box-type-p
    (#/setBoxType: self (convert-box-type box-type)))
  (when content-view-p
    (#/setContentView: self content-view))
  (when content-view-margins-p
    (#/setContentViewMargins: self (coerce-obj content-view-margins 'ns:ns-size))
    (#/sizeToFit self))
  (when corner-radius-p
    (#/setCornerRadius: self (cgfloat corner-radius)))
  (when fill-color-p
    (#/setFillColor: self (coerce-obj fill-color 'ns:ns-color)))
  (when title-p
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when title-font-p
    (#/setTitleFont: self title-font))
  (when title-position-p
    (#/setTitlePosition: self (convert-title-position title-position)))
  (when transparent-p
    (#/setTransparent: self transparent)))

(defmethod initialize-instance :after ((self ns:ns-combo-box)
                     &key
                     (button-bordered nil button-bordered-p)
                     (completes nil completes-p)
                     (data-source nil data-source-p) ;; object that conforms to the NSComboBoxDataSource protocol
                     (delegate nil delegate-p) ;; object that conforms to the NSComboBoxDelegate protocol
                     (has-vertical-scroller nil has-vertical-scroller-p)
                     (intercell-spacing nil intercell-spacing-p) ;; ns:ns-size or list of the form (width height)
                     (item-height nil item-height-p) ;; number coercable to a double-float
                     (items-with-object-values nil items-with-object-values-p) ;; list of strings or numbers
                     (number-of-visible-items nil number-of-visible-items-p) ;; integer number
                     (uses-data-source nil uses-data-source-p)
                     &allow-other-keys)
  (when button-bordered-p
    (#/setButtonBordered: self button-bordered))
  (when completes-p
    (#/setCompletes: self completes))
  (when uses-data-source-p ;; must come before data-source
    (#/setUsesDataSource: self uses-data-source))
  (when data-source-p
    (#/setDataSource: self data-source))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when has-vertical-scroller-p
    (#/setHasVerticalScroller: self has-vertical-scroller))
  (when intercell-spacing-p
    (#/setIntercellSpacing: self (coerce-obj intercell-spacing 'ns:ns-size)))
  (when item-height-p
    (#/setItemHeight: self (cgfloat item-height)))
  (when items-with-object-values-p
    (#/addItemsWithObjectValues: self (coerce-obj items-with-object-values 'ns:ns-array)))
  (when number-of-visible-items-p
    (#/setNumberOfVisibleItems: self (coerce number-of-visible-items 'integer))))

(defmethod initialize-instance :after ((self ns:ns-control)
                     &key
                     (action nil action-p) ;; string
                     (alignment :natural alignment-p) ;; number or keyword defined in convert-text-alignment
                     (allows-undo nil allows-undo-p)
                     (attributed-string-value "" attributed-string-value-p) ;; string or attributed-string
                     (base-writing-direction :natural base-writing-direction-p) ;; number or keyword from convert-writing-direction
                     (continuous nil continuous-p)
                     (initial-value nil initial-value-p) ;; numeric value
                     (enabled t enabled-p)
                     (font nil font-p) ;; ns:ns-font
                     (formatter nil formatter-p) ;; ns:ns-formatter, see formatters.lisp
                     (ignores-multi-click t ignores-multi-click-p)
                     (refuses-first-responder nil refuses-first-responder-p)
                     (tag 0 tag-p) ;; integer 
                     (target nil target-p) ;; ns:ns-object
                     &allow-other-keys) ;; see view.lisp for other allowed keywords
  (when action-p
    (#/setAction: self (get-selector action)))
  (when alignment-p
    (#/setAlignment: self (convert-text-alignment alignment)))
  (when allows-undo-p
    (#/setAllowsUndo: (#/cell self) allows-undo))
  (when attributed-string-value-p
    (#/setAttributedStringValue: self (coerce-obj attributed-string-value 'ns:ns-attributed-string)))
  (when base-writing-direction-p
    (#/setBaseWritingDirection: self (convert-writing-direction base-writing-direction)))
  (when continuous-p
    (#/setContinuous: self continuous))
  (when initial-value-p
    (cond ((floatp initial-value)
           (#/setFloatValue: self initial-value))
          ((integerp initial-value)
           (#/setIntegerValue: self initial-value))
          ((typep initial-value 'double-float)
           (#/setDoubleValue: self initial-value))
          ((numberp initial-value)
           (#/setFloatValue: self (float initial-value)))
          ((stringp initial-value)
           (#/setStringValue: self (coerce-obj initial-value 'ns:ns-string)))
          ((typep initial-value 'ns:ns-object)
           (#/setObjectValue: self initial-value))))
  (when enabled-p
    (#/setEnabled: self enabled))
  (when (and font-p (typep font 'ns:ns-font))
    (#/setFont: self font))
  (when (and formatter-p (typep formatter 'ns:ns-formatter))
    (#/setFormatter: self formatter))
  (when ignores-multi-click-p
    (#/setIgnoresMultiClick: self ignores-multi-click))
  (when refuses-first-responder-p
    (#/setRefusesFirstResponder: self refuses-first-responder))
  (when (and tag-p (numberp tag))
    (#/setTag: self (truncate tag)))
  (when (and target-p (typep target 'ns:ns-object))
    (#/setTarget: self target)))

(defmethod initialize-instance :after ((self ns:ns-scroll-view)
                     &key
                     (autohides-scrollers t autohides-scrollers-p)  ;; t or nil
                     (background-color nil background-color-p)      ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     (border-type nil border-type-p)                ;; number or keyword: see convert-border
                     (content-view nil content-view-p)
                     (document-cursor nil document-cursor-p)        ;; number or keyword: see convert-cursor
                     (document-view nil document-view-p)            ;; the view that will be scrolled
                     (draws-background nil draws-background-p)
                     (find-bar-position nil find-bar-position-p)    ;; number or keyword: see convert-find-bar-position
                     (has-horizontal-ruler nil has-horizontal-ruler-p) ;; t or nil
                     (has-horizontal-scroller nil has-horizontal-scroller-p) ;; t or nil
                     (has-vertical-ruler nil has-vertical-ruler-p)   ;; t or nil
                     (has-vertical-scroller nil has-vertical-scroller-p) ;; t or nil
                     (horizontal-line-scroll nil horizontal-line-scroll-p) ;; points to scroll
                     (horizontal-page-scroll nil horizontal-page-scroll-p) ;; points to scroll
                     (horizontal-ruler-view nil horizontal-ruler-view-p) ;; ns:ns-ruler-view
                     (horizontal-scroll-elasticity nil horizontal-scroll-elasticity-p) ;; number or keyword: see convert-elasticity
                     (horizontal-scroller nil horizontal-scroller-p)  ;; ns:ns-scroller
                     (line-scroll nil line-scroll-p)            ;; points to scroll in both h and v
                     (page-scroll nil page-scroll-p)            ;; points to scroll in both h and v
                     (rulers-visible nil rulers-visible-p)
                     (scroller-knob-style nil scroller-knob-style-p) ;; number or keyword: see convert-knob-style
                     (scroller-style nil scroller-style-p)  ;; number or keyword: see convert-scroller-style
                     (scrolls-dynamically t scrolls-dynamically-p) ;; t or nil whether to redraw while scrolling
                     (uses-predominant-axis-scrolling t uses-predominant-axis-scrolling-p) ;; t or nil
                     (vertical-line-scroll nil vertical-line-scroll-p)        ;; points to scroll
                     (vertical-page-scroll nil vertical-page-scroll-p)      ;; points to scroll  
                     (vertical-ruler-view nil vertical-ruler-view-p)   ;; ns:ns-ruler-view
                     (vertical-scroll-elasticity nil vertical-scroll-elasticity-p) ;; number or keyword: see convert-elasticity
                     (vertical-scroller nil vertical-scroller-p)      ;; ns:ns-scroller
                     &allow-other-keys)
  (when autohides-scrollers-p
    (#/setAutohidesScrollers: self autohides-scrollers))
  (when background-color-p
    (#/setBackgroundColor: self (coerce-obj background-color 'ns:ns-color)))   
  (when border-type-p
    (#/setBorderType: self (convert-border border-type)))
  (when content-view-p
    (#/setContentView: self content-view))
  (when document-cursor-p
    (#/setDocumentCursor: self (convert-cursor document-cursor)))
  (when document-view-p
    (#/setDocumentView: self document-view))
  (when draws-background-p
    (#/setDrawsBackground: self draws-background))
  (when find-bar-position-p
    (#/setFindBarPosition: self (convert-find-bar-position find-bar-position)))
  (when has-horizontal-ruler-p
    (#/setHasHorizontalRuler: self has-horizontal-ruler))
  (when has-horizontal-scroller-p
    (#/setHasHorizontalScroller: self has-horizontal-scroller))
  (when has-vertical-ruler-p
    (#/setHasVerticalRuler: self has-vertical-ruler))
  (when has-vertical-scroller-p
    (#/setHasVerticalScroller: self has-vertical-scroller))
  (when horizontal-line-scroll-p
    (#/setHorizontalLineScroll: self (cgfloat horizontal-line-scroll)))
  (when horizontal-page-scroll-p
    (#/setHorizontalPageScroll: self (cgfloat horizontal-page-scroll)))
  (when horizontal-ruler-view-p
    (#/setHorizontalRulerView: self horizontal-ruler-view))
  (when horizontal-scroll-elasticity-p
    (#/setHorizontalScrollElasticity: self (convert-elasticity horizontal-scroll-elasticity)))
  (when horizontal-scroller-p
    (#/setHorizontalScroller: self horizontal-scroller))
  (when line-scroll-p
    (#/setLineScroll: self (cgfloat line-scroll)))
  (when page-scroll-p
    (#/setPageScroll: self (cgfloat page-scroll)))
  (when rulers-visible-p
    (#/setRulersVisible: self rulers-visible))
  (when scroller-knob-style-p
    (#/setScrollerKnobStyle: self (convert-knob-style scroller-knob-style)))
  (when scroller-style-p
    (#/setScrollerStyle: self (convert-scroller-style scroller-style)))
  (when scrolls-dynamically-p
    (#/setScrollsDynamically: self scrolls-dynamically))
  (when uses-predominant-axis-scrolling-p
    (#/setUsesPredominantAxisScrolling: self uses-predominant-axis-scrolling))
  (when vertical-line-scroll-p
    (#/setVerticalLineScroll: self (cgfloat vertical-line-scroll)))
  (when vertical-page-scroll-p
    (#/setVerticalPageScroll: self (cgfloat vertical-page-scroll)))
  (when vertical-ruler-view-p
    (#/setVerticalRulerView: self vertical-ruler-view)) 
  (when vertical-scroll-elasticity-p
    (#/setVerticalScrollElasticity: self (convert-elasticity vertical-scroll-elasticity)))
  (when vertical-scroller-p
    (#/setVerticalScroller: self vertical-scroller))
  self)

(defmethod initialize-instance :after ((self ns:ns-text)
                     &key
                     (alignment nil alignment-p)
                     (background-color nil background-color-p) ;; ns:ns-color or keyword convertible by coerce-obj
                     (base-writing-direction nil base-writing-direction-p)  ;; number or keyword from convert-writing-direction
                     (delegate nil delegate-p) ;; any ns:ns-object
                     (draws-background nil draws-background-p)
                     (editable nil editable-p)
                     (field-editor nil field-editor-p)
                     (font nil font-p) ;; ns:ns-font
                     (horizontally-resizable nil horizontally-resizable-p)
                     (imports-graphics nil imports-graphics-p)
                     (max-size nil max-size-p) ;; ns:ns-size or list of the form (<width> <height>)
                     (min-size nil min-size-p) ;; ns:ns-size or list of the form (<width> <height>)
                     (rich-text nil rich-text-p)
                     (selectable nil selectable-p)
                     (string nil string-p) ;; string, attributed-string, or ns:ns-string
                     (text-color nil text-color-p) ;; ns:ns-color or keyword convertible by coerce-obj
                     (uses-font-panel nil uses-font-panel-p)
                     (vertically-resizable nil vertically-resizable-p)
                     &allow-other-keys)  ;; see control.lisp and view.lisp for other allowed keywords
  (when alignment-p
    (#/setAlignment: self (convert-text-alignment alignment)))
  (when background-color-p
    (#/setBackgroundColor: self (coerce-obj background-color 'ns:ns-color)))
  (when base-writing-direction-p
    (#/setBaseWritingDirection: self (convert-writing-direction base-writing-direction)))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when draws-background-p
    (#/setDrawsBackground: self draws-background))
  (when editable-p
    (#/setEditable: self editable))
  (when field-editor-p
    (#/setFieldEditor: self field-editor))
  (when font-p
    (#/setFont: self font))
  (when horizontally-resizable-p
    (#/setHorizontallyResizable: self horizontally-resizable))
  (when imports-graphics-p
    (#/setImportsGraphics: self imports-graphics))
  (when max-size-p
    (#/setMaxSize: self (coerce-obj max-size 'ns:ns-size)))
  (when min-size-p
    (#/setMinSize: self (coerce-obj min-size 'ns:ns-size)))
  (when rich-text-p
    (#/setRichText: self rich-text))
  (when selectable-p
    (#/setSelectable: self selectable))
  (when string-p
    (if rich-text-p
        (#/setAttributedString: (#/textStorage self) (coerce-obj string 'ns:ns-attributed-string))
        (#/setString: self (coerce-obj string 'ns:ns-string))))
  (when text-color-p
    (#/setTextColor: self (coerce-obj text-color 'ns:ns-color)))
  (when uses-font-panel-p
    (#/setUsesFontPanel: self uses-font-panel))
  (when vertically-resizable-p
    (#/setVerticallyResizable: self vertically-resizable)))

(defmethod initialize-instance :after ((self ns:ns-text-view)
                     &key
                     (delegate nil delegate-p) ;; ns:ns-object
                     (displayes-link-tool-tips nil displayes-link-tool-tips-p)
                     (draws-background nil draws-background-p)
                     (editable nil editable-p)
                     (enabled-text-checking-types nil enabled-text-checking-types-p) ;; keyword or list of keywords or number
                     (field-editor nil field-editor-p)
                     (grammar-checking-enabled nil grammar-checking-enabled-p)
                     (imports-graphics nil imports-graphics-p)
                     (incremental-searching-enabled nil incremental-searching-enabled-p)
                     (insertion-point-color nil insertion-point-color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     (layout-orientation nil layout-orientation-p) ;; number or keyword: :h :horizontal :v :vertical
                     (link-text-attributes nil link-text-attributes-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (marked-text-attributes nil marked-text-attributes-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (rich-text nil rich-text-p)
                     (ruler-visible nil ruler-visible-p)
                     (selectable nil selectable-p)
                     (selected-text-attributes nil selected-text-attributes-p) ;; assoc-list, hash-table, or ns:ns-dictionary of attribute-value pairs
                     (smart-insert-delete-enabled nil smart-insert-delete-enabled-p)
                     (text-container nil text-container-p) ;; ns:ns-text-container
                     (text-container-inset nil text-container-inset-p) ;; ns:ns-size or list: (width height)
                     (uses-find-bar nil uses-find-bar-p)
                     (uses-find-panel nil uses-find-panel-p)
                     (uses-font-panel nil uses-font-panel-p)
                     (uses-inspector-bar nil uses-inspector-bar-p)
                     (uses-ruler nil uses-ruler-p)
                     &allow-other-keys)  ;; see control.lisp and view.lisp for other allowed keywords
  (when delegate-p
    (#/setDelegate: self delegate))
  (when displayes-link-tool-tips-p
    (#/setDisplaysLinkToolTips: self displayes-link-tool-tips))
  (when draws-background-p
    (#/setDrawsBackground: self draws-background))
  (when editable-p
    (#/setEditable: self editable))
  (when enabled-text-checking-types-p
    (#/setEnabledTextCheckingTypes: self (convert-text-checking-types enabled-text-checking-types)))
  (when field-editor-p
    (#/setFieldEditor: self field-editor))
  (when grammar-checking-enabled-p
    (#/setGrammarCheckingEnabled: self grammar-checking-enabled))
  (when imports-graphics-p
    (#/setImportsGraphics: self imports-graphics))
  (when incremental-searching-enabled-p
    (#/setIncrementalSearchingEnabled: self incremental-searching-enabled))
  (when insertion-point-color-p
    (#/setInsertionPointColor: self (coerce-obj insertion-point-color 'ns:ns-color)))
  (when layout-orientation-p
    (#/setLayoutOrientation: self (convert-text-orientation layout-orientation)))
  (when link-text-attributes-p
    (#/setLinkTextAttributes: self (coerce-obj link-text-attributes 'ns:ns-dictionary)))
  (when marked-text-attributes-p
    (#/setMarkedTextAttributes: self (coerce-obj marked-text-attributes 'ns:ns-dictionary)))
  (when rich-text-p
    (#/setRichText: self rich-text))
  (when ruler-visible-p
    (#/setRulerVisible: self ruler-visible))
  (when selectable-p
    (#/setSelectable: self selectable))
  (when selected-text-attributes-p
    (#/setSelectedTextAttributes: self  (coerce-obj selected-text-attributes 'ns:ns-dictionary)))
  (when smart-insert-delete-enabled-p
    (#/setSmartInsertDeleteEnabled: self smart-insert-delete-enabled))
  (when text-container-p
    (#/setTextContainer: self text-container))
  (when text-container-inset-p
    (#/setTextContainerInset: self (coerce-obj text-container-inset 'ns:ns-size)))
  (when uses-find-bar-p
    (#/setUsesFindBar: self uses-find-bar))
  (when uses-find-panel-p
    (#/setUsesFindPanel: self uses-find-panel))
  (when uses-font-panel-p
    (#/setUsesFontPanel: self uses-font-panel))
  (when uses-inspector-bar-p
    (#/setUsesInspectorBar: self uses-inspector-bar))
  (when uses-ruler-p
    (#/setUsesRuler: self uses-ruler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-control subclasses

(defmethod initialize-instance :after ((self ns:ns-button)
                     &key
                     (allows-mixed-state nil allows-mixed-state-p)
                     (alternate-image nil alternate-image-p) ;; ns:ns-image
                     (alternate-title nil alternate-title-p) ;; string, attributed-string, ns:ns-string
                     (attributed-alternate-title nil attributed-alternate-title-p) ;; string, attributed-string or Obj-C equivalents
                     (attributed-title nil attributed-title-p) ;; string, attributed-string or Obj-C equivalents
                     (bezel-style nil bezel-style-p) ;; number or keyword: see convert-bezel-style
                     (bordered nil bordered-p)
                     (button-type nil button-type-p) ;; number or keyword: see convert-button-type
                     (image nil image-p) ;; ns:ns-image
                     (image-position nil image-position-p) ;; single value or list, values are numbers or keyword: see convert-image-position
                     (key-equivalent nil key-equivalent-p)
                     (key-equivalent-modifier-mask 0 key-equivalent-modifier-mask-p) ;; number or list of keywords or keyword: :control, :alternate, or :command
                     (shows-border-only-while-mouse-inside nil shows-border-only-while-mouse-inside-p)
                     (sound nil sound-p)
                     (state nil state-p) ;; number or keyword: :on, :off, or :mixed
                     (title nil title-p) ;; string, attributed-string or Obj-C equivalents
                     (transparent nil transparent-p)
                     &allow-other-keys)  ;; see control.lisp and view.lisp for other allowed keywords
  (when allows-mixed-state-p
    (#/setAllowsMixedState: self allows-mixed-state))
  (when alternate-image-p
    (#/setAlternateImage: self alternate-image))
  (when alternate-title-p
    (#/setAlternateTitle: self (coerce-obj alternate-title 'ns:ns-string)))
  (when attributed-alternate-title-p
    (#/setAttributedAlternateTitle: self (coerce-obj attributed-alternate-title 'ns:ns-attributed-string)))
  (when attributed-title-p
    (#/setAttributedTitle: self (coerce-obj attributed-title 'ns:ns-attributed-string)))
  (when bezel-style-p
    (#/setBezelStyle: self (convert-bezel-style bezel-style)))
  (when bordered-p
    (#/setBordered: self bordered))
  (when button-type-p
    (#/setButtonType: self (convert-button-type button-type)))
  (when image-p
    (#/setImage: self image))
  (when image-position-p
    (if (listp image-position)
      (dolist (ip image-position)
        (#/setImagePosition: self (convert-image-position ip)))
      (#/setImagePosition: self (convert-image-position image-position))))
  (when key-equivalent-p
    (#/setKeyEquivalent: self (coerce-obj key-equivalent 'ns:ns-string)))
  (when key-equivalent-modifier-mask-p
    (#/setKeyEquivalentModifierMask: self (convert-key-modifier-mask key-equivalent-modifier-mask)))
  (when shows-border-only-while-mouse-inside-p
    (#/setShowsBorderOnlyWhileMouseInside: self shows-border-only-while-mouse-inside))
  (when (and sound-p (typep sound 'ns:ns-sound))
    (#/setSound: self sound))
  (when state-p
    (#/setState: self (convert-state state)))
  (when title-p
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when transparent-p
    (#/setTransparent: self transparent))
  self)

(defmethod initialize-instance :after ((self ns:ns-color-well)
                     &key
                     (bordered nil bordered-p)
                     (color nil color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     &allow-other-keys)
  (when bordered-p
    (#/setBordered: self bordered))
  (when color-p
    (#/setColor: self (coerce-obj color 'ns:ns-color))))
                     

(defmethod initialize-instance :after ((self ns:ns-outline-view)
                     &key
                     (autoresizes-outline-column nil autoresizes-outline-column-p)
                     (autosave-expanded-items nil autosave-expanded-items-p)
                     (indentation-marker-follows-cell nil indentation-marker-follows-cell-p)
                     (indentation-per-level nil indentation-per-level-p)
                     (outline-table-column nil outline-table-column-p)  ;; ns:ns-table-column
                     &allow-other-keys)
  (when autoresizes-outline-column-p
    (#/setAutoresizesOutlineColumn: self autoresizes-outline-column))
  (when autosave-expanded-items-p
    (#/setAutosaveExpandedItems: self autosave-expanded-items))
  (when indentation-marker-follows-cell-p
    (#/setIndentationMarkerFollowsCell: self indentation-marker-follows-cell))
  (when indentation-per-level-p
    (#/setIndentationPerLevel: self (cgfloat indentation-per-level)))
  (when outline-table-column-p
    (#/setOutlineTableColumn: self outline-table-column)))
      
(defmethod initialize-instance :after ((self ns:ns-slider)
                     &key
                     (allows-tick-mark-values-only nil allows-tick-mark-values-only-p)
                     (alt-increment-value 1 alt-increment-value-p)
                     (image nil image-p) ;; ns:ns-image
                     (max-value nil max-value-p)
                     (min-value nil min-value-p)
                     (number-of-tick-marks 0 number-of-tick-marks-p) ;; integer
                     &allow-other-keys)  ;; see control.lisp and view.lisp for other allowed keywords
  (when allows-tick-mark-values-only-p
    (#/setAllowsTickMarkValuesOnly: self allows-tick-mark-values-only))
  (when (and alt-increment-value-p (numberp alt-increment-value))
    (#/setAltIncrementValue: self (cgfloat alt-increment-value)))
  (when image-p
    (#/setImage: self image))
  (when (and max-value-p (numberp max-value))
    (#/setMaxValue: self (cgfloat max-value)))
  (when (and min-value-p (numberp min-value))
   (#/setMinValue: self (cgfloat min-value)))
  (when (and number-of-tick-marks-p (numberp number-of-tick-marks))
    (#/setNumberOfTickMarks: self (truncate number-of-tick-marks)))
  self)

(defmethod initialize-instance :after ((self ns:ns-table-view)
                     &key
                     (columns nil columns-p) ;; list of ns:ns-table-columns
                     (allows-column-reordering nil allows-column-reordering-p)
                     (allows-column-resizing nil allows-column-resizing-p)
                     (allows-column-selection nil allows-column-selection-p)
                     (allows-empty-selection nil allows-empty-selection-p)
                     (allows-multiple-selection nil allows-multiple-selection-p)
                     (allows-type-select nil allows-type-select-p)
                     (autosave-name nil autosave-name-p) ;; string or ns:ns-string
                     (autosave-table-columns nil autosave-table-columns-p)
                     (background-color nil background-color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     (column-autoresizing-style nil column-autoresizing-style-p) ;; number or keyword: see convert-table-col-resizing
                     (corner-view nil corner-view-p) ;; ns:ns-view
                     (data-source nil data-source-p) ;; ns:ns-object which conforms to NSTableViewDataSource protocol
                     (delegate nil delegate-p) ;; ns:ns-object which conforms to NSTableViewDelegate protocol
                     (double-action nil double-action-p) ;; string
                     (dragging-destination-feedback-style nil dragging-destination-feedback-style-p) ;; number or :none :regular :source-list
                     (dragging-source-operation-mask nil dragging-source-operation-mask-p) ;; number or keyword: see convert-table-drag-src-op
                     (floats-group-rows nil floats-group-rows-p)
                     (grid-color nil grid-color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     (grid-style-mask nil grid-style-mask-p) ;; number or keyword or list of keywords: see convert-grid-style
                     (header-view nil header-view-p) ;; ns:ns-table-header-view
                     (intercell-spacing nil intercell-spacing-p) ;; ns:ns-size or list: (width height)
                     (row-height nil row-height-p) ;; number
                     (row-size-style nil row-size-style-p) ;; number or keyword: see convert-row-size-style
                     (selection-highlight-style nil selection-highlight-style-p) ;; number or keyword: see convert-selection-highlight-style
                     (sort-descriptors nil sort-descriptors-p) ;; list, 1D array, or ns:ns-array of ns:ns-sort-descriptor objects
                     (uses-alternating-row-background-colors nil uses-alternating-row-background-colors-p)
                     (vertical-motion-can-begin-drag nil vertical-motion-can-begin-drag-p)
                     &allow-other-keys)
  (when columns-p
    ;; must do this first because other parameters only affect columns that already exist
    (dolist (col columns)
      (#/addTableColumn: self col)))
  (when allows-column-reordering-p
    (#/setAllowsColumnReordering: self allows-column-reordering))
  (when allows-column-resizing-p
    (#/setAllowsColumnResizing: self allows-column-resizing))
  (when allows-column-selection-p
    (#/setAllowsColumnSelection: self allows-column-selection))
  (when allows-empty-selection-p
    (#/setAllowsEmptySelection: self allows-empty-selection))
  (when allows-multiple-selection-p
    (#/setAllowsMultipleSelection: self allows-multiple-selection))
  (when allows-type-select-p
    (#/setAllowsTypeSelect: self allows-type-select))
  (when autosave-name-p
    (#/setAutosaveName: self (coerce-obj autosave-name 'ns:ns-string)))
  (when autosave-table-columns-p
    (#/setAutosaveTableColumns: self autosave-table-columns))
  (when background-color-p
    (#/setBackgroundColor: self (coerce-obj background-color 'ns:ns-color)))
  (when column-autoresizing-style-p
    (#/setColumnAutoresizingStyle: self (convert-table-col-resizing column-autoresizing-style)))
  (when corner-view-p
    (#/setCornerView: self corner-view))
  (when data-source-p
    (#/setDataSource: self data-source))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when double-action-p
    (#/setDoubleAction: self (get-selector double-action)))
  (when dragging-destination-feedback-style-p
    (#/setDraggingDestinationFeedbackStyle: self (convert-table-drag-feedback-style dragging-destination-feedback-style)))
  (when dragging-source-operation-mask-p
    (#/setDraggingSourceOperationMask:forLocal: self (convert-table-drag-src-op dragging-source-operation-mask) t))
  (when floats-group-rows-p
    (#/setFloatsGroupRows: self floats-group-rows))
  (when grid-color-p
    (#/setGridColor: self (coerce-obj grid-color 'ns:ns-color)))
  (when grid-style-mask-p
    (#/setGridStyleMask: self (convert-grid-styles grid-style-mask)))
  (when header-view-p
    (#/setHeaderView: self header-view))
  (when intercell-spacing-p
    (#/setIntercellSpacing: self (coerce-obj intercell-spacing 'ns:ns-size)))
  (when row-height-p
    (#/setRowHeight: self (cgfloat row-height)))
  (when row-size-style-p
    (#/setRowSizeStyle: self (convert-row-size-style row-size-style)))
  (when selection-highlight-style-p
    (#/setSelectionHighlightStyle: self (convert-selection-highlight-style selection-highlight-style)))
  (when sort-descriptors-p
    (#/setSortDescriptors: self (coerce-obj sort-descriptors 'ns:ns-array)))
  (when uses-alternating-row-background-colors-p
    (#/setUsesAlternatingRowBackgroundColors: self uses-alternating-row-background-colors))
  (when vertical-motion-can-begin-drag-p
    (#/setVerticalMotionCanBeginDrag: self vertical-motion-can-begin-drag)))

(defmethod initialize-instance :after ((self ns:ns-text-field)
                     &key
                     (bezeled nil bezeled-p)
                     (bezel-style nil bezel-style-p) ;; number or keyword: see values in convert-text-bezel-style above
                     (bordered nil bordered-p)
                     (delegate nil delegate-p) ;; ns:ns-object
                     (draws-background nil draws-background-p)
                     (editable nil editable-p)
                     (imports-graphics nil imports-graphics-p)
                     (preferred-max-layout-width nil preferred-max-layout-width-p)
                     (selectable nil selectable-p)
                     (text-color nil text-color-p) ;; ns:ns-color or keyword acceptable to (coerce-obj <keyword> 'ns:ns-color)
                     &allow-other-keys)  ;; see control.lisp and view.lisp for other allowed keywords
  (when bezeled-p
    (#/setBezeled: self bezeled))
  (when bezel-style-p
    (#/setBezelStyle: self  (convert-text-bezel-style bezel-style)))
  (when bordered-p
    (#/setBordered: self bordered))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when draws-background-p
    (#/setDrawsBackground: self draws-background))
  (when editable-p
    (#/setEditable: self editable))
  (when imports-graphics-p
    (#/setImportsGraphics: self imports-graphics))
  (when preferred-max-layout-width-p
    (#/setPreferredMaxLayoutWidth: self preferred-max-layout-width))
  (when selectable-p
    (#/setSelectable: self selectable))
  (when text-color-p
    (#/setTextColor: self (coerce-obj text-color 'ns:ns-color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ns-window subclasses

(defmethod initialize-instance :after ((self ns:ns-panel)
                     &key
                     (floating-panel nil floating-panel-p)
                     (works-when-modal nil works-when-modal-p)
                     &allow-other-keys)  ;; see window.lisp for other allowed keywords
  (when floating-panel-p
    (#/setFloatingPanel: self floating-panel))
  (when works-when-modal-p
    (#/setWorksWhenModal: self works-when-modal)))

(defmethod initialize-instance :after ((self ns:ns-save-panel)
                     &key
                     (accessory-view nil accessory-view-p)  ;; ns-view object
                     (allowed-file-types (%null-ptr)) ;; list of strings of allowed file types
                     (allows-other-file-types nil allows-other-file-types-p)
                     (can-create-directories nil can-create-directories-p)
                     (can-select-hidden-extension nil can-select-hidden-extension-p)
                     (delegate nil delegate-p) ;; NSOpenSavePanelDelegate object
                     (directory-url nil directory-url-p)  ;; ns:ns-url
                     (extension-hidden nil extension-hidden-p)
                     (message nil message-p)
                     (name-field-label nil name-field-label-p)
                     (name-field-string-value nil name-field-string-value-p)
                     (prompt nil prompt-p)
                     (shows-hidden-files nil shows-hidden-files-p)
                     ;;(shows-tag-field nil shows-tag-field-p)
                     ;;(tag-names nil tag-names-p)  ;; list of strings 
                     (title nil title-p) ;; string
                     (treats-file-packages-as-directories nil treats-file-packages-as-directories-p)
                     &allow-other-keys)  ;; see window.lisp for other allowed keywords
  (when accessory-view-p
    (#/setAccessoryView: self accessory-view))
  (#/setAllowedFileTypes: self (if allowed-file-types
                                   (coerce-obj allowed-file-types 'ns:ns-array)
                                   (%null-ptr)))
  (when allows-other-file-types-p
    (#/setAllowsOtherFileTypes: self allows-other-file-types))
  (when can-create-directories-p
    (#/setCanCreateDirectories: self can-create-directories))
  (when can-select-hidden-extension-p
    (#/setCanSelectHiddenExtension: self can-select-hidden-extension))
  (when delegate-p
    (#/setDelegate: self delegate))
  (when directory-url-p
    (#/setDirectoryURL: self directory-url))
  (when extension-hidden-p
    (#/setExtensionHidden: self extension-hidden))
  (when message-p
    (#/setMessage: self (coerce-obj message 'ns:ns-string)))
  (when name-field-label-p
    (#/setNameFieldLabel: self (coerce-obj name-field-label 'ns:ns-string)))
  (when name-field-string-value-p
    (#/setNameFieldStringValue: self (coerce-obj name-field-string-value 'ns:ns-string)))
  (when prompt-p
    (#/setPrompt: self (coerce-obj prompt 'ns:ns-string)))
  (when shows-hidden-files-p
    (#/setShowsHiddenFiles: self shows-hidden-files))
  ;;(when shows-tag-field-p
  ;;  (#/setShowsTagField: self shows-tag-field))
  ;;(when tag-names-p
  ;;  (#/setTagNames: self (coerce-obj tag-names 'ns:ns-array)))
  (when title-p
    (#/setTitle: self (coerce-obj title 'ns:ns-string)))
  (when treats-file-packages-as-directories-p
    (#/setTreatsFilePackagesAsDirectories: self treats-file-packages-as-directories)))


;;; NOTE: you should not create an ns-open-panel using make-instance. Instead use make-open-panel
;;; loaded from open-panel.lisp. That will avoid getting a rash of CG trace messages logged to the
;;; system log (I found this the hard way). Retaining an ns-open-panel in any fashion will cause
;;; the log messages.
(defmethod initialize-instance :after ((self ns:ns-open-panel)
                     &key
                     (allows-multiple-selection nil allows-multiple-selection-p)
                     (can-choose-directories nil can-choose-directories-p)
                     (can-choose-files nil can-choose-files-p)
                     (resolves-aliases nil resolves-aliases-p)
                     &allow-other-keys)
  (when allows-multiple-selection-p
    (#/setAllowsMultipleSelection: self allows-multiple-selection))
  (when can-choose-directories-p
    (#/setCanChooseDirectories: self can-choose-directories))
  (when can-choose-files-p
    (#/setCanChooseFiles: self can-choose-files))
  (when resolves-aliases-p
    (#/setResolvesAliases: self resolves-aliases)))

(provide :objc-initialize)