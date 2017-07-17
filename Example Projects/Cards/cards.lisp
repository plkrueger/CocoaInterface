;; cards.lisp

;; Test window that shows an assoc-array using an NSOutlineView to show
;; each level of the array (which are nested hash-tables).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :demo-packages)
  (require :lisp-controller)
  (require :ns-string-utils)
  (require :list-utils)
  (require :assoc-array)
  (require :constraint-layout)
  (require :window-controller))

(in-package :cards)

(defclass hand-of-cards (lisp-window-controller)
  ((lisp-ctrl :accessor lisp-ctrl))
  (:metaclass ns:+ns-object))

(Objc:defmethod (#/deal: :void)
                ((self hand-of-cards) (sender :id))
  (declare (ignore sender))
  (unless (eql (lisp-ctrl self) (%null-ptr))
    (setf (root (lisp-ctrl self)) (deal-cards))))

(defun first-ht (aa)
  ;; hand will be an assoc-array
  (iu::index1-ht aa))

(defmethod make-deal-window ((hand hand-of-cards))
  (let* ((lc (make-instance 'lisp-controller
               :col-ids (list "PosSuit" "Cards")
               :root-child-key #'first-ht
               :child-key-0 #'ht-value
               :sort-key #'ht-key
               :sort-pred #'hand-suit-order
               :col-keys-0 (list #'ht-key)
               :col-keys-1 (list #'ht-key #'(lambda (hte)
                                              (sorted-by-rank (ht-value hte))))))
         (deal-button (make-instance ns:ns-button
                        :title "New Deal"
                        :action "deal:"
                        :target hand
                        :bezel-style :round-rect
                        :button-type :momentary-light))
         (hand-col (#/autorelease (make-instance ns:ns-table-column
                                     :column-title "Position and Suit"
                                     :identifier "PosSuit"
                                     :min-width 60
                                     :editable nil
                                     :selectable nil)))
         (suit-col (#/autorelease (make-instance ns:ns-table-column
                                     :column-title "Cards"
                                     :identifier "Cards"
                                     :min-width 60
                                     :editable nil
                                     :selectable nil)))
         (tv (make-instance 'ns:ns-outline-view
               :columns (list hand-col suit-col)
               :data-source lc
               :delegate lc
               :allows-column-resizing nil
               :column-autoresizing-style :uniform
               :outline-table-column hand-col))
         (sv (make-instance ns:ns-scroll-view
               :autohides-scrollers t
               :has-horizontal-scroller t
               :has-vertical-scroller t
               :document-view tv))
         (win (make-instance 'ns:ns-window
                :title "Deal It!"
                :resizable t
                :content-subviews (list sv deal-button)))
         (cview (#/contentView win)))

    ;; set the lisp-ctrl field in the window controller
    (setf (lisp-ctrl hand) lc)

    ;; set the view for the lisp-controller
    (setf (view lc) tv)

    (anchor cview sv (list :leading :top :trailing))
    (order-views :orientation :v
                 :views (list sv deal-button :margin)
                 :align :leading)

    ;; Return the window, the list of objects created
    (values win (list lc deal-button tv sv win))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility Functions for hands
;;
;; A hand is represented as a bit vector that is 52 bits long. Cards in the hand
;; have a corresponding bit value of 1. Cards not in the hand have a corresponding
;; bit of 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype rank-list ()
  '(satisfies all-ranks))

(defconstant *aces* #*1000000000000100000000000010000000000001000000000000)
(defconstant *kings* #*0100000000000010000000000001000000000000100000000000)
(defconstant *queens* #*0010000000000001000000000000100000000000010000000000)
(defconstant *jacks* #*0001000000000000100000000000010000000000001000000000)
(defconstant *spades* #*1111111111111000000000000000000000000000000000000000)
(defconstant *hearts* #*0000000000000111111111111100000000000000000000000000)
(defconstant *diamonds* #*0000000000000000000000000011111111111110000000000000)
(defconstant *clubs* #*0000000000000000000000000000000000000001111111111111)
(defconstant *card-ranks* '("A" "K" "Q" "J" "10" "9" "8" "7" "6" "5" "4" "3" "2"))
(defconstant *card-suits* '("Spades" "Hearts" "Diamonds" "Clubs"))
(defconstant *hand-suits* '("Spades" "Hearts" "Diamonds" "Clubs" "North" "East" "South" "West"))

(defun full-deck ()
  (make-array '(52) :element-type 'bit :initial-element 1))

(defun card-rank (card)
  ;; card is a bit index
  (nth (mod card 13) *card-ranks*))

(defun all-ranks (rank-list)
  (and (listp rank-list)
       (null (set-difference rank-list *card-ranks* :test #'string=))))

(defun hand-suit-order (a b)
  (< (position a *hand-suits* :test #'string=) (position b *hand-suits* :test #'string=)))

(defun higher-rank (r1 r2)
  (< (position r1 *card-ranks* :test #'string=) (position r2 *card-ranks* :test #'string=)))

(defun sorted-by-rank (rlist)
  (when (typep rlist 'rank-list)
    (format nil "~{~a~^, ~}" (sort-list-in-place rlist #'higher-rank))))

(defun card-suit (card)
  ;; card is a bit index
  (nth (floor card 13) *card-suits*))

(defun pick-random-card (deck)
  ;; returns a card index
  (let* ((cnt (count 1 deck))
         (card (1+ (random cnt))))
    (position-if #'(lambda (bit)
                     (when (plusp bit)
                       (decf card))
                     (zerop card))
                 deck)))

(defun add-card (deal hand card)
  (setf (assoc-aref deal hand (card-suit card))
        (cons (card-rank card) (assoc-aref deal hand (card-suit card)))))

(defun remove-card (card deck)
  ;; card is a bit index
  (setf (aref deck card) 0))

(defun deal-cards ()
  ;; randomizes and returns four unique hands
  (let ((deck (full-deck))
        (deal (make-instance 'assoc-array :rank 2))
        (card nil))
    (dotimes (i 13)
      (setf card (pick-random-card deck))
      (add-card deal "West" card)
      (remove-card card deck)
      (setf card (pick-random-card deck))
      (add-card deal "North" card)
      (remove-card card deck)
      (setf card (pick-random-card deck))
      (add-card deal "East" card)
      (remove-card card deck)
      (setf card (pick-random-card deck))
      (add-card deal "South" card)
      (remove-card card deck))
    deal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Function

(defun test-deal ()
  (on-main-thread
   (let ((wc (make-instance 'hand-of-cards
               :build-method #'make-deal-window)))
     (show-window wc)
     wc)))

(provide :cards)