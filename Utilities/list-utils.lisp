;; list-utils.lisp

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
  (require :interface-packages))

(in-package :iu)

(defun add-to-list-at (lst indx value)
  ;; splices value into lst at the designated indx
  ;; if indx = 0 we make sure that the lst pointer remains
  ;; valid.
  (cond ((null lst)
         (setf lst (list value)))
        ((>= indx (list-length lst))
         (nconc lst (list value)))
        (t
         (let* ((nth-ptr (nthcdr indx lst))
                (new-cons (cons (car nth-ptr) (cdr nth-ptr))))
           ;; splice in the new value at the position of the specified indx
           (setf (car nth-ptr) value)
           (setf (cdr nth-ptr) new-cons))))
  lst)

(defun delete-from-list (lst thing)
  (cond ((eql 1 (list-length lst))
         nil)
        ((eq thing (first lst))
         (setf (car lst) (cadr lst))
         (setf (cdr lst) (cddr lst))
         (if (and (null (car lst)) (null (cdr lst)))
           nil
           lst))
        (t 
         (delete thing lst))))

(defun find-cdr (lst cdr-thing)
  ;; return the cons cell with a cdr eq to cdr-thing
  (do* ((cons-cell lst (cdr cons-cell)))
       ((or (null cons-cell) (eq (cdr cons-cell) cdr-thing)) cons-cell)))

(defun sort-list-in-place (lst sort-pred &optional (sort-key nil))
  ;; Sorting a list normally results in a reordering of the cons
  ;; cells. We swap the contents of cons cells around after sorting 
  ;; so that previous references to the list are still valid.
  ;; That way we can sort "in place" without having to copy the whole list.
  (let ((old-first-cons lst)
        (new-first-cons nil))
    (if sort-key
      (setf new-first-cons (sort lst sort-pred :key sort-key))
      (setf new-first-cons (sort lst sort-pred)))
    (let* ((prev-cons (find-cdr new-first-cons old-first-cons))
           (new-first-car (car new-first-cons))
           (new-first-cdr (cdr new-first-cons)))
      (when prev-cons
        ;; exchange the two cons cells: the one that used to 
        ;; be the first (called the old-first-cons) with the 
        ;; first in the sorted list (called the new-first-cons)
        ;; first exchange cars
        (setf (car new-first-cons) (car old-first-cons))
        (setf (car old-first-cons) new-first-car)
        ;; always set cdr of new-first-cons to the cdr of the old-first-cons 
        (setf (cdr new-first-cons) (cdr old-first-cons))  
        ;; have to be more careful about other cdr pointers
        (if (eq prev-cons new-first-cons)
          (setf (cdr old-first-cons) new-first-cons)
          (progn
            (setf (cdr prev-cons) new-first-cons)
            (setf (cdr old-first-cons) new-first-cdr)))))
    lst))

(defun sequential-sublists (lst n &optional (fill-last nil fill-last-p))
  ;; returns a list of sublists of lst, where all but potentially the last
  ;; contains n elements. The first n elements are in the first sublist,
  ;; the next n in the second sublist, etc.
  ;; if fill-last is provided, then all sublists returned will contain
  ;; the same number of elements with the last filled with whatever
  ;; is provided as the fill-last argument.
  (let* ((lngth (list-length lst))
         (padding-needed (if fill-last-p
                           (mod (- n (mod lngth n)) n)
                           0))
         (padded-list (make-list padding-needed :initial-element fill-last))
         (lsts (list (append (copy-list lst) padded-list))))
    (do* ((indx (1- n))
          (subl (first lsts))
          (ncdr (nthcdr indx subl)
                (nthcdr indx subl))
          (rest-subl (cdr ncdr)
                     (cdr ncdr)))
         ((null ncdr) lsts)
      (setf (cdr ncdr) nil)
      (setf (cdr (last lsts)) (if rest-subl (list rest-subl)))
      (setf subl rest-subl))))

(defun every-n-sublists (lst n)
  ;; Returns a list of sublists of lst, where all but potentially the last
  ;; contains n elements. Every nth element starting with the first element
  ;; are in the first sublist, Every nth element starting with the second
  ;; element are in the second sublist, etc.

  (let ((lsts (make-list n :initial-element nil))
        (i 0))
    (dolist (item lst)
      (push item (nth (mod i n) lsts))
      (incf i))
    (mapcar #'nreverse lsts)))

(defun interleave (&rest lsts)
  ;; interleaves elements from lsts and returns a single list
  ;; interleaving stops when the end of the shortest list is
  ;; reached.
  (apply #'mapcan #'list lsts))

(defun columnize (lst col-count)
  ;; returns a list with the elements of lst reordered so that if they are
  ;; printed in row column order the lst elements would be displayed down the
  ;; each column and then to the next.
  (apply #'interleave (sequential-sublists lst (ceiling (list-length lst) col-count) nil)))

(defun tree-nth (lst &rest coords)
  (do* ((tree lst
              elt)
        (coord-list coords
                    (rest coord-list))
        (coord (first coord-list)
               (first coord-list))
        (elt (nth coord tree)
             (nth coord tree)))
       ((null coord) tree)))

(provide :list-utils)
