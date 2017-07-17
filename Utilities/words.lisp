;; delim-words.lisp

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

;; Parses a string into a set of "words" delimited by any of a number of characters
;; specified. List will return either just words or words interspersed with the 
;; delimiter character found.

(defconstant $default-delims #(#\space #\return #\newline #\linefeed #\. #\, #\: #\; #\! #\?))

(defun words (str &key 
                  (delims $default-delims)
                  ;; delims can be any sequence of characters
                  ;; or a function that takes a char and returns t if it is a delimeter
                  (backslash-escape nil)
                  (include-delims nil))
  (do* ((word-list nil)
        (s (string str))
        (last-pos (1- (length s)))
        (next-word "")
        (word-start 0)
        (prev-was-escape nil)
        (pos 0 (1+ pos)))
       ((> pos last-pos) (delete "" (nreverse word-list) :test #'string=))
    (cond ((and backslash-escape prev-was-escape)
           (setf prev-was-escape nil))
          ((and backslash-escape (char= (elt str pos) #\\))
           (setf next-word (concatenate 'string next-word (subseq str word-start pos)))
           (setf word-start (1+ pos))
           (setf prev-was-escape t))
          ((functionp delims) 
           (when (funcall delims (elt str pos))
             (push (concatenate 'string next-word (subseq str word-start pos))
                   word-list)
             (when include-delims
               (push (elt str pos) word-list))
             (setf next-word "")
             (setf word-start (1+ pos))))
          ((find (elt str pos) delims)
           (push (concatenate 'string next-word (subseq str word-start pos))
                 word-list)
           (when include-delims
             (push (elt str pos) word-list))
           (setf next-word "")
           (setf word-start (1+ pos)))
          ((eql pos last-pos)
           (push (concatenate 'string next-word (subseq str word-start))
                 word-list))
          (t
           ;; nothing to do
           ))))

(defun replace-substr (str old-substr new-substr)
  ;; returns a new sequence composed by replacing all instances of old-substr
  ;; by new-substr. The two substrings need not be the same length
  (do* ((new-subseqs nil)
        (old-len (length old-substr))
        (start 0
               (+ repl-start old-len))
        (repl-start (search old-substr str :start2 start)
                    (search old-substr str :start2 start))
        (kept-str (if repl-start
                    (subseq str start repl-start)
                    (subseq str start))
                  (if repl-start
                    (subseq str start repl-start)
                    (subseq str start))))
       ((null repl-start) (apply #'concatenate 'string
                                 (nreverse (cons kept-str new-subseqs))))
    (push kept-str new-subseqs)
    (push new-substr new-subseqs)))

(provide :words)