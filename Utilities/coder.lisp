;; coding.lisp

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

;; Implements methods to assist with saving and restoring Lisp instances/slots/values as part of documents that
;; are saved to disk.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes)
  (require :ns-string-utils))

(in-package :iu)

;; Supports helper functions that can encode lisp objects into NSData for archiving

(defmethod encode-lisp-object-for-key ((coder ns:ns-coder) obj key-string)
  (#/encodeObject:forKey: coder (lisp-object-to-ns-data obj) (lisp-to-temp-nsstring key-string)))

(defmethod encode-lisp-object-for-key ((coder ns:ns-coder) ( obj) key-string)
  (#/encodeObject:forKey: coder (lisp-object-to-ns-data obj) (lisp-to-temp-nsstring key-string)))

(defmethod decode-lisp-object-for-key ((coder ns:ns-coder) key-string)
  (ns-data-to-lisp-object (#/decodeObjectForKey: coder (lisp-to-temp-nsstring key-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encode-with-coder methods for standard lisp objects

(defmethod encode-with-coder ((coder ns:ns-coder) lisp-obj)
  ;; default method for all other lisp objects
  ;; assumes that they can be printed out and re-created by doing read-from-string
  (lisp-object-to-ns-data obj))

(defmethod encode-with-coder ((coder ns:ns-coder) (lisp-obj)
  ;; default method for all other lisp objects
  ;; assumes that they can be printed out and re-created by doing read-from-string
  (lisp-object-to-ns-data obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List methods to augment ns-keyed-archiver functionality for standard lisp objects

(defmethod archived-data-with-root-object ((obj ns:ns-object))
  ;; Do default archiving for an ns-object instance.
  ;; This means that any class that derives from ns-object and wants to be archived must
  ;; either define this method (and probably call-next-method) or be satisfied with
  ;; whatever archiving is done by the parent class.
  (#/archivedDataWithRootObject: ns:ns-keyed-archiver obj))

(defmethod archived-data-with-root-object ((obj standard-object))
  ;; Not an ns-object, but a lisp object
  (#/archivedDataWithRootObject: ns:ns-keyed-archiver obj))

(defmethod archived-data-with-root-object (obj)
  ;; archive some object which is not an ns-object or a lisp instance
  ;; This is presumably a lisp object that can be just written to a string and read back
  (lisp-object-to-ns-data 
