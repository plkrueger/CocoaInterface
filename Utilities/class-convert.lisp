;; class-convert.lisp

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

;; Code to convert class definitions between Objective-C and Lisp. Will take a .h file generated via 
;; IB and create a prototype Lisp class or will take a Lisp Class and generate a .h that can be loaded into
;; IB. Users are free to start in IB and import into Lisp or start in Lisp and load into IB.

;; An Objective-C .h file showing the class interface looks something like:

#|

@interface NSWindow : NSResponder {
    IBOutlet id delegate;
    IBOutlet NSView *initialFirstResponder;
    IBOutlet NSMenu *menu;
}
- (IBAction)deminiaturize:(id)sender;
- (IBAction)makeKeyAndOrderFront:(id)sender;
- (IBAction)miniaturize:(id)sender;
- (IBAction)orderBack:(id)sender;
- (IBAction)orderFront:(id)sender;
- (IBAction)orderOut:(id)sender;
- (IBAction)performClose:(id)sender;
- (IBAction)performMiniaturize:(id)sender;
- (IBAction)performZoom:(id)sender;
- (IBAction)print:(id)sender;
- (IBAction)runToolbarCustomizationPalette:(id)sender;
- (IBAction)toggleToolbarShown:(id)sender;
@end


|#

;; Such files will be named "<ClassName>.h"

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :words)
  (require :binding-utils)
  (require :open-panel)
  (require :save-panel))

(in-package :iu)

(defun foreign-id-slot-p (slot)
  (and (typep slot 'CCL::FOREIGN-DIRECT-SLOT-DEFINITION)
       (ccl::objc-id-type-p (ccl::foreign-slot-definition-foreign-type slot))))

(defmethod class-outlets ((cl objc:objc-class))
  (mapcar #'(lambda (dslot)
              (iu::lisp-to-objc-keypathname (string (slot-value dslot 'ccl::name))))
          (remove-if-not #'foreign-id-slot-p (class-direct-slots cl))))

(defmethod objc-method-class-name ((self ccl::lisp-objc-method))
  (ccl::objc-class-descriptor-name (ccl::lisp-objc-method-class-descriptor self)))

(defmethod objc-method-name ((self ccl::lisp-objc-method))
  (ccl::objc-selector-name (ccl::lisp-objc-method-sel self)))

(defun message-methods (msg)
  (let ((mi (ccl::get-objc-message-info msg)))
    (and mi (ccl::objc-message-info-methods mi))))

(defun action-method-p (mi)
  ;; mi is a struct of type objc-method-info (defined in db-io.lisp)
  (and (typep mi 'ccl::objc-method-info)
       (equal '(:ID) (ccl::objc-method-info-arglist mi))
       (equal :VOID (ccl::objc-method-info-result-type mi))))

(defun class-actions (cl-name)
  (let ((actions nil))
    (maphash #'(lambda(k v)
                 (declare (ignore k))
                 (let* ((message-name (objc-method-name v))
                        (msg-info (ccl::get-objc-message-info message-name)))
                   (when (and (string= cl-name (objc-method-class-name v))
                              (action-method-p (find cl-name 
                                                     (ccl::objc-message-info-methods msg-info)
                                                     :key #'ccl::objc-method-info-class-name
                                                     :test #'string=)))
                     (push message-name actions))))
             ccl::*lisp-objc-methods*)
    actions))

(defmethod lisp-to-dot-h ((cl objc:objc-class))
  (let* ((cl-name (ccl::compute-objc-classname (class-name cl)))
         (super-name (ccl::compute-objc-classname (class-name (first (class-direct-superclasses cl)))))
         (outlets (class-outlets cl))
         (actions (class-actions cl-name)))
    (concatenate 'string
                 (format nil "@interface ~a : ~a {" cl-name super-name)
                 (format nil "~{~^~%    IBOutlet id ~a;~}~%}~%" outlets)
                 (format nil "~{- (IBAction)~a(id)sender;~%~}@end~%" actions))))

(defmethod save-dot-h ((cl objc:objc-class))
  (let* ((save-str (lisp-to-dot-h cl))
         (fdstr (first (iu:save-panel :type "h"))))
    (when (and save-str fdstr)
      (with-open-file (fd fdstr :direction :output :if-exists :overwrite :if-does-not-exist :create)
        (write-string save-str fd)))))

(defun dot-h-to-lisp (&optional (pr t))
  (let ((dot-h-name (first (iu::open-panel :types (list "h"))))
        (lisp-strs nil)
        (cl-name nil)
        (super-name nil)
        (slot-line-prefix "  ("))
    (when dot-h-name
      (with-open-file (objc-fd dot-h-name :direction :input)
        (do ((line (read-line objc-fd nil nil)
                   (read-line objc-fd nil nil)))
            ((null line) (let ((res-str (apply #'concatenate 'string lisp-strs)))
                           (if pr
                             (progn 
                               (format t "~a" res-str)
                               (values))
                             (values res-str cl-name))))
          (let* ((wrds (iu:words line :delims " {-();"))
                 (wrd1 (first wrds)))
            (cond ((string= wrd1 "@interface")
                   (setf cl-name (string-downcase (string (ccl::objc-to-lisp-classname (second wrds)))))
                   (setf super-name (ccl::objc-to-lisp-classname (fourth wrds)))
                   (when (symbolp super-name)
                     (setf super-name (format nil 
                                              "~a:~a" 
                                              (string-downcase (package-name (symbol-package super-name)))
                                              (string-downcase (string super-name)))))
                   (setf lisp-strs (list (format nil
                                                 "(defclass ~a (~a)"
                                                 cl-name
                                                 super-name))))
                  ((string= wrd1 "IBOutlet")
                   (let ((sl-name (string-downcase (objc-to-lisp-keypathname (third wrds)))))
                     (nconc lisp-strs (list (format nil
                                                    "~%~a(~a :foreign-type :id :accessor ~a)"
                                                    slot-line-prefix
                                                    sl-name
                                                    sl-name)))
                     (setf slot-line-prefix "   ")))
                  ((string= wrd1 "}")
                   (nconc lisp-strs (list (format nil ")~%  (:metaclass ns:+ns-object))"))))
                  ((string= wrd1 "IBAction")
                   (nconc lisp-strs (list (format nil
                                                  "~%~%(objc:defmethod (#/~a :void)~% ~17t((self ~a) (~a :id))~%  ())"
                                                  (second wrds)
                                                  cl-name
                                                  (fourth wrds)))))
                  (t
                   nil))))))))
    
(defun save-lisp-from-dot-h ()
  (multiple-value-bind (save-str cl-name) (dot-h-to-lisp nil)
    (let ((fdstr (first (iu:save-panel :type "lisp" :name cl-name))))
      (when (and save-str fdstr)
        (with-open-file (fd fdstr :direction :output :if-exists :overwrite :if-does-not-exist :create)
          (write-string save-str fd))))))

(defun objc-metaclass-p (form)
  (let ((metaclass-form (find :metaclass (cdddr form) :key #'first)))
    (and metaclass-form (eq (second metaclass-form) 'ns:+ns-object))))

(defun scan-file-for-classes (file-name &key (objc-only nil))
  ;; look for class definitions in the file and also determine if they inherit from Objective-C
  ;; If requested classes are objc-only we also convert the class name to Objective-C form
  ;; We also return any module name if it exists in a (provide <module-name>) statment as a second value.
  (with-open-file (f file-name)
    (do ((next-form (read f nil :eof)
                    (read f nil :eof))
         (found-classes nil)
         (module-name nil))
        ((eq next-form :eof) (values (if objc-only
                                       (mapcar #'ccl::compute-objc-classname found-classes)
                                       found-classes)
                                     module-name))
      (if (eq (first next-form) 'defclass)
        (if (or (not objc-only) (objc-metaclass-p next-form))
          (push (second next-form) found-classes))
        (if (eq (first next-form) 'provide)
          (setf module-name (second next-form)))))))

(defun scan-file-for-module (file-name)
  ;; look for a (provide <module-name>) form in the file
  (with-open-file (f file-name)
    (do ((next-form (read f nil :eof)
                    (read f nil :eof))
         (module-name nil))
        ((or (eq next-form :eof) module-name) module-name)
      (if (eq (first next-form) 'provide)
        (setf module-name (second next-form))))))

(provide :class-convert)