(require :lisp-app-win-controller)

(defmethod ref-count ((self ns:ns-object))
  (#_CFGetRetainCount self))

(probe-file "ccl:contrib;krueger;InterfaceProjects;Cocoa Dev;lisp-app-doc.bundle")

(setf ns (namestring *))

(setf lb (lb::lisp-bundle-with-path ns))

(setf dict (#/infoDictionary lb))

(setf ldc (ad::install-lisp-app-tools))

(iu::bundle-objects ldc)

(iu::delegate ldc)

(setf ht (iu::smh ))

(setf keys
      (let ((keys nil))
        (maphash #'(lambda (k v)
                     (push k keys)
                     (format t "~%~%Key = ~s~%~{~s~%~}" k v))
                 ht)
        keys))

(setf mitems
      (let ((mi nil))
        (maphash #'(lambda (k v)
                     (setf mi (append mi v)))
                 ht)
        mi))

(mapcar #'ref-count mitems)

(setf dev-key (second keys))

(setf dev-mi (first (gethash dev-key ht)))

(setf self (first (iu::documents ldc)))

(ad::app-doc-class self)

(ad::doc-classes self)

(setf wc (first (iu::ns-to-lisp-list (#/windowControllers ad))))

(defun find-bad-prop (dict)
  (let ((keys (iu::ns-to-lisp-list (#/allKeys dict) :element-class ns:ns-object))
        (bad-paths nil))
    (dolist (key keys bad-paths)
      (let* ((val (#/objectForKey: dict key))
             (val-type (type-of val)))
        (unless (member val-type 
                        (list 'ns:NS-Data
                              'ns:NS-Date
                              'ns:NS-Number
                              'ns:NS-String
                              'ns:NS-Array
                              'ns:NS-Dictionary)
                        :test #'(lambda (typ1 typ2)
                                  (subtypep typ1 typ2)))
          ;; something that shouldn't be there
          (push (list (iu::ns-to-lisp-string key)
                      val-type
                      val)
                bad-paths))
        ;; check containers recursively
        (cond ((subtypep val-type 'ns:ns-array)
               (let ((bad-sub-paths (find-bad-prop-in-array val)))
                 (when bad-sub-paths
                   (push (list (iu::ns-to-lisp-string key)
                               bad-sub-paths)
                         bad-paths))))
              ((subtypep val-type 'ns:ns-dictionary)
               (let ((bad-sub-paths (find-bad-prop val)))
                 (when bad-sub-paths
                   (push (list (iu::ns-to-lisp-string key)
                               bad-sub-paths)
                         bad-paths))))
              (t
               nil))))
    bad-paths))

(defun find-bad-prop-in-array (arr)
  (let ((asize (#/count arr))
        (bad-paths nil))
    (dotimes (indx asize bad-paths)
      (let* ((val (#/objectAtIndex: arr indx))
             (val-type (type-of val)))
        (unless (member val-type 
                        (list 'ns:NS-Data
                              'ns:NS-Date
                              'ns:NS-Number
                              'ns:NS-String
                              'ns:NS-Array
                              'ns:NS-Dictionary)
                        :test #'(lambda (typ1 typ2)
                                  (subtypep typ1 typ2)))
          ;; something that shouldn't be there
          
          (push (list indx
                      val-type
                      val)
                bad-paths))
        ;; check containers recursively
        (cond ((subtypep val-type 'ns:ns-array)
               (let ((bad-sub-paths (find-bad-prop-in-array val)))
                 (when bad-sub-paths
                   (push (list indx
                               bad-sub-paths)
                         bad-paths))))
              ((subtypep val-type 'ns:ns-dictionary)
               (let ((bad-sub-paths (find-bad-prop val)))
                 (when bad-sub-paths
                   (push (list indx
                               bad-sub-paths)
                         bad-paths))))
              (t
               nil))))
    bad-paths))

