;; ts-queue.lisp
;; An updated version of thread-safe-queue.lisp

#|
The MIT license.

Copyright (c) 2017 Paul L. Krueger

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

;; lockless between readers and writers, so one reader and one writer never stall
;; multiple readers wait for each other
;; multiple writers wait for each other
;; When reader is done it should call reader-done with ts-queue as argument
;; When writer is done it should call writer-done with ts-queue as argument
;; writers-should-quit is a convenience if outside process wishes to tell writers to quit it
;;   should set this to some non-nil value
;; writes-done is a semaphore that will be signaled when all registered writers are done
;; We make use of or-semaphores to wait on any of multiple conditions to avoid timeouts
;; writers are free to determine when they should quit on their own, but in eithr case should call
;;   writer-done
;; when writers are done and ts-queue is empty, pop-queue returns :quit and reader should quit
;; when all readers are done, the processing-complete semaphore is signaled
;; The semaphores new-reader and new-writer are signaled when a new reader or writer is detected
;; interacting with the ts-queue. They are purely for the convenience of ts-queue users

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iu-classes)
  (require :or-semaphore))

(in-package :iu)

(defstatic *q-debug* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ts-queue class and methods

(defclass ts-queue ()
  ((queue-name :accessor queue-name
               :initarg :name)
   (first-node :accessor first-node)
   (last-node :accessor last-node)
   (push-lock :accessor push-lock
              :initform (make-lock))
   (pop-lock :accessor pop-lock
             :initform (make-lock))
   (has-data :accessor has-data
             :initform (make-semaphore))
   (writers-done :accessor writers-done
                 :initform (make-semaphore))
   (has-data-or-writers-done :accessor has-data-or-writers-done
                             :initform nil)
   (processing-complete :accessor processing-complete
                        :initform (make-semaphore))
   (reader-procs :accessor reader-procs
                 :initform nil)
   (writer-procs :accessor writer-procs
                 :initform nil)
   (new-reader :accessor new-reader
               :initform (make-semaphore))
   (new-writer :accessor new-writer
               :initform (make-semaphore))
   (writers-should-quit :accessor writers-should-quit
                        :initform nil)
   (pushed-data :accessor pushed-data
                :initform nil))
  (:default-initargs
      :name "thread-safe-queue"))

;; only readers manipulate the first-node and only writers manipulate the last-node
;; There is always a first and last node although they may or may not contain valid
;; data (initially they do not). 

(defclass qnode ()
  ((data :accessor data :initarg :data)
   (status :accessor status :initarg :status)
   (tail-p :accessor tail-p :initarg :tail-p)
   (next :accessor next :initarg :next))
  (:default-initargs
      :data nil
      :status :valid
      :tail-p t
      :next nil))

;; data is the data stored in each node
;; status is one of :valid (indicating the node contains valid data),
;; :invalid (indicating the node data field is not valid), or
;; :next-invalid (indicating that the next node contains invalid data
;; even if its own status field is :valid). The :next-invalid staus is
;; only ever set for the first node in the ts-queue.
;; tail-p indicates that this node is the last in the ts-queue.
;; next points to the next node in the ts-queue.

(defmethod initialize-instance :after ((self ts-queue) &key &allow-other-keys)
  (setf (has-data-or-writers-done self)
        (make-instance 'or-semaphore
          :sems (list (has-data self) (writers-done self))))
  (setf (last-node self) (make-instance 'qnode
                           :data nil
                           :status :invalid))
  (setf (first-node self) (make-instance 'qnode
                            :data nil
                            :status :next-invalid
                            :tail-p nil
                            :next (last-node self))))

(defmethod print-object ((self ts-queue) strm)
  (format strm "#<TS-QUEUE ~a>" (queue-name self)))

(defmethod push-queue ((self ts-queue) new-data)
  (when *q-debug*
    (iu::ns-log-format "process ~s pushing ~s on ~s" *current-process* new-data self))
  (with-lock-grabbed ((push-lock self))
    ;; block any other writers
    (unless (eq (writer-procs self) (pushnew *current-process* (writer-procs self)))
      (signal-semaphore (new-writer self)))
    (let ((new-node (make-instance 'qnode :data new-data))
          (prev-node (last-node self)))
      ;; link the new node into the list
      (setf (last-node self) new-node)
      (setf (next prev-node) new-node)
      ;; reset the tail-p flag which may allow the pop code
      ;; to move the head of the ts-queue to this node
      (setf (tail-p prev-node) nil))
    ;; tell readers there is something available
    (signal-semaphore (has-data self))
    new-data))

(defmethod push-new-queue ((self ts-queue) new-data &key (test #'eq))
  ;; if push-new-queue is used to put data on the ts-queue, it should always be used
  (with-lock-grabbed ((push-lock self))
    ;; block any other writers
    (unless (eq (writer-procs self) (pushnew *current-process* (writer-procs self)))
      (signal-semaphore (new-writer self)))
    (unless (member new-data (pushed-data self) :test test)
      (when *q-debug*
        (iu::ns-log-format "process ~s pushing new ~s on ~s" *current-process* new-data self))
      (push new-data (pushed-data self))
      (let ((new-node (make-instance 'qnode :data new-data))
            (prev-node (last-node self)))
        ;; link the new node into the list
        (setf (last-node self) new-node)
        (setf (next prev-node) new-node)
        ;; reset the tail-p flag which may allow the pop code
        ;; to move the head of the ts-queue to this node
        (setf (tail-p prev-node) nil))
      ;; tell readers there is something available
      (signal-semaphore (has-data self))
      new-data)))

(defmethod register-reader ((self ts-queue))
  (unless (eq (reader-procs self) (pushnew *current-process* (reader-procs self)))
    (signal-semaphore (new-reader self))))

(defmethod pop-queue ((self ts-queue) &key (timeout nil))
  ;; timeout is a real number denoting number of seconds to
  ;; wait for something to be in the ts-queue or queue processing to be terminated
  ;; queue processing is terminated when the queue is empty and all writer processes
  ;; are done.
  (flet ((next-in-queue ()
           (when *q-debug*
             (iu::ns-log-format "~s popping from ~s" *current-process* self))
           (with-lock-grabbed ((pop-lock self))
             ;; block other readers from the ts-queue
             (do* ((fn (first-node self)
                       fn-next)
                   (fn-stat (status fn)
                            (status fn))
                   (fn-next (next fn)
                            (next fn))
                   (next-is-tail (tail-p fn-next)
                                 (tail-p fn-next)))
                  ((and next-is-tail (eq :next-invalid fn-stat)) nil)
               (if next-is-tail
                   ;; since the next node is the tail of the ts-queue we can't
                   ;; shrink the ts-queue any further
                   (case fn-stat
                     (:valid 
                      ;; the data in the first node is valid
                      (setf (status fn) :invalid)
                      (return-from next-in-queue (data fn)))
                     (:invalid
                      ;; the data in the next node is valid
                      (setf (status fn) :next-invalid)
                      (return-from next-in-queue (data fn-next)))
                     (:next-invalid
                      ;; The ts-queue is empty -- actually the "do"
                      ;; end condition implements this. We should
                      ;; never get to here. And the "do" end-condition
                      ;; should never be detected because of the
                      ;; semaphore wait.
                      (return-from next-in-queue nil)))
                   (case fn-stat
                     ;; there are more than two nodes in the ts-queue, so 
                     ;; we are permitted to shrink it by moving the first
                     ;; pointer to the next node
                     (:valid
                      ;; the data in the first node is valid
                      (setf (status fn) :invalid)
                      (return-from next-in-queue (data fn)))
                     (:invalid 
                      ;; The data here is invalid, but the next node should
                      ;; be ok. Shrink the ts-queue and try again
                      (setf (first-node self) fn-next))
                     (:next-invalid 
                      ;; Mark the next node as invalid to maintain the information
                      ;; contained in the :next-invalid status here and shrink
                      ;; the ts-queue. Try again.
                      (setf (status fn-next) :invalid)
                      (setf (first-node self) fn-next))))))))
    (unless (eq (reader-procs self) (pushnew *current-process* (reader-procs self)))
      (signal-semaphore (new-reader self)))
    (let ((result (if timeout
                      (timed-wait-on-or-semaphore (has-data-or-writers-done self) timeout)
                      (wait-on-or-semaphore (has-data-or-writers-done self)))))
      (cond ((eql result (has-data self))
             ;; Read the next entry off the queue
             (next-in-queue))
            ((eql result (writers-done self))
             ;; the next line is a little tricky. We just caught the result of a writers-done
             ;; event but other readers may need it as well, so we'll re-signal that semaphore
             (signal-semaphore (writers-done self))
             (when *q-debug*
               (iu::ns-log-format "Found ~s empty and no writers, so returning :quit to ~s"
                                  self *current-process*))
             :quit)
            (t
             nil)))))

(defmethod empty-queue-p ((self ts-queue))
  (with-lock-grabbed ((pop-lock self))
    (with-lock-grabbed ((push-lock self))
      (let* ((fn (first-node self))
             (nxt (next fn)))
        (and (eq (status fn) :next-invalid)
             (tail-p nxt))))))

(defmethod reader-done ((self ts-queue))
  ;; should be called by a writer process before it quite
  (when *q-debug*
    (iu::ns-log-format "reader-done called by ~s" *current-process*))
  (with-lock-grabbed ((pop-lock self))
    ;; block any other readers
    (setf (reader-procs self) (delete *current-process* (reader-procs self)))
    (unless (reader-procs self)
      (signal-semaphore (processing-complete self)))))

(defmethod writer-done ((self ts-queue))
  ;; should be called by a writer process before it quits
  (when *q-debug*
    (iu::ns-log-format "writer-done called by ~s" *current-process*))
  (with-lock-grabbed ((push-lock self))
    ;; block any other writers
    (setf (writer-procs self) (delete *current-process* (writer-procs self)))
    (unless (writer-procs self)
      (when *q-debug*
        (iu::ns-log-format "Signaling writers-done ~s" (writers-done self)))
      (signal-semaphore (writers-done self)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions

(defstatic *write-count* 0)
(defstatic *read-count* 0)
(defstatic *write-sum* 0)
(defstatic *read-sum* 0)
(defstatic *count-lock* (make-lock))
(defstatic *test-tsq* nil)

(defmethod q-writer ((self ts-queue))
  ;; Wait some random amount of time less than 1/10 sec. and push a random value
  ;; onto the ts-queue. Continue until told to quit and then report stats to validate
  ;; ts-queue operation.
  (let ((push-count 0)
        (sum 0)
        (rstate (make-random-state t))) ;; to get a different random state for each writer
    (do ((quit (writers-should-quit self)
               (writers-should-quit self)))
        (quit (progn
                (with-lock-grabbed (*count-lock*)
                  (incf *write-sum* sum)
                  (incf *write-count* push-count))
                (writer-done self)))
      (sleep (random 1.0))
      (let ((val (random 10 rstate)))
        (push-queue self val)
        (incf sum val)
        (incf push-count)))))

(defmethod q-reader ((self ts-queue))
  ;; Pop data off the ts-queue until told to quit and ts-queue is empty.
  ;; Then report stats to validate ts-queue operation.
  (let ((pop-count 0)
        (sum 0))
    (do ((data (pop-queue self)
               (pop-queue self)))
        ((eq data :quit)
         (progn
           (with-lock-grabbed (*count-lock*)
             (incf *read-sum* sum)
             (incf *read-count* pop-count))
           (reader-done self)))
      (when *q-debug*
        (iu::ns-log-format "~s got back ~s from pop-queue" *current-process* data))
      (when data
        (incf sum data)
        (incf pop-count)))))

(defun test-ts-queue ()
  (setf *write-count* 0)
  (setf *read-count* 0)
  (setf *read-sum* 0)
  (setf *write-sum* 0)
  (let ((q (make-instance 'ts-queue)))
    (setf *test-tsq* q)
    ;; start multiple reader and writer threads
    (process-run-function "reader" #'q-reader q)
    (process-run-function "reader" #'q-reader q)
    (process-run-function "writer" #'q-writer q)
    (process-run-function "writer" #'q-writer q)
    (process-run-function "writer" #'q-writer q)
    (sleep (+ 3 (random 7)))
    ;; tell threads to quit
    (setf (writers-should-quit q) t)
    ;; wait for all the threads to quit
    (unless (timed-wait-on-semaphore (processing-complete q) 30)
      (format t "~%They didn't all finish for some reason"))
    ;; report count validity to prove there was one read for one write
    (format t "~%Test wrote ~s and read ~s" *write-count* *read-count*)
    ;; report data consistency to prove that the data read was the same as written
    (format t "~%Test write sum = ~s and read sum = ~s" *write-sum* *read-sum*)
    (values)))
   
(provide :ts-queue)