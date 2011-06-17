;;; Copyright 2011 Kazuo Koga <kogakazuo@gmail.com>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package :oppai)

(defclass sync-channel ()
     ((name
       :initarg :name)
      (lock
       :initform (make-lock))
      (condvar
       :initform (make-condition-variable))
      (state
       :initform 'empty)
      value))

(defmethod print-object ((object sync-channel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name state) object
       (format stream "~s ~a" name state))))

(defun make-sync-channel (&key (name ""))
  (make-instance 'sync-channel :name name))

(defun write-sync-channel (chan val)
  (with-slots (lock condvar state value) chan
     (labels ((transfer ()
                (unwind-protect
                    (loop initially (setf value val)
                                    (setf state 'wait-read)
                                    (condition-notify condvar)
                          do (condition-wait condvar lock)
                          until (eql state 'accept-read)
                          finally (return t))
                  (setf state 'empty)
                  (condition-notify condvar))))
       (with-lock-held (lock)
         (ecase state
           (empty
            (transfer))
           ((wait-read accept-read)
            (loop do (condition-wait condvar lock)
                  until (eql state 'empty))
            (transfer)))))))

(defun read-sync-channel (chan)
  (with-slots (lock condvar state value) chan
     (labels ((transfer (&aux (val value))
                (setf value nil)
                (setf state 'accept-read)
                (condition-notify condvar)
                val))
       (with-lock-held (lock)
         (ecase state
           (wait-read
            (transfer))
           ((empty accept-read)
            (loop do (condition-wait condvar lock)
                  until (eql state 'wait-read))
            (transfer)))))))

(defun try-read-sync-channel (chan)
  (with-slots (lock condvar state value) chan
     (labels ((transfer (&aux (val value))
                (setf value nil)
                (setf state 'accept-read)
                (condition-notify condvar)
                val))
       (with-lock-held (lock)
         (ecase state
           (wait-read
            (values (transfer) t))
           ((empty accept-read)
            (values nil nil)))))))
