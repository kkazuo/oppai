#|
Copyright (c) 2011, Kazuo Koga <kogakazuo@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
|#

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
     (labels ((transfer ()
                (unwind-protect value
                  (setf value nil)
                  (setf state 'accept-read)
                  (condition-notify condvar))))
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
     (labels ((transfer ()
                (unwind-protect value
                  (setf value nil)
                  (setf state 'accept-read)
                  (condition-notify condvar))))
       (with-lock-held (lock)
         (ecase state
           (wait-read
            (values (transfer) t))
           ((empty accept-read)
            (values nil nil)))))))
