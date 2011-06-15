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

(defun make-channel (&optional name)
  (let* ((name name)
         (undef (gensym "UNDEF"))
         (value undef))
    (declare (ignorable name))
    (dlambda
     (:read
      (&aux val)
      (process-wait "Channel Read"
                    (lambda ()
                      (not (eq value undef))))
      (prog1
        (setf val value)
        (setf value undef)))
     (:readablep
      ()
      (not (eq value undef)))
     (:write
      (val)
      (process-wait "Channel Write"
                    (lambda ()
                      (eq value undef)))
      (setf value val)
      (process-wait "Channel Wait Rendezvous"
                    (lambda ()
                      (eq value undef)))))))

(defun make-alternate (&optional name)
  (let ((name name)
        (ranst (make-random-state)))
    (declare (ignorable name))
    (lambda (&rest channels &aux pri)
      (when (eq (car channels) :pri)
        (setf channels (cdr channels))
        (setf pri t))
      (let ((ready
             (process-wait "Channel Alt Wait"
                           (lambda ()
                             (loop for chan in channels
                                   and i = 0 then (1+ i)
                                   nconc (if (funcall chan :readablep)
                                             (list i)))))))
        (let ((channel
               (nth (if pri
                        (car ready)
                        (nth (random (length ready) ranst) ready))
                    channels)))
          (values (funcall channel :read) channel))))))

(declaim (inline ?))
(defun ? (channel)
  (funcall channel :read))

(declaim (inline !))
(defun ! (channel value)
  (funcall channel :write value))

(declaim (inline alt))
(defun alt (alt-object &rest args)
  (apply alt-object args))

(declaim (inline pri-alt))
(defun pri-alt (alt-object &rest args)
  (apply alt-object :pri args))

(defun skip ()
  (dlambda
   (:read () nil)
   (:readablep () t)))
