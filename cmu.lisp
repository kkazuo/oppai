;;
;; Copyright (c) 2011 Kazuo Koga <kogakazuo@gmail.com>
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

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
