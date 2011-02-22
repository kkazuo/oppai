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
  (let ((mx (make-lock))
        (wq (make-condition-variable))
        (name name)
        (st :not-ready)
        value alter)
    (declare (type keyword st)
             (ignorable name))
    (dlambda
     (:read ()
            (labels ((transfer ()
                       (let ((val value))
                         (setf st :not-ready)
                         (setf value nil)
                         (condition-notify wq)
                         val)))
              (with-lock-held (mx)
                (ecase st
                  (:not-ready
                   (setf st :reading)
                   (loop (when (eq st :ready)
                           (return))
                         (condition-wait wq mx))
                   (transfer))
                  (:writing
                   (setf st :ready)
                   (condition-notify wq)
                   (loop (when (eq st :transfer)
                           (return))
                         (condition-wait wq mx))
                   (transfer))))))
     (:write (val)
             (labels ((transfer (val tag)
                        (setf value val)
                        (condition-notify wq)
                        (loop (unless (eq st tag)
                                (return))
                              (condition-wait wq mx))
                        (values)))
               (with-lock-held (mx)
                 (ecase st
                   (:not-ready
                    (setf st :writing)
                    (loop (when (eq st :ready)
                            (return))
                          (condition-wait wq mx))
                    (setf st :transfer)
                    (transfer val :transfer))
                   (:alting
                    (setf st :alt-enabling)
                    (funcall alter)
                    (setf alter nil)
                    (loop (when (eq st :ready)
                            (return))
                          (condition-wait wq mx))
                    (setf st :transfer)
                    (transfer val :transfer))
                   (:reading
                    (setf st :ready)
                    (transfer val :ready))))))
     (:alting (alt)
              (with-lock-held (mx)
                (ecase st
                  (:not-ready
                   (setf st :alting)
                   (setf alter alt)
                   nil)
                  (:writing
                   (setf st :alt-enabling)
                   (funcall alt)
                   t))))
     (:unalting ()
                (with-lock-held (mx)
                  (ecase st
                    (:alting
                     (setf st :not-ready)
                     (setf alter nil)
                     nil)
                    (:alt-enabling
                     (setf st :writing)
                     (setf alter nil)
                     t)))))))

(defun make-alternate (&optional name)
  (let ((mx (make-lock))
        (wq (make-condition-variable))
        (name name)
        (st :not-ready)
        (ranst (make-random-state)))
    (declare (type keyword st)
             (ignorable name))
    (lambda
        (&rest channels &aux pri)
      (when (eq (car channels) :pri)
        (setf channels (cdr channels))
        (setf pri t))
      (with-lock-held (mx)
        (setf st :not-ready))
      (let* ((callback (lambda ()
                         (with-lock-held (mx)
                           (ecase st
                             (:not-ready
                              (setf st :enabling))
                             (:waiting
                              (setf st :enabling)
                              (condition-notify wq))
                             (:enabling)))))
             (ready (loop for chan in channels
                          and i = 0 then (1+ i)
                          nconc (if (funcall chan :alting callback)
                                    (list i)))))
        (unless ready
          (with-lock-held (mx)
            (ecase st
              (:not-ready
               (setf st :waiting)
               (loop (unless (eq st :waiting)
                       (return))
                     (condition-wait wq mx)))
              (:enabling))))
        (let ((ready* (loop for chan in channels
                            and i = 0 then (1+ i)
                            nconc (if (funcall chan :unalting)
                                      (list i)))))
          (if ready*
              (let ((channel
                     (nth
                      (if pri
                          (car ready*)
                          (nth (random (length ready*) ranst) ready*))
                      channels)))
                (values (funcall channel :read) channel))))))))

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
   (:alting (alt) (declare (ignore alt)) t)
   (:unalting () t)))
