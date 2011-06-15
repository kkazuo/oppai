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

#|
This is oppai, a thread synchronization primitive library.
- 1 to 1
- non buffering
- occam language style input guard (see http://frmb.org/occtutor.html)

Run only on SBCL now.

|#


;; make a channel.
(defvar ch1 (make-channel))
(defvar ch2 (make-channel))

;; communicate each other.
(make-thread (lambda () (loop (! ch1 (random 100)))))
(make-thread (lambda () (loop (princ (? ch1)))))

;; make more channel.
(defvar ch3 (make-channel))
(defvar ch4 (make-channel))

;; make a ALT env.
(defvar a (make-alternate))

;; multiple writer.
(make-thread (lambda () (loop (! ch3 (random 100)))))
(make-thread (lambda () (loop (! ch4 (- (random 100))))))

;; select channel.
(loop (princ (alt a ch3 ch4)))
;; or can skip.
(loop (princ (alt a ch3 ch4 (skip))))


;; DO NOT share channel endpoint.
#|
;; multiple thread share on write endpoint. this does not work.
(defvar chx (make-channel))
(make-thread (lambda () (loop (! chx (random 100)))))
(make-thread (lambda () (loop (! chx (random 100)))))

;; multiple thread share on read endpoint. this does not work.
(defvar chy (make-channel))
(make-thread (lambda () (loop (? chy))))
(make-thread (lambda () (loop (? chy))))

;; multiple thread switch endpoint read/write. this does not work.
(defvar chz (make-channel))
(make-thread (lambda () (loop (! chz (? chz)))))
(make-thread (lambda () (loop (! chz (random 100)) (princ (? chz)))))
|#
