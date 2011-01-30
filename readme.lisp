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
