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

(defun do-parallel (&rest functions)
  (let ((thrs (loop for fun in (cdr functions)
                    collect (make-thread fun :name "do-parallel-thread")))
        (fun (car functions)))
    (prog1
      (when fun
        (funcall fun))
      (loop for thr in thrs
            do (join-thread thr)))))
