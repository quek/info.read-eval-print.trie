(in-package :info.read-eval-print.trie)

(defvar *data*
  (let ((s (make-random-state nil)))
    (sort (with-open-file (in "/usr/share/dict/words")
            (loop for word = (read-line in nil)
                  while word
                  collect (sb-ext:string-to-octets word)))
          #'(lambda (a b)
              (declare (ignore a b))
              (<= (random 1000 s) (random 1000 s))))))

(defvar *data-a*
  (loop for i from 0
        for x in *data*
        unless (zerop (mod i 3))
          collect x))
(length *data-a*)
;;⇒ 66114

(defvar *data-b*
  (loop for i from 0
        for x in *data*
        if (zerop (mod i 3))
          collect x))
(length *data-b*)
;;⇒ 33057

(let* ((da (make-da)))
  (time (loop for x in *data-a*
              do (da-put da x)))
  (time (and (loop for x in *data-a*
                   always (da-get da x))
             (loop for x in *data-b*
                   never (da-get da x)))))
;; 1 回目
;;Evaluation took:
;;  51.317 seconds of real time
;;  53.652000 seconds of total run time (52.464000 user, 1.188000 system)
;;  [ Run times consist of 4.132 seconds GC time, and 49.520 seconds non-GC time. ]
;;  104.55% CPU
;;  143,224,315,804 processor cycles
;;  29,835,494,832 bytes consed
;;
;;Evaluation took:
;;  0.069 seconds of real time
;;  0.068000 seconds of total run time (0.068000 user, 0.000000 system)
;;  98.55% CPU
;;  193,877,351 processor cycles
;;  0 bytes consed
;;
;; 2 回目
;;Evaluation took:
;;  3.174 seconds of real time
;;  5.932000 seconds of total run time (5.824000 user, 0.108000 system)
;;  [ Run times consist of 0.368 seconds GC time, and 5.564 seconds non-GC time. ]
;;  186.89% CPU
;;  8,858,558,801 processor cycles
;;  1,624,867,792 bytes consed
;;
;;Evaluation took:
;;  0.119 seconds of real time
;;  0.196000 seconds of total run time (0.184000 user, 0.012000 system)
;;  [ Run times consist of 0.040 seconds GC time, and 0.156 seconds non-GC time. ]
;;  164.71% CPU
;;  332,753,590 processor cycles
;;  40,390,272 bytes consed
