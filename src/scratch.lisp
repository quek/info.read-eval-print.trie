(ql:quickload :info.read-eval-print.trie)

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
;;Evaluation took:
;;  2.120 seconds of real time
;;  2.120000 seconds of total run time (2.120000 user, 0.000000 system)
;;  100.00% CPU
;;  5,915,770,302 processor cycles
;;  25,552,080 bytes consed
;;
;;Evaluation took:
;;  0.067 seconds of real time
;;  0.068000 seconds of total run time (0.068000 user, 0.000000 system)
;;  101.49% CPU
;;  188,572,903 processor cycles
;;  0 bytes consed


(let ((da (make-da)))
  (da-put da #(0))
  da)
(let ((da (make-da)))
  (da-put da #())
  da)

(let ((da (make-da)))
  (da-insert-branch da (da-get-root da) 0)
  da)
;;⇒ #S(DA :CELLS #((3674004220 . 4) (-1 . -1) (3 . 0) (-1 . 2)))



;;⇒ #S(DA :CELLS #((3674004220 . 5) (-3 . -3) (3 . 0) (-3 . -1) (-3 . 2)))
