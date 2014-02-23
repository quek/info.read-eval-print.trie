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
                   for i from 0
                      if (da-get da x) do (print i)
                   never (da-get da x)))))
;;⇒ T


;;2014-02-16
;;Evaluation took:
;;  1.865 seconds of real time
;;  1.852000 seconds of total run time (1.852000 user, 0.000000 system)
;;  99.30% CPU
;;  5,203,238,789 processor cycles
;;  25,406,336 bytes consed
;;
;;Evaluation took:
;;  0.050 seconds of real time
;;  0.052000 seconds of total run time (0.052000 user, 0.000000 system)
;;  104.00% CPU
;;  139,507,396 processor cycles
;;  20,000 bytes consed

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

(progn
  (asdf/run-program:run-program "rm -rf /tmp/trie")
  (with-open-trie (trie (make-file-trie "/tmp/trie/"))
    (time (loop for x in (subseq *data* 0 10000)
                do (trie-put trie x x)))))
;;⇒ NIL

(with-open-trie (trie (make-file-trie "/tmp/trie/"))
  (time (loop for x in (subseq *data* 0 10000)
              always (equalp x (trie-get trie x)))))
;;⇒ T

(progn
  (asdf/run-program:run-program "rm -rf /tmp/trie")
  (let ((trie (make-file-trie "/tmp/trie/")))
    (with-open-trie (trie trie)
      (trie-put trie #(1) #(1 1))))
  (let ((trie (make-file-trie "/tmp/trie/")))
    (with-open-trie (trie trie)
      (trie-get trie #(1)))))
;;⇒ #(1 1)
