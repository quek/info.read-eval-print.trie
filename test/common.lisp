(in-package :info.read-eval-print.trie.test)

(def-suite all)

(defvar *words*
  (let ((s (make-random-state nil)))
    (sort (with-open-file (in "/usr/share/dict/words")
            (loop for word = (read-line in nil)
                  while word
                  collect (sb-ext:string-to-octets word)))
          #'(lambda (a b)
              (declare (ignore a b))
              (<= (random 10 s) (random 10 s))))))

(defmacro with-test-dir ((dir-var &optional (dir-path "/tmp/trie-test/")) &body body)
  `(let ((,dir-var ,dir-path))
     (ignore-errors (sb-ext:delete-directory ,dir-var :recursive t))
     (ensure-directories-exist ,dir-var)
     ,@body))
