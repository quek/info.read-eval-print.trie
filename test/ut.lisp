(in-package :info.read-eval-print.trie.test)

(def-suite all)

(def-suite da :in all)
(in-suite da)

(test test-put-and-get
  (let ((da (info.read-eval-print.trie::make-da)))

    (info.read-eval-print.trie::da-put da #())
    (is-true (info.read-eval-print.trie::da-get da #()))

    (info.read-eval-print.trie::da-put da #(0))
    (is-true (info.read-eval-print.trie::da-get da #(0)))

    (info.read-eval-print.trie::da-put da #(1))
    (is-true (info.read-eval-print.trie::da-get da #(1)))

    (info.read-eval-print.trie::da-put da #(2 1 0))
    (is-true (info.read-eval-print.trie::da-get da #(2 1 0)))

    (info.read-eval-print.trie::da-put da #(1 2 3))
    (is-true (info.read-eval-print.trie::da-get da #(1 2 3)))

    (is-false (info.read-eval-print.trie::da-get da #(1 2)))
    (info.read-eval-print.trie::da-put da #(1 2))
    (is-true (info.read-eval-print.trie::da-get da #(1 2)))))

(test test-delete
  (let ((da (info.read-eval-print.trie::make-da)))

    (info.read-eval-print.trie::da-put da #(1 2))
    (info.read-eval-print.trie::da-put da #(1 2 3))
    (info.read-eval-print.trie::da-put da #(1 2 3 4))

    (info.read-eval-print.trie::da-delete da #(1 2 3))
    (is-true (info.read-eval-print.trie::da-get da #(1 2)))
    (is-false (info.read-eval-print.trie::da-get da #(1 2 3)))
    (is-true (info.read-eval-print.trie::da-get da #(1 2 3 4)))

    (info.read-eval-print.trie::da-delete da #(1 2 3 4))
    (is-true (info.read-eval-print.trie::da-get da #(1 2)))
    (is-false (info.read-eval-print.trie::da-get da #(1 2 3)))
    (is-false (info.read-eval-print.trie::da-get da #(1 2 3 4)))

    (info.read-eval-print.trie::da-delete da #(1 2))
    (is-false (info.read-eval-print.trie::da-get da #(1 2)))
    (is-false (info.read-eval-print.trie::da-get da #(1 2 3)))
    (is-false (info.read-eval-print.trie::da-get da #(1 2 3 4)))))


(defvar *words*
  (let ((s (make-random-state nil)))
    (sort (with-open-file (in "/usr/share/dict/words")
            (loop for word = (read-line in nil)
                  while word
                  collect (sb-ext:string-to-octets word)))
          #'(lambda (a b)
              (declare (ignore a b))
              (<= (random 10 s) (random 10 s))))))

(test test-by-dict/words
  (loop for i from 0
        for word in *words*
        if (zerop (mod i 5))
          collect word into not-put-words
        else
          collect word into put-words
        finally
           (time
            (let ((da (info.read-eval-print.trie::make-da)))
              (loop for word in put-words
                    do (info.read-eval-print.trie::da-put da word))
              (is-true
               (loop for word in put-words
                     always (info.read-eval-print.trie::da-get da word)))
              (is-true
               (loop for word in not-put-words
                     never (info.read-eval-print.trie::da-get da word)))

              (let* ((count 100)
                     (delete-words (subseq put-words 0 count)))
                (loop for word in delete-words
                      do (info.read-eval-print.trie::da-delete da word))
                (is-true
                 (loop for word in delete-words
                       never (info.read-eval-print.trie::da-get da word)))
                (is-true
                 (loop for word in (subseq put-words count)
                       always (info.read-eval-print.trie::da-get da word)))

                (loop for word in delete-words
                      do (info.read-eval-print.trie::da-put da word))
                (is-true
                 (loop for word in put-words
                       always (info.read-eval-print.trie::da-get da word))
                 (is-false
                  (loop for word in not-put-words
                        never (info.read-eval-print.trie::da-get da word)))))))))


(debug! 'all)
