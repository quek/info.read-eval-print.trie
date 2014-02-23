(in-package :info.read-eval-print.trie.test)

(def-suite trie :in all)
(in-suite trie)

(test test-file-trie-put-and-get
  (with-open-trie (trie (make-trie "/tmp/test-file-trie-put-and-get/"))
    (trie-open trie)
    (trie-put trie #(1 2 3) #(10 20 30))
    (is (equalp #(10 20 30)
                (trie-get trie #(1 2 3))))))

(test test-file-trie-delete
  (with-open-trie  (trie (make-trie "/tmp/test-file-trie-delete/"))
    (trie-open trie)
    (trie-put trie #(1 2 3) #(10 20 30))
    (trie-delete trie #(1 2 3))
    (is (eq nil
            (trie-get trie #(1 2 3))))))


(test test-with-open-trie
  (let ((path "/tmp/test-with-open-trie"))
    (let ((trie (make-trie path)))
      (with-open-trie (trie trie)
        (trie-put trie #(1) #(1 1))))
    (let ((trie (make-trie path)))
      (with-open-trie (trie trie)
        (is (equalp #(1 1)
                    (trie-get trie #(1))))))))

(test test-trie-by-dict-words
  (let ((put-words ())
        (not-put-words ())
        (delete-count 100)
        (delete-words ())
        (path "/tmp/test-trie-by-dict-words/"))
    (asdf/run-program:run-program (format nil "rm -rf ~a" path))
    (loop for i from 0
          for word in *words*
          if (zerop (mod i 5))
            do (push word not-put-words)
          else
            do (push word put-words))
    (setf delete-words (subseq put-words 0 delete-count))
    (time
     (with-open-trie (trie (make-trie path))
       (loop for word in put-words
             do (trie-put trie word word))
       (loop for word in delete-words
             do (trie-delete trie word))))
    (with-open-trie (trie (make-trie path))
     (is-true
      (loop for word in delete-words
            never (trie-get trie word)))
     (is-true
      (loop for word in (subseq put-words delete-count)
            always (equalp word (trie-get trie word))))
      (loop for word in delete-words
            do (trie-put trie word word))
      (is-true
       (loop for word in put-words
             always (equalp word (trie-get trie word)))
       (is-false
        (loop for word in not-put-words
             never (trie-get trie word)))))))