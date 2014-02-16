(in-package :info.read-eval-print.trie.test)

(def-suite trie :in all)
(in-suite trie)

(test test-memory-trie-put-and-get
  (let ((trie (make-memory-trie)))
    (trie-put trie #(1 2 3) #(10 20 30))
    (is (equalp #(10 20 30)
                (trie-get trie #(1 2 3))))))

(test test-memory-trie-delete
  (let ((trie (make-memory-trie)))
    (trie-put trie #(1 2 3) #(10 20 30))
    (trie-delete trie #(1 2 3))
    (is (eq nil
            (trie-get trie #(1 2 3))))))


(test test-file-trie-put-and-get
  (let ((trie (make-file-trie "/tmp/test-file-trie-put-and-get/")))
    (trie-open trie)
    (trie-put trie #(1 2 3) #(10 20 30))
    (is (equalp #(10 20 30)
                (trie-get trie #(1 2 3))))
    (trie-close trie)))

(test test-file-trie-delete
  (let ((trie (make-file-trie "/tmp/test-file-trie-delete/")))
    (trie-open trie)
    (trie-put trie #(1 2 3) #(10 20 30))
    (trie-delete trie #(1 2 3))
    (is (eq nil
            (trie-get trie #(1 2 3))))
    (trie-close trie)))