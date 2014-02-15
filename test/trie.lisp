(in-package :info.read-eval-print.trie.test)

(def-suite trie :in all)
(in-suite trie)

(test test-trie-put-and-get
  (let ((trie (info.read-eval-print.trie:make-trie)))
    (info.read-eval-print.trie:trie-put trie #(1 2 3) #(10 20 30))
    (is (equalp #(10 20 30)
                (info.read-eval-print.trie:trie-get trie #(1 2 3))))))

(test test-trie-delete
  (let ((trie (info.read-eval-print.trie:make-trie)))
    (info.read-eval-print.trie:trie-put trie #(1 2 3) #(10 20 30))
    (info.read-eval-print.trie:trie-delete trie #(1 2 3))
    (is (eq nil
            (info.read-eval-print.trie:trie-get trie #(1 2 3))))))
