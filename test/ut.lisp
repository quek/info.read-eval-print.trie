(in-package :info.read-eval-print.trie.test)

(def-suite all)

(def-suite da :in all)
(in-suite da)

(test put-and-get
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

(debug! 'all)


