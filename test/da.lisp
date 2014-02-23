(in-package :info.read-eval-print.trie.test)

(def-suite da :in all)
(in-suite da)

(test test-da-put-and-get
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

(test test-da-delete
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



(test test-da-save-load
  (let ((a (info.read-eval-print.trie::make-da))
        (b (info.read-eval-print.trie::make-da)))
    (info.read-eval-print.trie::da-put a #(1 2 3 4 5 6 7 8 9))
    (info.read-eval-print.trie::da-save a "/tmp/da")
    (info.read-eval-print.trie::da-load b "/tmp/da")
    (is (equalp a b))))
