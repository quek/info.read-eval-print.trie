(in-package :info.read-eval-print.trie.test)

(def-suite file-segment :in all)
(in-suite file-segment)

(test test-file-segment-put
  (with-test-dir (dir)
    (let* ((segment (make-instance 'info.read-eval-print.trie::file-segment
                                   :size 8 :directory dir)))
      (info.read-eval-print.trie::segment-open segment)
      (is (= 0
             (info.read-eval-print.trie::segment-put segment #(1))))
      (is (= 1
             (info.read-eval-print.trie::segment-put segment #(1 2 3 4 5 6 7 8))))
      (is (= 2
             (info.read-eval-print.trie::segment-put segment #(1 2 3))))
      (info.read-eval-print.trie::segment-close segment))))