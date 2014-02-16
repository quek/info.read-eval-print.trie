(defpackage :info.read-eval-print.trie
 (:use :cl :anaphora)
 (:export #:make-file-trie
          #:make-memory-trie
          #:trie-open
          #:trie-close
          #:trie-get
          #:trie-put
          #:trie-delete))
