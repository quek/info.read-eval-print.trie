(defpackage :info.read-eval-print.trie
 (:use :cl :anaphora)
 (:export #:make-trie
          #:trie-open
          #:trie-close
          #:trie-get
          #:trie-put
          #:trie-delete
          #:with-open-trie))
