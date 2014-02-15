(defpackage :info.read-eval-print.trie
 (:use :cl :anaphora)
 (:export #:make-trie
          #:trie-get
          #:trie-put
          #:trie-delete))
