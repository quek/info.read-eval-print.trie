(asdf:defsystem :info.read-eval-print.trie.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "common")
               (:file "da")
               (:file "trie")
               (:file "run"))
  :depends-on (:info.read-eval-print.trie
               :fiveam))
