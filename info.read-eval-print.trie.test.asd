(asdf:defsystem :info.read-eval-print.trie.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "ut"))
  :depends-on (:info.read-eval-print.trie
               :fiveam))
