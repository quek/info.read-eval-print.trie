(asdf:defsystem :info.read-eval-print.trie
  :serial t
  :description "trie tree"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "BSD"
  :pathname "src/"
  :components ((:file "package")
               (:file "util")
               (:file "da")
               (:file "data")
               (:file "trie"))
  :depends-on (:anaphora))
