(in-package :info.read-eval-print.trie)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))))
