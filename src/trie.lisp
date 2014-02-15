(in-package :info.read-eval-print.trie)

(defstruct trie
  (da (make-da))
  (data (make-data-strage)))

(defun trie-put (trie key value)
  (let* ((da (trie-da trie))
         (key-index (da-put da key))
         (data-index (data-put (trie-data trie) value)))
    (da-set-base da key-index (- data-index))))

(defun trie-get (trie key)
  (let ((da (trie-da trie)))
    (awhen (da-get da key)
      (data-get (trie-data trie) (- (da-get-base da it))))))

(defun trie-delete (trie key)
  (let* ((da (trie-da trie)))
    (multiple-value-bind (key-index data-index) (da-delete da key)
      (when key-index
        (data-delete (trie-data trie) (- data-index))))))


#+nil
(let ((trie (make-trie)))
  (trie-put trie #(1) #(2))
  (trie-get trie #(1)))
;;⇒ #(2)

#+nil
(let ((trie (make-trie)))
  (trie-put trie #(1) #(2))
  (trie-delete trie #(1))
  (trie-get trie #(1)))
;;⇒ NIL
