(in-package :info.read-eval-print.trie)

(defgeneric trie-open (trie))
(defgeneric trie-close (trie))
(defgeneric trie-get (trie key))
(defgeneric trie-put (trie key vlaue))
(defgeneric trie-delete (trie key))

(defun make-file-trie (directory)
  (make-instance 'file-strage-trie :directory directory))

(defun make-memory-trie ()
  (make-instance 'in-memory-trie))

(defclass trie ()
  ((da :initform (make-da))
   (data)))

(defclass file-strage-trie (trie)
  ())

(defclass in-memory-trie (trie)
  ())


(defmethod trie-open (trie)
  (with-slots (data) trie
    (data-open data)))

(defmethod trie-close (trie)
  (with-slots (data) trie
    (data-close data)))


(defmethod initialize-instance :after ((trie file-strage-trie) &key directory)
  (with-slots (data) trie
    (ensure-directories-exist directory)
    (setf data (make-instance 'file-data-strage :directory directory))))

(defmethod initialize-instance :after ((trie in-memory-trie) &key)
  (with-slots (data) trie
    (setf data (make-instance 'in-memory-data-strage))))


(defmethod trie-put (trie key value)
  (with-slots (da data) trie
    (multiple-value-bind (key-index put-p) (da-put da key)
      (unless put-p
        ;;TODO replace じゃないとね
        (print (list key-index (da-get-base da key-index)))
        (data-delete data (- (da-get-base da key-index))))
      (da-set-base da key-index (- (data-put data value))))))

(defmethod trie-get (trie key)
  (with-slots (da data) trie
    (awhen (da-get da key)
      (data-get data (- (da-get-base da it))))))

(defmethod trie-delete (trie key)
  (with-slots (da data) trie
    (multiple-value-bind (key-index data-index) (da-delete da key)
      (when key-index
        (data-delete data (- data-index))))))


#+nil
(let ((trie (make-file-trie "/tmp/trie/")))
  (data-open (trie-data trie))
  (trie-put trie #(1) #(2))
  (trie-put trie #(1) #(1 2 3))
  (trie-get trie #(1)))
;;⇒ #(2)

#+nil
(let ((trie (make-trie)))
  (data-open (trie-data trie))
  (trie-put trie #(1) #(2))
  (trie-delete trie #(1))
  (trie-get trie #(1)))
;;⇒ NIL
