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
  ((directory :initarg :directory)))

(defclass in-memory-trie (trie)
  ())


(defmethod trie-da-path ((trie file-strage-trie))
  (merge-pathnames "da" (slot-value trie 'directory)))

(defmethod trie-flush-da ((trie file-strage-trie))
  (with-slots (da) trie
    (da-save da (trie-da-path trie))))


(defmethod trie-open (trie))

(defmethod trie-open ((trie file-strage-trie))
  (with-slots (da data) trie
    (da-load da (trie-da-path trie))
    (data-open data)))

(defmethod trie-close (trie))

(defmethod trie-close ((trie file-strage-trie))
  (with-slots (data) trie
    (trie-flush-da trie)
    (data-close data)))


(defmethod initialize-instance :after ((trie file-strage-trie) &key)
  (with-slots (data directory) trie
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
        (data-delete data (- (da-get-base da key-index))))
      (da-set-base da key-index (- (data-put data value))))))

(defmethod trie-put :after ((trie file-strage-trie) key value)
  (trie-flush-da trie))

(defmethod trie-get (trie key)
  (with-slots (da data) trie
    (awhen (da-get da key)
      (data-get data (- (da-get-base da it))))))

(defmethod trie-delete (trie key)
  (with-slots (da data) trie
    (multiple-value-bind (key-index data-index) (da-delete da key)
      (when key-index
        (data-delete data (- data-index))))))

(defmethod trie-delete :after ((trie file-strage-trie) key)
  (trie-flush-da trie))


(defmacro with-open-trie ((var trie) &body body)
  `(let ((,var ,trie))
     (trie-open ,var)
     (unwind-protect
          (progn ,@body)
       (trie-close ,var))))
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
