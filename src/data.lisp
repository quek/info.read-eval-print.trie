(in-package :info.read-eval-print.trie)


(defgeneric data-get (data-strage index))
(defgeneric data-put (data-strage data))
(defgeneric data-delete (data-strage index))
(defgeneric data-open (data-strage))
(defgeneric data-close (data-strage))


(defclass data-strage ()
  ((segments)))

(defclass file-data-strage (data-strage)
  ((directory :initarg :directory)))


(defmethod initialize-instance :after ((file-data-strage file-data-strage) &key)
  (with-slots (directory segments) file-data-strage
    (setf segments (coerce (loop for i from 3 to 32
                                 collect (make-instance 'file-segment
                                                        :size (expt 2 i)
                                                        :directory directory))
                           'vector))))

(defmethod data-open (data-strage)
  (with-slots (segments) data-strage
    (loop for segment across segments
          do (segment-open segment))))

(defmethod data-close (data-strage)
  (with-slots (segments) data-strage
    (loop for segment across segments
          do (segment-close segment))))

(defmethod data-get (data-strage index)
  (with-slots (segments) data-strage
    (multiple-value-bind (segment-index data-index) (decode-index index)
      (segment-get (aref segments segment-index)
                   data-index))))

(defmethod data-put (data-strage data)
  (with-slots (segments) data-strage
    (loop with data-size = (length data)
          for segment-index from 0
          for segment across segments
          if (<= data-size (segment-size segment))
            do (return-from data-put
                 (encode-index segment-index (segment-put segment data))))))

(defmethod data-delete (data-strage index)
  (with-slots (segments) data-strage
   (multiple-value-bind (segment-index data-index) (decode-index index)
     (segment-delete (aref segments segment-index) data-index))))



(defgeneric segment-size (segment))
(defgeneric segment-put (segment data))
(defgeneric segment-get (segment index))
(defgeneric segment-delete (segment index))
(defgeneric segment-open (segment))
(defgeneric segment-close (segment))

(defgeneric free-index (segment))


(defclass segment ()
  ((size :initarg :size :reader segment-size)))

(defclass file-segment (segment)
  ((length-size :initarg :length-size)
   (path :initarg :path)
   (stream)))


(defmethod initialize-instance :after ((segment file-segment) &key size directory)
  (with-slots (length-size path) segment
    (setf length-size (ceiling (integer-length size) 8)
          path (merge-pathnames (princ-to-string size) directory))))

(defmethod segment-open ((segment file-segment))
  (with-slots (path stream) segment
    (setf stream (open path :direction :io :element-type '(unsigned-byte 8) :if-exists :overwrite
                                       :if-does-not-exist :create))
    (when (zerop (file-length stream))
      (write-u4 stream 0))))

(defmethod segment-close ((segment file-segment))
  (with-slots (stream) segment
    (close stream)))

(declaim (inline read-un))
(defun read-un (stream n)
  (locally (declare #.*fastest*)
    (loop for i fixnum from 0 below n
          for x fixnum = (read-byte stream)
          sum (the fixnum (ash x (the fixnum (* i 8)))) fixnum)))

(defun read-u4 (stream)
  (read-un stream 4))

(defun write-u4 (stream u4)
  (loop for i from 0 to 3
        for n = u4 then (ash n -8)
        do (write-byte (logand n #xff) stream)))

(defmethod read-free-index ((segment file-segment))
  (with-slots (stream) segment
    (file-position stream 0)
    (read-u4 stream)))

(defmethod write-free-index ((segment file-segment) index)
  (with-slots (stream) segment
    (file-position stream 0)
    (write-u4 stream index)))

(defun read-data-length (stream length-size)
  (read-un stream length-size))

(defun read-data (stream length-size)
  (let ((buffer (make-array (read-data-length stream length-size) :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    buffer))

(defun write-data-length (stream data length-size)
  (loop for i from 0 below length-size
        for data-length = (length data) then (ash data-length -8)
        do (write-byte (logand data-length #xff) stream)))

(defun write-length-and-data (stream data length-size &key fill)
  (write-data-length stream data length-size)
  (write-sequence data stream)
  (when fill
    (loop repeat (- fill (length data))
          do (write-byte 0 stream))))

(defun file-position-to-index (file-position segment-size length-size)
  (/ (- file-position 4)                ;4 is free-index
     (+ segment-size length-size)))

(defun index-to-file-position (index segment-size length-size)
  (+ 4                                  ;4 is free-index
     (* index (+ segment-size length-size))))

(defmethod segment-put ((segment file-segment) data)
  (with-slots (size length-size stream) segment
    (let ((free-index (read-free-index segment)))
      (if (zerop free-index)
          (let ((file-length (file-length stream)))
            (file-position stream file-length)
            (write-length-and-data stream data length-size :fill size)
            (file-position-to-index file-length size length-size))
          (let ((position (index-to-file-position free-index size length-size)))
            (file-position stream position)
            (let ((next-free-index (read-u4 stream)))
              (file-position stream position)
              (write-length-and-data stream data length-size)
              (write-free-index segment next-free-index)
              free-index))))))

(defmethod segment-get ((segment file-segment) index)
  (with-slots (size length-size stream) segment
    (file-position stream (index-to-file-position index size length-size))
    (read-data stream length-size)))

(defmethod segment-delete ((segment file-segment) index)
  (with-slots (size length-size stream) segment
    (let ((free-index (read-free-index segment)))
      (write-free-index segment index)
      (file-position stream (index-to-file-position index size length-size))
      (write-u4 stream free-index))))



(defun decode-index (index)
  (values (logand index #x1f)
          (ash index -5)))

(defun encode-index (segment-index data-index)
  (+ (ash data-index 5)
     segment-index))
