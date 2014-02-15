(in-package :info.read-eval-print.trie)

(defstruct segment
  size
  (array (make-array 1 :adjustable t :fill-pointer t
                     :initial-contents (list 0))))

(defun segment-put (segment data)
  (let* ((array (segment-array segment))
         (free-index (aref array 0)))
    (if (zerop free-index)
        (progn
          (vector-push-extend data array)
          (1- (length array)))
        (progn
          (setf (aref array 0) (aref array free-index)
                (aref array free-index) data)
          free-index))))

(defun segment-get (segment index)
  (aref (segment-array segment) index))

(defun segment-delete (segment index)
  (let ((array (segment-array segment)))
    (setf (aref array index) (aref array 0)
          (aref array 0) index)))


(defstruct data-strage
  (segments (coerce (loop for i from 3 to 32
                          collect (make-segment :size (expt 2 i)))
                    'vector)))


(defun decode-index (index)
  (values (logand index #x1f)
          (ash index -5)))

(defun encode-index (segment-index data-index)
  (+ (ash data-index 5)
     segment-index))

(defun data-get (data-strage index)
  (multiple-value-bind (segment-index data-index) (decode-index index)
    (segment-get (aref (data-strage-segments data-strage) segment-index)
                 data-index)))

(defun data-put (data-strage data)
  (loop with data-size = (length data)
        for segment-index from 0
        for segment across (data-strage-segments data-strage)
        if (<= data-size (segment-size segment))
          do (return-from data-put
               (encode-index segment-index (segment-put segment data)))))

(defun data-delete (data-strage index)
  (multiple-value-bind (segment-index data-index) (decode-index index)
    (segment-delete (aref (data-strage-segments data-strage) segment-index)
                    data-index)))

#+nil
(let ((x (make-data-strage))
      i)
  (setf i (data-put x #(1 2 3)))
  (data-get x i)
  (setf i (data-put x (make-array 1024)))
  (data-get x i)
  x)

