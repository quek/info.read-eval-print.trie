(in-package :info.read-eval-print.trie)

(defconstant +da-signature+ #xDAFCDAFC)

(defconstant +da-pool-begin+ 3)

(defconstant +trie-index-max+ #x7fffffff)

(defconstant +da-key-end+ 256)
(defconstant +trie-char-max+ +da-key-end+)


(defconstant +trie-index-error+ most-negative-fixnum)

(defstruct da
  (cells (make-array 3 :initial-contents `((,+da-signature+ . ,+da-pool-begin+)
                                           (-1 . -1)
                                           (,+da-pool-begin+ . 0)))))

(define-condition da-error (error) ())
(define-condition da-extend-pool-error (da-error) ())

(defun da-size (da)
  (length (da-cells da)))

(defun da-get-root (da)
  (declare (ignore da))
  2)

(defun da-get-base (da s)
  (car (aref (da-cells da) s)))

(defun da-get-check (da s)
  (cdr (aref (da-cells da) s)))

(defun da-set-base (da s val)
  (when (< s (da-size da))
    (setf (car (aref (da-cells da) s)) val)))

(defun da-set-check (da s val)
  (when (< s (da-size da))
    (setf (cdr (aref (da-cells da) s)) val)))

(defun da-walk (da s c)
  (let ((next (+ (da-get-base da s) c)))
    (if (and (not (minusp next))
             (< next (da-size da))
             (= (da-get-check da next) s))
        next
        nil)))

(defun da-insert-branch (da s c)
  (let (next
        (base (da-get-base da s)))
    (if (plusp base)
        (progn
          (setf next (+ base c))
          (when (and (< next (da-size da))
                     (= (da-get-check da next) s))
            (return-from da-insert-branch next))
          (when (or (> base +trie-index-max+)
                    (not (da-check-free-cell da next)))
            (let* ((symbols (da-output-symbols da s c))
                   (new-base (da-find-free-base da symbols)))
              (da-relocate-base da s new-base)
              (setf next (+ new-base c)))))
        (let ((new-base (da-find-free-base da (list c))))
          (da-set-base da s new-base)
          (setf next (+ new-base c))))
    (da-alloc-cell da next)
    (da-set-check da next s)
    next))

(defun da-check-free-cell (da s)
  (and (da-extend-pool da s)
       (minusp (da-get-check da s))))

(defun da-has-children-p (da s)
  (let ((base (da-get-base da s)))
    (if (>= base 0)
        (loop for c from 0 to (min +trie-char-max+ (- +trie-index-max+ base))
              thereis (= (da-get-check da (+ base c)) s)))))

(defun da-output-symbols (da s c)
  "base[s] を遷移元とする c を集める。"
  (let* ((base (da-get-base da s))
         (symbols (loop with size = (da-size da)
                        for c from 0 to (min +trie-char-max+ (- +trie-index-max+ base))
                        for cs = (+ base c)
                        if (and (< cs size) (= (da-get-check da cs) s))
                          collect c)))
    (sort (if c (cons c symbols) symbols)
          #'<=)))

(defun da-get-state-key (da s)
  (nreverse
   (loop for state = s then parent
         until (= (da-get-root da) state)
         for parent = (da-get-check da state)
         collect (- state (da-get-base da parent)))))

(defun da-get-free-list (da)
  (declare (ignore da))
  1)

(defun da-find-free-base (da symbols)
  (let ((first-sym (car symbols))
        (s (- (da-get-check da (da-get-free-list da)))))
    (loop while (and (/= s (da-get-free-list da))
                     (< s (+ first-sym +da-pool-begin+)))
          do (setf s (- (da-get-check da s))))
    (when (= s (da-get-free-list da))
      (setf s (+ first-sym +da-pool-begin+))
      (loop do (da-extend-pool da s)
            if (minusp (da-get-check da s))
              do (loop-finish)
            do (incf s)))
    (loop until (da-fit-symbols da (- s first-sym) symbols)
          do (if (= (- (da-get-check da s)) (da-get-free-list da))
                 (da-extend-pool da (da-size da)))
             (setf s (- (da-get-check da s))))
    (- s first-sym)))

(defun da-fit-symbols (da base symbols)
  (not (loop for sym in symbols
             thereis (or (> base (- +trie-index-max+ sym))
                         (not (da-check-free-cell da (+ base sym)))))))

(defun da-relocate-base (da s new-base)
  (loop with old-base = (da-get-base da s)
        for sym in (da-output-symbols da s nil)
        for old-next = (+ old-base sym)
        for new-next = (+ new-base sym)
        for old-next-base = (da-get-base da old-next)
        do (da-alloc-cell da new-next)
           (da-set-check da new-next s)
           (da-set-base da new-next old-next-base)
           (when (plusp old-next-base)
             (loop with size = (da-size da)
                   for c from 0 to (min +trie-char-max+ (- +trie-index-max+ old-next-base))
                   for cs = (+ old-next-base c)
                   if (and (< cs size) (= old-next (da-get-check da cs)))
                     do (da-set-check da (+ old-next-base c) new-next))))
  (da-set-base da s new-base))

(defun da-extend-pool (da to-index)
  (cond ((or (<= to-index 0)
             (<= +trie-index-max+ to-index))
         (error 'da-extend-pool-error))
        ((< to-index (da-size da))
         da)
        (t
         (let ((new-begin (da-size da))
               (new-cells (make-array (1+ to-index))))
           (replace new-cells (da-cells da))
           (setf (da-cells da) new-cells)
           ;; initialize new free list
           (loop for i from new-begin to to-index
                 do (setf (aref (da-cells da) i)
                          (cons (- (1- i)) (- (1+ i)))))
           (let ((free-tail (- (da-get-base da (da-get-free-list da)))))
             (da-set-check da free-tail (- new-begin))
             (da-set-base da new-begin (- free-tail))
             (da-set-check da to-index (- (da-get-free-list da)))
             (da-set-base da (da-get-free-list da) (- to-index)))
           ;; update header cell
           (da-set-check da 0 (da-size da))
           da))))

(defun da-prune (da s)
  (da-prune-upto da (da-get-root da) s))

(defun da-prune-upto (da p s)
  (loop for i = s then parent
        while (and (/= p i)
                   (not (da-has-children-p da i)))
        for parent = (da-get-check da i)
        do (da-free-cell da i)))

(defun da-alloc-cell (da cell)
  (let ((prev (- (da-get-base da cell)))
        (next (- (da-get-check da cell))))
    (da-set-check da prev (- next))
    (da-set-base da next (- prev))))

(defun da-free-cell (da cell)
  (loop for i = (- (da-get-check da (da-get-free-list da))) then (- (da-get-check da i))
        while (and (/= i (da-get-free-list da))
                   (< i cell))))

(defun da-enumerate (da fun user-data)
  (da-enumerate-recursive da (da-get-root da) fun user-data))

(defun da-enumerate-recursive (da state fun user-data)
  (let ((base (da-get-base da state)))
    (if (< base 0)
        (funcall fun (da-get-state-key da state) state user-data)
        (loop for sym in (da-output-symbols da state nil)
              always(da-enumerate-recursive da (+ base sym) fun user-data)))))


(defun da-get (da key)
  (let ((s (da-get-root da)))
    (and (loop for c across key
               always (setf s (da-walk da s c)))
         (da-walk da s +da-key-end+))))

(defun da-put (da key)
  (let ((s (da-get-root da))
        (i 0)
        (key-length (length key)))
    (loop while (< i key-length)
          for x = (da-walk da s (aref key i))
          while x
          do (setf s x)
             (incf i))
    (loop while (< i key-length)
          do (setf s (da-insert-branch da s (aref key i)))
             (incf i))
    (da-insert-branch da s +da-key-end+)))


#+nil
(let ((da (make-da)))
  ;; (da-put da #(1))
  (da-insert-branch da (da-get-root da) 1)
  da)
