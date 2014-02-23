(in-package :info.read-eval-print.trie)

(defconstant +da-signature+ #x4AFCDAFC)

(defconstant +da-pool-begin+ 3)

(defconstant +trie-index-max+ #x7fffffff)

(defconstant +da-key-end+ 7 "全てのキーは +da-key-end+ で終わるのので 7 にして、7 は 256 にする")
(defmacro c (c)
  (let ((x (gensym)))
    `(let ((,x ,c))
       (if (= ,x +da-key-end+)
           256
           ,x))))

(defconstant +trie-char-max+ 256 "#xff + 1(+da-key-end+)")


(defconstant +trie-index-error+ most-negative-fixnum)

(defstruct (da (:constructor %make-da))
  (cells (make-array 512 :adjustable t :fill-pointer 0))
  (dirty-list ()))

(defun make-da ()
  (let ((da (%make-da)))
    (loop with cells = (da-cells da)
          for x in (list (cons +da-signature+ +da-pool-begin+)
                         (cons -1  -1)
                         (cons +da-pool-begin+ 0))
          for i from 0
          do (vector-push-extend x cells)
             (da-set-dirty da i))
    da))

(define-condition da-error (error) ())
(define-condition da-extend-pool-error (da-error) ())

(defun da-clear-dirty (da)
  (setf (da-dirty-list da) ()))

(defun da-set-dirty (da s)
  (pushnew s (da-dirty-list da)))

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
  (da-set-dirty da s)
  (setf (car (aref (da-cells da) s)) val))

(defun da-set-check (da s val)
  (da-set-dirty da s)
  (setf (cdr (aref (da-cells da) s)) val))

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
          (when (or (> (+ base c) +trie-index-max+)
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

(defun max-check-state-at-base (da base)
  (min (+ base +trie-char-max+) (1- (da-size da))))

(defun da-has-children-p (da s)
  (let ((base (da-get-base da s)))
    (and (plusp base)
         (loop for i from base to (max-check-state-at-base da base)
               thereis (= (da-get-check da i) s)))))

(defun da-output-symbols (da s c)
  "base[s] を遷移元とする c を集める。"
  (let* ((base (da-get-base da s))
         (symbols (loop for i from base to (max-check-state-at-base da base)
                        if (= (da-get-check da i) s)
                          collect (- i base))))
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
             (loop for i from old-next-base to (max-check-state-at-base da old-next-base)
                   if (= old-next (da-get-check da i))
                     do (da-set-check da i new-next)))
           (da-free-cell da old-next))
  (da-set-base da s new-base))

(defun da-extend-pool (da to-index)
  (cond ((or (<= to-index 0)
             (<= +trie-index-max+ to-index))
         (error 'da-extend-pool-error))
        ((< to-index (da-size da))
         da)
        (t
         (let ((new-begin (da-size da)))
           ;; initialize new free list
           (loop with cells = (da-cells da)
                 for i from new-begin to to-index
                 do (vector-push-extend (cons (- (1- i)) (- (1+ i))) cells)
                    (da-set-dirty da i))
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
        until (or (= p i)
                  (da-has-children-p da i))
        for parent = (da-get-check da i)
        do (da-free-cell da i)))

(defun da-alloc-cell (da cell)
  (let ((prev (- (da-get-base da cell)))
        (next (- (da-get-check da cell))))
    (da-set-check da prev (- next))
    (da-set-base da next (- prev))))

(defun da-free-cell (da cell)
  (loop with free = (da-get-free-list da)
        for i = (- (da-get-check da free)) then (- (da-get-check da i))
        while (and (/= i free)
                   (< i cell))
        finally (let ((prev (- (da-get-base da i))))
                  (da-set-base da cell (- prev))
                  (da-set-check da cell (- i))
                  (da-set-base da i (- cell))
                  (da-set-check da prev (- cell)))))

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
    (and (loop for x across key
               for c = (c x)
               always (setf s (da-walk da s c)))
         (da-walk da s +da-key-end+))))

(defun da-put (da key)
  (let ((s (da-get-root da))
        (i 0)
        (key-length (length key)))
    (loop while (< i key-length)
          for c = (c (aref key i))
          for x = (da-walk da s c)
          while x
          do (setf s x)
             (incf i)
          finally (when x
                    (awhen (da-walk da x +da-key-end+)
                      (return-from da-put (values it nil)))))
    (loop while (< i key-length)
          for c = (c (aref key i))
          do (setf s (da-insert-branch da s c))
             (incf i))
    (values (da-insert-branch da s +da-key-end+) t)))

(defun da-delete (da key)
  (awhen (da-get da key)
    (let ((data-index (da-get-base da it)))
      (da-set-base da it 0)
      (da-prune da it)
      (values it data-index))))


(defun da-save (da path)
  (with-open-file (out path :direction :output
                            :element-type '(signed-byte 32)
                            :if-exists :overwrite
                                       :if-does-not-exist :create)
    (loop with cells = (da-cells da)
          for s in (sort (da-dirty-list da) #'<=)
          for cell = (aref cells s)
          do (file-position out (* 2 s))
             (write-byte (car cell) out)
             (write-byte (cdr cell) out))
    (da-clear-dirty da)))

(defun da-load (da path)
  (if (probe-file path)
      (let ((cells (da-cells da)))
        (setf (fill-pointer cells) 0)
        (with-open-file (in path :direction :input
                                 :element-type '(signed-byte 32))
          (loop for a = (read-byte in nil)
                for b = (read-byte in nil)
                while a
                do (vector-push-extend (cons a b) cells))
          (da-clear-dirty da)
          da))))