(in-package :x.fdatatypes)

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct vec-ctx
    (count 0 :type fixnum-1)
    (shift (error "no shift") :type shift)
    (root (error "no root") :type simple-vector)
    (tail (error "no tail") :type simple-vector)))

(define-constant +vec-ctx-shift+ 5)
(define-constant +vec-ctx-block-size+ (expt 2 +vec-ctx-shift+))
(define-constant +empty-vec+ #() :test 'equalp)
(define-constant +empty-vec-ctx+ (make-vec-ctx :shift +vec-ctx-shift+
                                               :root #()
                                               :tail #())
  :test 'equalp)


;;;;

(defstruct (vec-ctx-iterator (:constructor %make-vec-ctx-iterator))
  root-path tail index)

(defun vec-ctx-iterator (vec-ctx)
  (let* (((:slotval root tail shift) vec-ctx))
    (declare (fixnum-1 shift))
    (%make-vec-ctx-iterator
     :root-path root
     :tail tail
     :index (truncate shift +vec-ctx-shift+))))  ;; we pass levels in index slot first time

(labels ((rec-down (root-path level)
           (declare (fixnum-1 level))
           (if (zerop level)
               root-path
               (let* ((top (car root-path))
                      (#(idx _ nodes) top)
                      (nodes-1 (aref nodes idx)))
                 (declare (simple-vector top nodes nodes-1))
                 (rec-down (cons (vector 0 (length nodes-1) nodes-1)
                                 root-path)
                           (1- level)))))
         (next-root-path (root-path &optional (level 0))
           (declare (fixnum-1 level))
           (when root-path
             (let* (((top . rest) root-path)
                    (#(idx len nodes) top)
                    (next-idx (1+ idx)))
               (declare (simple-vector top nodes) (fixnum-1 idx len))
               (if (< next-idx len)
                   (rec-down (cons (vector next-idx len nodes) rest) level)
                   (next-root-path rest (1+ level))))))
         (next-path (root-path tail)
           (let ((next-root-path (next-root-path root-path)))
             (if next-root-path
                 (values next-root-path tail)
                 (if tail
                     (values (list (vector 0 (length (the simple-vector tail)) tail)) nil)
                     (values nil nil)))))
         (first-path (root-path tail levels)
           (let ((empty-tail-p (equalp tail +empty-vec+)))
             (if (equalp root-path +empty-vec+)
                 (if empty-tail-p
                     (values nil nil)
                     (values (list (vector 0 (length (the simple-vector tail)) tail)) nil))
                 (values (rec-down (list (vector 0 (length root-path) root-path)) levels)
                         (if empty-tail-p nil tail))))))

  ;; returns (values next-iterator key val validp)
  (defun vec-ctx-iterator-next (vec-ctx-iterator)
    (let* (((:slotval root-path tail index) vec-ctx-iterator))
      (when (vectorp root-path)
        (multiple-value-setq (root-path tail)
          (first-path root-path tail index))
        (setf index 0))
      (if root-path
          (let* ((top (car root-path))
                 (#(idx _ nodes) top)
                 ((:mval next-root-path next-tail) (next-path root-path tail)))
            (declare (simple-vector top) (integer index))
            (values 
             (when next-root-path
               (%make-vec-ctx-iterator :root-path next-root-path
                                       :tail next-tail
                                       :index (1+ index)))
             index
             (svref nodes idx)
             t))
          (values nil nil nil nil)))))

(defmethod iterator-next ((x vec-ctx-iterator))
  (vec-ctx-iterator-next x))

;;;;;;;;;;

(defun tail-off (vec-ctx)
  (- (vec-ctx-count vec-ctx) (length (vec-ctx-tail vec-ctx))))

(defun vec-ctx-ref (vec-ctx key)
  (declare (fixnum-1 key))
  (let* (((:slotval count shift tail (array root)) vec-ctx))
    (declare (fixnum-1 count) (shift shift))
    (if (and (>= key 0) (< key count))
        (if (>= key (tail-off vec-ctx))
            (svref tail (logand key #x1F))
            (loop :for level :from shift :above 0 :by +vec-ctx-shift+
               :do (setf array (svref array (mask key level)))
               :finally (return (svref array (logand key #x1f)))))
        (error 'out-of-bounds :bad-index key :limit count))))

(defun vec-ctx-update (vec-ctx index val)
  (declare (fixnum-1 index))
  (let* (((:slotval count shift tail root) vec-ctx))
    (declare (simple-vector tail) (fixnum-1 count))
    (cond ((and (>= index 0) (< index count))
           (if (>= index (tail-off vec-ctx))
               (let* ((length (length tail))
                      (new-tail (make-array length)))
                 (replace new-tail tail)
                 (setf (svref new-tail (logand index #x1f)) val)
                 (make-vec-ctx :count count :shift shift :root root :tail new-tail))
               (labels ((rec (level array)
                          (declare (simple-vector array) (fixnum-1 level))
                          (let ((new-array (copy-seq array)))
                            (if (zerop level)
                                (setf (svref new-array (logand index #x1f)) val)
                                (let ((sub-index (mask index level)))
                                  (setf (svref new-array sub-index)
                                        (rec (- level +vec-ctx-shift+) (svref array sub-index)))))
                            new-array)))
                 (make-vec-ctx :count count :shift shift :root (rec shift root) :tail tail))))
          (t
           (error 'out-of-bounds :bad-index index :limit count)))))

(labels ((push-tail (vec-ctx level root tail)
           (declare (simple-vector root) (fixnum-1 level))
           (let* ((new-child nil)
                  (expansion nil)
                  (root-length (length root)))
             (if (zerop level)
                 (setf new-child tail)
                 (progn
                   (multiple-value-setq (new-child expansion)
                     (push-tail vec-ctx (- level +vec-ctx-shift+) (svref root (1- root-length)) tail))
                   (if (null expansion)
                       (let ((new-root (copy-seq root)))
                         (setf (svref new-root (1- root-length)) new-child)
                         (return-from push-tail new-root))
                       (setf new-child expansion))))
             (if (eql root-length +vec-ctx-block-size+)
                 (values root (vector new-child))
                 (expand-clone-simple-vector root root-length new-child)))))

  (defun vec-ctx-add-tail (vec-ctx val)
    (let* (((:slotval count shift tail root) vec-ctx)
           (tail-length (length tail)))
      (declare (simple-vector tail) (fixnum-1 count) (shift shift))
      (if (< tail-length +vec-ctx-block-size+)
          (make-vec-ctx :count (1+ count)
                        :shift shift
                        :root root
                        :tail (expand-clone-simple-vector tail tail-length val))
          (let* (((:mval new-root expansion) (push-tail vec-ctx (- shift +vec-ctx-shift+) root tail))
                 (new-shift shift))
            (unless (null expansion)
              (setf new-root (vector new-root expansion))
              (incf new-shift +vec-ctx-shift+))
            (make-vec-ctx :count (1+ count) :shift new-shift :root new-root :tail (vector val))))))

  (defun vec-ctx-add (vec-ctx index val)
    (declare (fixnum index))
    (let* (((:slotval count shift root tail) vec-ctx))
      (declare (fixnum-1 count) (shift shift) (simple-vector tail))
      (when (or (> index count) (< index 0))
        (error 'out-of-bounds :bad-index index :limit (1+ count)))
      (if (>= index (tail-off vec-ctx))
          (if (eql index count)
              (vec-ctx-add-tail vec-ctx val)
              (let* ((chunk-idx (rem index +vec-ctx-block-size+)))
                (if (eql (length tail) +vec-ctx-block-size+)
                    (let ((new-tail (make-array +vec-ctx-block-size+))
                          (last-val (svref tail 31)))
                      (replace new-tail tail :end2 chunk-idx)
                      (setf (svref new-tail chunk-idx) val)
                      (replace new-tail tail :start1 (1+ chunk-idx) :start2 chunk-idx :end2 31)
                      (let* (((:mval new-root expansion) (push-tail vec-ctx (- shift +vec-ctx-shift+) root new-tail))
                             (new-shift shift))
                        (unless (null expansion)
                          (setf new-root (vector new-root expansion))
                          (incf new-shift +vec-ctx-shift+))
                        (make-vec-ctx :count (1+ count) :shift new-shift :root new-root :tail (vector last-val))))
                    (make-vec-ctx :count (1+ count)
                                  :shift shift
                                  :root root
                                  :tail (expand-clone-simple-vector tail chunk-idx val)))))
          (vec-ctx-add*-helper vec-ctx (list index) (list val))))))

(defun vec-ctx-add-tail* (vec-ctx contents)
  (loop :for x :in contents
     :do (setf vec-ctx (vec-ctx-add-tail vec-ctx x)))
  vec-ctx)

(labels ((pop-tail (vec-ctx shift root)
           (declare (simple-vector root) (shift shift))
           (let ((ptail nil)
                 (root-length (length root)))
             (when (> shift 0)
               (let (new-child)
                 (multiple-value-setq (new-child ptail)
                   (pop-tail vec-ctx (- shift +vec-ctx-shift+) (svref root (1- root-length))))
                 (unless (null new-child)
                   (let ((new-root (copy-seq root)))
                     (setf (svref new-root (1- root-length)) new-child)
                     (return-from pop-tail (values new-root ptail))))))
             (when (zerop shift)
               (setf ptail (svref root (1- root-length))))
             (when (eql root-length 1)
               (return-from pop-tail (values nil ptail)))
             (values (shrink-clone-simple-vector root (1- root-length)) ptail))))

  (defun vec-ctx-del-tail (vec-ctx)
    (let* (((:slotval count shift tail root) vec-ctx)
           (tail-length (length tail)))
      (declare (simple-vector tail) (fixnum-1 count) (shift shift))
      (cond ((zerop count)
             (error 'container-empty))
            ((eql count 1)
             +empty-vec-ctx+)
            ((> tail-length 1)
             (let ((new-tail (shrink-clone-simple-vector tail (1- tail-length))))
               (make-vec-ctx :count (1- count) :shift shift :root root :tail new-tail)))
            (t
             (let* (((:mval new-root ptail) (pop-tail vec-ctx (- shift +vec-ctx-shift+) root))
                    (new-shift shift))
               (declare ((or null simple-vector) new-root))
               (when (null new-root)
                 (setf new-root +empty-vec+))
               (when (and (> shift +vec-ctx-shift+) (eql (length (the simple-vector new-root)) 1))
                 (setf new-root (svref new-root 0))
                 (decf new-shift +vec-ctx-shift+))
               (make-vec-ctx :count (1- count) :shift new-shift :root new-root :tail ptail)))))))

(defun vec-ctx-iota (n &optional (start 0) (step 1))
  (declare (fixnum-1 n))
  (let ((v +empty-vec-ctx+))
    (loop :for x :from start :below n :by step
       :do (setf v (vec-ctx-add-tail v x)))
    v))

(defun vec-ctx-map-chunks (vec-ctx function)
  (declare (function function))
  (let* (((:slotval shift tail root) vec-ctx)
         (id 0))
    (declare (fixnum-1 id) (shift shift))
    (labels ((rec (array level)
               (declare (simple-vector array) (fixnum-1 level))
               (if (zerop level)
                   (loop :for x :across array
                      :do (funcall function t (prog1 id (incf id)) x))
                   (loop :for x :across array :do (rec x (1- level))))))
      (rec root (1- (truncate shift +vec-ctx-shift+)))
      (funcall function nil id tail)
      nil)))

(defun vec-ctx-map (vec-ctx function &optional from-end)
  (if from-end
      (let* (((:slotval shift tail root) vec-ctx))
        (declare (shift shift))
        (labels ((map-backwards (array function)
                   (declare (simple-vector array) (function function))
                   (loop :for i :from (1- (length array)) :downto 0
                      :do (funcall function (svref array i))))
                 (rec (array level)
                   (declare (fixnum-1 level))
                   (map-backwards array (if (zerop level)
                                            function
                                            (lambda (x) (rec x (1- level)))))))
          (map-backwards tail function)
          (rec root (truncate shift +vec-ctx-shift+))
          nil))
      (flet ((fn (not-tail id chunk)
               (declare (simple-vector chunk) (function function)
                        (ignore id not-tail))
               (map nil function chunk)))
        (vec-ctx-map-chunks vec-ctx #'fn))))

(defun chunk-indexes (chunk-id indexes)
  (declare (fixnum-1 chunk-id))
  (let ((start (* chunk-id +vec-ctx-block-size+)))
    (declare (fixnum-1 start chunk-id))
    (unless (< (the fixnum (car indexes)) start)
      (let ((end (+ start +vec-ctx-block-size+))
            (result '())
            (result-length 0))
        (tagbody
           (mapl (lambda (idxs)
                   (let ((idx (car idxs)))
                     (declare (fixnum idx))
                     (cond ((< idx end) 
                            (push idx result)
                            (incf result-length))
                           (t
                            (setf indexes idxs)
                            (go :out)))))
                 indexes)
           (setf indexes nil)
         :out
           (map-into result (lambda (i) (rem (the fixnum i) +vec-ctx-block-size+)) result)
           (return-from chunk-indexes
             (values (nreverse result) indexes result-length)))))))

(defstruct (chunk-buffer (:copier nil))
  (array nil :type (or null simple-vector))
  (index 0 :type (mod 33)))

(defun ensure-chunk-buffer (buf)
  (when (null (chunk-buffer-array buf))
    (setf (chunk-buffer-array buf) (make-array +vec-ctx-block-size+)
          (chunk-buffer-index buf) 0))
  (- +vec-ctx-block-size+ (chunk-buffer-index buf)))

(defun copy-chunk-buffer (buf collect-fn chunk start end)
  (declare (chunk-buffer buf)
           (function collect-fn)
           (simple-vector chunk)
           ((mod 33) start end))
  (cond ((eql start end)
         nil)
        ((and (null (chunk-buffer-array buf)) (eql start 0) (eql end +vec-ctx-block-size+))
         (funcall collect-fn chunk))
        (t
         (let ((remaining (ensure-chunk-buffer buf))
               (to-copy (- end start)))
           (declare ((mod 33) remaining to-copy))
           (replace (the simple-vector (chunk-buffer-array buf)) chunk
                    :start1 (chunk-buffer-index buf) :end1 +vec-ctx-block-size+
                    :start2 start :end2 end)
           (cond ((>= to-copy remaining)
                  (funcall collect-fn (chunk-buffer-array buf))
                  (setf (chunk-buffer-array buf) nil)
                  (when (> to-copy remaining)
                    (copy-chunk-buffer buf collect-fn chunk (+ start remaining) end)))
                 (t
                  (incf (chunk-buffer-index buf) to-copy)))))))
  
(defun vec-ctx-del*-build-content (vec-ctx indexes)
  (nconcing (:into content :count content-length :before-last before-last)
    (let* ((chunk-indexes nil)
           (buf (make-chunk-buffer)))
      (labels ((copy (chunk start end)
                 (copy-chunk-buffer buf #'nconc-it chunk start end))
               (fn (not-last chunk-id chunk)
                 (declare (simple-vector chunk) (ignore not-last))
                 (cond (indexes
                        (multiple-value-setq (chunk-indexes indexes)
                          (chunk-indexes chunk-id indexes))
                        (let ((start 0))
                          (dolist (i chunk-indexes)
                            (copy chunk start i)
                            (setf start (1+ i)))
                          (copy chunk start (length chunk))))
                       (t
                        (copy chunk 0 (length chunk))))))
        (vec-ctx-map-chunks vec-ctx #'fn)
        (values content content-length before-last
                (chunk-buffer-array buf) (chunk-buffer-index buf))))))

(defun build-trees-from-content (content content-length level chunk-length)
  (declare (fixnum-1 content-length chunk-length level))
  (if (< content-length chunk-length)
      (values (coerce (the list content) 'simple-vector) level)
      (let (work-chunk work-chunk-size)
        (nconcing (:into upper :count upper-count)
          (loop 
             (multiple-value-setq (work-chunk content work-chunk-size)
               (maybe-nth-split content chunk-length))
             (unless work-chunk (return))
             (nconc-it (coerce (the list work-chunk) 'simple-vector)))
          (build-trees-from-content upper upper-count (1+ level) chunk-length)))))

(defun build-vec-ctx (content content-length before-last work-array work-index)
  (declare (fixnum-1 content-length))
  (let* ((tail (if work-array
                   (if (eql work-index +vec-ctx-block-size+)
                       work-array
                       (subseq (the simple-vector work-array) 0 work-index))
                   (prog1 (or (cadr before-last) +empty-vec+)
                     (rplacd before-last nil)
                     (decf content-length)))))
    (declare (simple-vector tail) (fixnum-1 content-length))
    (if (null content)
        (make-vec-ctx :count (length tail) :shift +vec-ctx-shift+ :root +empty-vec+ :tail tail)
        (let* (((:mval root level)
                (build-trees-from-content content content-length 1 +vec-ctx-block-size+)))
          (make-vec-ctx :count (+ (* content-length +vec-ctx-block-size+) (length tail))
                        :shift (* +vec-ctx-shift+ level)
                        :root root
                        :tail tail)))))

(defun vec-ctx-del*-helper (vec-ctx indexes)
  (multiple-value-call #'build-vec-ctx (vec-ctx-del*-build-content vec-ctx indexes)))

(defun vec-ctx-del* (vec-ctx indexes)
  (declare (list indexes))
  (let* (((:slotval count) vec-ctx)
         (indexes (sort (remove-duplicates indexes) #'<))
         (last (car (last indexes))))
    (cond ((null indexes)
           (error 'invalid-arguments))
          ((< (car indexes) 0)
           (error 'out-of-bounds :bad-index (car indexes) :limit count))
          ((>= last count)
           (error 'out-of-bounds :bad-index last :limit count))
          (t
           (vec-ctx-del*-helper vec-ctx indexes)))))

(defun vec-ctx-del (vec-ctx index)
  (declare (fixnum index))
  (let* (((:slotval count) vec-ctx))
    (declare (fixnum-1 count))
    (if (and (>= index 0) (< index count))
        (vec-ctx-del*-helper vec-ctx (list index))
        (error 'out-of-bounds :bad-index index :limit count))))

(defun vec-ctx-update*-helper (vec-ctx idxs vals)
  (let* ((chunk-idxs nil)
         (chunk-vals nil)
         (chunk-idxs-length 0)
         ((:slotval count shift root tail) vec-ctx))
    (flet ((update-chunk (chunk idxs vals)
             (loop :for idx :of-type fixnum :in idxs
                :for val :in vals
                :do (setf (svref chunk (rem idx +vec-ctx-block-size+)) val))
             chunk))
      (if (>= (car idxs) (tail-off vec-ctx))
          (make-vec-ctx :count count
                        :shift shift
                        :root root
                        :tail (update-chunk (copy-seq (the simple-vector tail)) idxs vals))
          (nconcing (:into chunks :count len :before-last before-last :last last)
            (flet ((fn (not-tail chunk-id chunk)
                     (declare (ignore not-tail) (simple-vector chunk))
                     (cond ((null idxs)
                            (nconc-it chunk))
                           (t
                            (multiple-value-setq (chunk-idxs idxs chunk-idxs-length)
                              (chunk-indexes chunk-id idxs))
                            (cond ((null chunk-idxs)
                                   (nconc-it chunk))
                                  (t
                                   (multiple-value-setq (chunk-vals vals)
                                     (nth-split vals chunk-idxs-length))
                                   (nconc-it (update-chunk (copy-seq chunk)
                                                           chunk-idxs
                                                           chunk-vals))))))))
              (vec-ctx-map-chunks vec-ctx #'fn)
              (rplacd before-last nil)
              (make-vec-ctx :count count
                            :shift shift
                            :root (build-trees-from-content chunks (1- len) 1 +vec-ctx-block-size+)
                            :tail (car last))))))))

(defun vec-ctx-process-contents (vec-ctx one-elem-fn many-elems-fn contents)
  (declare (function one-elem-fn many-elems-fn))
  (if (null contents)
      vec-ctx
      (nconcing (:into pairs :count content-length)
        (map-associations (lambda (container key val)
                            (declare (ignore container))
                            (nconc-it (cons key val)))
                          contents
                          nil)
        (cond ((eql content-length 1)
               (funcall one-elem-fn vec-ctx (caar pairs) (cdar pairs)))
              (t
               (setf pairs (sort pairs #'< :key #'car))
               (let* (((:slotval count) vec-ctx)
                      (first-index (caar pairs))
                      (last-index (caar (last pairs))))
                 (cond ((< first-index 0)
                        (error 'out-of-bounds :bad-index first-index :limit count))
                       ((>= last-index count)
                        (error 'out-of-bounds :bad-index last-index :limit count))
                       (t
                        (multiple-value-call many-elems-fn
                          vec-ctx (unzip-alist pairs))))))))))

;; we could repeatedly call vec-ctx-update,
;; but we can do multiple updates less wastefully
(defun vec-ctx-update* (vec-ctx contents)
  (vec-ctx-process-contents vec-ctx #'vec-ctx-update #'vec-ctx-update*-helper contents))

(defun vec-ctx-add*-build-content (vec-ctx idxs vals)
  (nconcing (:into content :count content-length :before-last before-last)
    (let* ((buf (make-chunk-buffer))
           (tmp (vector 0))
           (chunk-idxs nil))
      (labels ((copy (chunk start end)
                 (copy-chunk-buffer buf #'nconc-it chunk start end))
               (push-val (val)
                 (setf (svref tmp 0) val)
                 (copy tmp 0 1))
               (fn (not-last chunk-id chunk)
                 (declare (ignore not-last) (simple-vector chunk))
                 (cond (idxs
                        (multiple-value-setq (chunk-idxs idxs)
                          (chunk-indexes chunk-id idxs))
                        (let ((start 0))
                          (loop :for idx :in chunk-idxs
                             :for val* :on vals
                             :do (let ((val (car val*)))
                                   (copy chunk start idx)
                                   (push-val val)
                                   (setf start idx))
                             :finally (setf vals (cdr val*)))
                          (copy chunk start (length chunk))))
                       (t
                        (copy chunk 0 (length chunk))))))
        (vec-ctx-map-chunks vec-ctx #'fn)
        (values content content-length before-last
                (chunk-buffer-array buf) (chunk-buffer-index buf))))))
    
(defun vec-ctx-add*-helper (vec-ctx idxs vals)
  (multiple-value-call #'build-vec-ctx (vec-ctx-add*-build-content vec-ctx idxs vals)))

(defun vec-ctx-add* (vec-ctx contents)
  (vec-ctx-process-contents vec-ctx #'vec-ctx-add #'vec-ctx-add*-helper contents))
