(in-package :x.fdatatypes)

;;(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

(declaim (inline make-inode inode-hash))
(defstruct inode
  (hash 0 :type fixnum))

(declaim (inline make-tab-ctx tab-ctx-hash-fun tab-ctx-key-test tab-ctx-val-test))
(defstruct tab-ctx
  (count 0 :type fixnum-1)
  (root (error "no root") :type inode)
  (hash-fun (error "no hash-fun") :type function)
  (key-test (error "no key-test") :type function)
  (val-test (error "no val-test") :type function))

(defun tab-ctx-add (tab-ctx key val)
  (with-slots (root count hash-fun)
      tab-ctx
    (declare (fixnum-1 count))
    (multiple-value-bind (new-root added)
        (inode-add tab-ctx root 0 (funcall hash-fun key) key val)
      (if (eq root new-root)
          tab-ctx
          (make-tab-ctx :count (if added (1+ count) count)
                        :root new-root
                        :hash-fun hash-fun
                        :key-test (tab-ctx-key-test tab-ctx)
                        :val-test (tab-ctx-val-test tab-ctx))))))

(defun tab-ctx-del (tab-ctx key)
  (with-slots (root count hash-fun)
      tab-ctx
    (declare (fixnum-1 count))
    (let ((new-root (inode-del tab-ctx root hash-fun key)))
      (if (eq root new-root)
          tab-ctx
          (make-tab-ctx :count (1- count)
                        :root (or new-root (make-empty-node))
                        :hash-fun hash-fun
                        :key-test (tab-ctx-key-test tab-ctx)
                        :val-test (tab-ctx-val-test tab-ctx))))))

(defun tab-ctx-ref (tab-ctx key)
  (let ((leaf (inode-ref tab-ctx
                         (tab-ctx-root tab-ctx)
                         (funcall (tab-ctx-hash-fun tab-ctx) key)
                         key)))
    (if leaf
        (values (leaf-node-val leaf) t)
        (values nil nil))))

(defun tab-ctx-map (tab-ctx leaf-node-fn &optional from-end)
  (inode-map (tab-ctx-root tab-ctx) leaf-node-fn from-end))


(defstruct (tab-ctx-iterator (:constructor %make-tab-ctx-iterator)) cursor)

(defun tab-ctx-iterator (tab-ctx)
  (%make-tab-ctx-iterator :cursor (tab-ctx-root tab-ctx)))

(labels ((nodes (elem)
           (etypecase elem
             ((or bitmap-indexed-node full-node)
              (values (slot-value elem 'nodes) t))
             (hash-collision-node
              (values (slot-value elem 'leaves) nil))))
         (rec-down (root-path)
           (let* ((top (car root-path))
                  (idx (aref top 0))
                  (nodes (aref top 2))
                  (elem (aref nodes idx)))
             (declare (simple-vector top nodes))
             (if (leaf-node-p elem)
                 root-path
                 (multiple-value-bind (nodes nest)
                     (nodes elem)
                   (declare (simple-vector nodes))
                   (let* ((path-elem (vector 0 (length nodes) nodes))
                          (new-path (cons path-elem root-path)))
                     (if nest
                         (rec-down new-path)
                         new-path))))))
         (next-path (root-path)
           (when root-path
             (destructuring-bind (top . rest)
                 root-path
               (declare (simple-vector top))
               (let* ((idx (aref top 0))
                      (len (aref top 1))
                      (nodes (aref top 2))
                      (next-idx (1+ idx)))
                 (declare (simple-vector top nodes) (fixnum-1 idx len))
                 (if (< next-idx len)
                     (rec-down (cons (vector next-idx len nodes) rest))
                     (next-path rest)))))))
  
  ;; returns (values next-iterator key val validp)
  (defun tab-ctx-iterator-next (tab-ctx-iterator)
    (let ((cursor (tab-ctx-iterator-cursor tab-ctx-iterator)))
      (flet ((path (cursor)
               (if (listp cursor)
                   cursor
                   (let ((nodes (nodes cursor)))
                     (declare (simple-vector nodes))
                     (rec-down (list (vector 0 (length nodes) nodes)))))))
        (typecase cursor
          (empty-node
           (values nil nil nil nil))
          (leaf-node
           (values nil (leaf-node-key cursor) (leaf-node-val cursor) t))
          (t
           (let* ((path (path cursor))
                  (top (car path))
                  (idx (aref top 0))
                  (nodes (aref top 2))
                  (next-path (next-path path))
                  (node (aref nodes idx)))
             (declare (simple-vector top nodes))
             (values (when next-path
                       (%make-tab-ctx-iterator :cursor (next-path path)))
                     (leaf-node-key node)
                     (leaf-node-val node)
                     t))))))))

(defmethod iterator-next ((x tab-ctx-iterator))
  (tab-ctx-iterator-next x))

;;;;;;;;;;

(defgeneric inode-add (tab-ctx node shift hash key val)) 
(defgeneric inode-del (tab-ctx node hash key))           
(defgeneric inode-ref (tab-ctx node hash key))           
(defgeneric inode-map (node leaf-node-fn from-end))

;;;;;;;;;;

(defstruct (empty-node (:include inode)))

(defmethod inode-add ((c tab-ctx) (x empty-node) shift (hash fixnum) key val)
  (values (make-leaf-node :hash hash :key key :val val) t))

(defmethod inode-del ((c tab-ctx) (x empty-node) hash key)
  x)

(defmethod inode-ref ((c tab-ctx) (x empty-node) hash key)
  nil)

(defmethod inode-map ((x empty-node) leaf-node-fn from-end)
  nil)

;;;;;;;;;;

(declaim (inline make-leaf-node leaf-node-key leaf-node-val))
(defstruct (leaf-node (:include inode))
  (key (error "no key") :type t)
  (val (error "no val") :type t))

(defmethod inode-add ((c tab-ctx) (x leaf-node) shift (hash fixnum) key val)
  (if (eql hash (leaf-node-hash x))
      (if (funcall (tab-ctx-key-test c) key (leaf-node-key x))
          (if (funcall (tab-ctx-val-test c) val (leaf-node-val x))
              x
              (make-leaf-node :hash hash :key key :val val))
          (let ((new-leaf (make-leaf-node :hash hash :key key :val val)))
            (values (make-hash-collision-node :hash hash :leaves (vector x new-leaf)) t)))
      (create-node-with-leaf c shift x hash key val)))

(defmethod inode-del ((c tab-ctx) (x leaf-node) (hash fixnum) key)
  (unless (and (eql hash (leaf-node-hash x))
               (funcall (tab-ctx-key-test c) key (leaf-node-key x))) 
    x))

(defmethod inode-ref ((c tab-ctx) (x leaf-node) (hash fixnum) key)
  (when (and (eql hash (leaf-node-hash x))
             (funcall (tab-ctx-key-test c) key (leaf-node-key x)))
    x))

(defmethod inode-map ((x leaf-node) leaf-node-fn from-end)
  (declare (function leaf-node-fn))
  (funcall leaf-node-fn x))

;;;;;;;;;;

(declaim (inline %make-full-node make-full-node full-node-nodes full-node-shift))
(defstruct (full-node (:include inode)
                      (:constructor %make-full-node))
  (nodes (error "no nodes") :type simple-vector)
  (shift (error "no shift") :type shift))

(defun make-full-node (&key nodes shift)
  (%make-full-node :nodes nodes
                   :shift shift
                   :hash (inode-hash (svref nodes 0))))

(defmethod inode-add ((c tab-ctx) (x full-node) level-shift (hash fixnum) key val)
  (let* ((shift (full-node-shift x))
         (nodes (full-node-nodes x))
         (index (mask hash shift)))
    (multiple-value-bind (node added)
        (inode-add c (svref nodes index) (+ shift 5) hash key val)
      (values (if (eq node (svref nodes index))
                  x
                  (let ((new-nodes (copy-seq nodes)))
                    (setf (svref new-nodes index) node)
                    (make-full-node :nodes new-nodes :shift shift)))
              added))))
    
(defmethod inode-del ((c tab-ctx) (x full-node) (hash fixnum) key)
  (let* ((shift (full-node-shift x))
         (nodes (full-node-nodes x))
         (index (mask hash shift))
         (node (inode-del c (svref nodes index) hash key)))
    (declare (fixnum shift))
    (if (not (eq node (svref nodes index)))
        (if (null node)
            (make-bitmap-indexed-node :bitmap (lognot (the fixnum (bit-position hash shift)))
                                      :nodes (shrink-clone-simple-vector nodes index)
                                      :shift shift)
            (let ((new-nodes (copy-seq nodes)))
              (setf (svref new-nodes index) node)
              (make-full-node :nodes new-nodes :shift shift)))
        x)))

(defmethod inode-ref ((c tab-ctx) (x full-node) (hash fixnum) key)
  (inode-ref c (svref (full-node-nodes x) (mask hash (full-node-shift x))) hash key))

(defmethod inode-map ((x full-node) leaf-node-fn (from-end (eql nil)))
  (map nil (lambda (node)
             (inode-map node leaf-node-fn nil))
       (full-node-nodes x)))

(defmethod inode-map ((x full-node) leaf-node-fn (from-end (eql t)))
  (map-simple-vector-from-end (lambda (node)
                                (inode-map node leaf-node-fn t))
                              (full-node-nodes x)))

;;;;;;;;;;

(declaim (inline %make-bitmap-indexed-node make-bitmap-indexed-node
                 bitmap-indexed-node-bitmap bitmap-indexed-node-nodes bitmap-indexed-node-shift))
(defstruct (bitmap-indexed-node
             (:include inode)
             (:constructor %make-bitmap-indexed-node))
  (bitmap (error "no bitmap") :type fixnum)
  (nodes (error "no nodes") :type simple-vector)
  (shift (error "no shift") :type shift))
  
(defun make-bitmap-indexed-node (&key bitmap nodes shift)
  (%make-bitmap-indexed-node :bitmap bitmap
                             :nodes nodes
                             :shift shift
                             :hash (inode-hash (svref nodes 0))))

(declaim (inline create-node))
(defun create-node (bitmap nodes shift) 
  (if (eql bitmap -1)
      (make-full-node :nodes nodes :shift shift)
      (make-bitmap-indexed-node :bitmap bitmap
                                :nodes nodes
                                :shift shift)))

(declaim (inline create-node-with-leaf))
(defun create-node-with-leaf (tab-ctx shift branch hash key val)
  (let ((node (make-bitmap-indexed-node
               :bitmap (bit-position (inode-hash branch) shift)
               :nodes (vector branch)
               :shift shift)))
    (inode-add tab-ctx node shift hash key val)))

(declaim (inline index))
(defun index (node bit)
  (declare (bitmap-indexed-node node)
           (non-negative-fixnum bit))
  (let ((res (logand (bitmap-indexed-node-bitmap node) (1- bit))))
    (declare (non-negative-fixnum res))
    (logcount res)))

(defmethod inode-add ((c tab-ctx) (x bitmap-indexed-node) level-shift (hash fixnum) key val)
  (let* ((bitmap (bitmap-indexed-node-bitmap x))
         (nodes (bitmap-indexed-node-nodes x))
         (shift (bitmap-indexed-node-shift x))
         (bit (bit-position hash shift))
         (index (index x bit)))
    (declare (fixnum bitmap) (shift shift) (simple-vector nodes))
    (if (not (zerop (logand bitmap bit)))
        (multiple-value-bind (node added)
            (inode-add c (svref nodes index) (+ shift 5) hash key val)
          (values (if (eq node (svref nodes index))
                      x
                      (let ((new-nodes (copy-seq nodes)))
                        (setf (svref new-nodes index) node)
                        (make-bitmap-indexed-node :bitmap bitmap
                                                  :nodes new-nodes
                                                  :shift shift)))
                  added))
        (values (let ((new-node (make-leaf-node :hash hash :key key :val val)))
                  (create-node (logior bitmap bit)
                               (expand-clone-simple-vector nodes index new-node)
                               shift))
                t))))
                                                        
(defmethod inode-del ((c tab-ctx) (x bitmap-indexed-node) (hash fixnum) key)
  (let* ((bitmap (bitmap-indexed-node-bitmap x))
         (nodes (bitmap-indexed-node-nodes x))
         (shift (bitmap-indexed-node-shift x))
         (bit (bit-position hash shift)))
    (declare (fixnum bitmap bit))
    (if (not (zerop (logand bitmap bit)))
        (let* ((index (index x bit))
               (node (inode-del c (svref nodes index) hash key)))
          (if (eq node (svref nodes index))
              x
              (if (null node)
                  (unless (eql bitmap bit)
                    (make-bitmap-indexed-node :bitmap (logand bitmap (lognot bit))
                                              :nodes (shrink-clone-simple-vector nodes index)
                                              :shift shift))
                  (let ((new-nodes (copy-seq nodes)))
                    (setf (svref new-nodes index) node)
                    (make-bitmap-indexed-node :bitmap bitmap :nodes new-nodes :shift shift)))))
        x)))
         
(defmethod inode-ref ((c tab-ctx) (x bitmap-indexed-node) (hash fixnum) key)
  (with-slots (bitmap nodes shift)
      x
    (let ((bit (bit-position hash shift)))
      (declare (fixnum bitmap bit))
      (unless (zerop (logand bitmap bit))
        (inode-ref c (svref nodes (index x bit)) hash key)))))
  
(defmethod inode-map ((x bitmap-indexed-node) leaf-node-fn (from-end (eql nil)))
  (map nil (lambda (node)
             (inode-map node leaf-node-fn nil))
       (bitmap-indexed-node-nodes x)))

(defmethod inode-map ((x bitmap-indexed-node) leaf-node-fn (from-end (eql t)))
  (map-simple-vector-from-end (lambda (node)
                                (inode-map node leaf-node-fn t))
                              (bitmap-indexed-node-nodes x)))

;;;;;;;;;;

(declaim (inline make-hash-collision-node hash-collision-node-leaves))
(defstruct (hash-collision-node (:include inode))
  (leaves (error "no leaves") :type simple-vector))

(declaim (inline mask))
(defun find-index (tab-ctx node hash key)
  (declare (hash-collision-node node) (fixnum hash))
  (let ((leaves (hash-collision-node-leaves node)))
    (declare (simple-vector leaves))
    (dotimes (i (length leaves))
      (when (inode-ref tab-ctx (svref leaves i) hash key)
        (return-from find-index i)))
    -1))

(defmethod inode-add ((c tab-ctx) (x hash-collision-node) shift (hash fixnum) key val)
  (if (eql hash (hash-collision-node-hash x))
      (let ((leaves (hash-collision-node-leaves x))
            (index (find-index c x hash key)))
        (declare (simple-vector leaves))
        (if (not (eql index -1))
            (if (funcall (tab-ctx-val-test c) val (svref leaves index)) 
                x
                (let ((new-leaves (copy-seq leaves)))
                  (setf (svref new-leaves index)
                        (make-leaf-node :hash hash :key key :val val))
                  (make-hash-collision-node :hash hash :leaves new-leaves)))
            (let* ((length (length leaves))
                   (new-leaves (make-array (1+ length))))
              (replace new-leaves leaves)
              (setf (svref new-leaves length)
                    (make-leaf-node :hash hash :key key :val val))
              (values (make-hash-collision-node :hash hash :leaves new-leaves) t))))
      (create-node-with-leaf c shift x hash key val)))

(defmethod inode-del ((c tab-ctx) (x hash-collision-node) (hash fixnum) key)
  (let ((leaves (hash-collision-node-leaves x))
        (index (find-index c x hash key)))
    (declare (simple-vector leaves))
    (if (eql index -1)
        x
        (if (eql (length leaves) 2)
            (svref leaves (if (zerop index) 1 0))
            (make-hash-collision-node :hash hash
                                      :leaves (shrink-clone-simple-vector leaves index))))))
                
(defmethod inode-ref ((c tab-ctx) (x hash-collision-node) (hash fixnum) key)
  (let ((index (find-index c x hash key)))
    (unless (eql index -1)
      (svref (hash-collision-node-leaves x) index))))

(defmethod inode-map ((x hash-collision-node) leaf-node-fn (from-end (eql nil)))
  (map nil (lambda (node)
             (inode-map node leaf-node-fn nil))
       (hash-collision-node-leaves x)))

(defmethod inode-map ((x hash-collision-node) leaf-node-fn (from-end (eql t)))
  (map-simple-vector-from-end (lambda (node)
                                (inode-map node leaf-node-fn t))
                              (hash-collision-node-leaves x)))

;;;;;;;;;;

