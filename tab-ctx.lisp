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
  (let* (((:slotval root count) tab-ctx)
         ((:mval new-root added) (inode-add tab-ctx
                                            root
                                            0
                                            (funcall (tab-ctx-hash-fun tab-ctx) key)
                                            key
                                            val)))
    (declare (fixnum-1 count))
    (if (eq root new-root)
        tab-ctx
        (make-tab-ctx :count (if added (1+ count) count)
                      :root new-root
                      :hash-fun (tab-ctx-hash-fun tab-ctx)
                      :key-test (tab-ctx-key-test tab-ctx)
                      :val-test (tab-ctx-val-test tab-ctx)))))

(defun tab-ctx-del (tab-ctx key)
  (let* (((:slotval root count) tab-ctx)
         (new-root (inode-del tab-ctx
                              root
                              (funcall (tab-ctx-hash-fun tab-ctx) key)
                              key)))
    (declare (fixnum-1 count))
    (if (eq root new-root)
        tab-ctx
        (make-tab-ctx :count (1- count)
                      :root (or new-root (make-empty-node))
                      :hash-fun (tab-ctx-hash-fun tab-ctx)
                      :key-test (tab-ctx-key-test tab-ctx)
                      :val-test (tab-ctx-val-test tab-ctx)))))

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
  (let* (((:slotval shift nodes) x)
         (index (mask hash shift))
         ((:mval node added) (inode-add c (svref nodes index) (+ shift 5) hash key val)))
    (values (if (eq node (svref nodes index))
                x
                (let ((new-nodes (copy-seq nodes)))
                  (setf (svref new-nodes index) node)
                  (make-full-node :nodes new-nodes :shift shift)))
            added)))
    
(defmethod inode-del ((c tab-ctx) (x full-node) (hash fixnum) key)
  (let* (((:slotval nodes shift) x)
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

(declaim (inline mask))
(defun create-node (bitmap nodes shift) 
  (if (eql bitmap -1)
      (make-full-node :nodes nodes :shift shift)
      (make-bitmap-indexed-node :bitmap bitmap
                                :nodes nodes
                                :shift shift)))

(declaim (inline mask))
(defun create-node-with-leaf (tab-ctx shift branch hash key val)
  (let ((node (make-bitmap-indexed-node
               :bitmap (bit-position (inode-hash branch) shift)
               :nodes (vector branch)
               :shift shift)))
    (inode-add tab-ctx node shift hash key val)))

(declaim (inline mask))
(defun index (node bit)
  (declare (bitmap-indexed-node node)
           (non-negative-fixnum bit))
  (let* (((:slotval bitmap) node)
         (res (logand bitmap (1- bit))))
    (declare (fixnum bitmap) (non-negative-fixnum res))
    (logcount res)))

(defmethod inode-add ((c tab-ctx) (x bitmap-indexed-node) level-shift (hash fixnum) key val)
  (let* (((:slotval bitmap nodes shift) x)
         (bit (bit-position hash shift))
         (index (index x bit)))
    (declare (fixnum bitmap) (shift shift) (simple-vector nodes))
    (if (not (zerop (logand bitmap bit)))
        (let* (((:mval node added) (inode-add c (svref nodes index) (+ shift 5) hash key val)))
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
  (let* (((:slotval bitmap nodes shift) x)
         (bit (bit-position hash shift)))
    (declare (fixnum bitmap bit))
    (if (not (zerop (logand bitmap bit)))
        (let* ((index (index x bit))
               (node (inode-del c (svref nodes index) hash key)))
          (if (eq node (svref nodes index))
              x
              (if (null node)
                  (if (eql bitmap bit)
                      nil
                      (make-bitmap-indexed-node :bitmap (logand bitmap (lognot bit))
                                                :nodes (shrink-clone-simple-vector nodes index)
                                                :shift shift))
                  (let ((new-nodes (copy-seq nodes)))
                    (setf (svref new-nodes index) node)
                    (make-bitmap-indexed-node :bitmap bitmap :nodes new-nodes :shift shift)))))
        x)))
         
(defmethod inode-ref ((c tab-ctx) (x bitmap-indexed-node) (hash fixnum) key)
  (let* (((:slotval bitmap nodes shift) x)
         (bit (bit-position hash shift)))
    (declare (fixnum bitmap bit))
    (unless (zerop (logand bitmap bit))
      (inode-ref c (svref nodes (index x bit)) hash key))))

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
  (let* (((:slotval leaves) node))
    (declare (simple-vector leaves))
    (dotimes (i (length leaves))
      (when (inode-ref tab-ctx (svref leaves i) hash key)
        (return-from find-index i)))
    -1))

(defmethod inode-add ((c tab-ctx) (x hash-collision-node) shift (hash fixnum) key val)
  (if (eql hash (hash-collision-node-hash x))
      (let* (((:slotval leaves) x)
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
  (let* (((:slotval leaves) x)
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

