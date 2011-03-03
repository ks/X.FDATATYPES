(in-package :x.fdatatypes)

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

;;;;;;;;;; INTERFACE TO FSET

(defgeneric add-key (container key))
(defgeneric add-key* (container &rest keys))

(defgeneric has-key (container key))
(defgeneric has-key* (container &rest keys))

;;;;;;;;;; USER CONSTRUCTORS:

(defun fset-ex (&key (key-test #'equal) (hash-fun #'sxhash))
  (%make-fset :tab-ctx (make-tab-ctx :root (make-empty-node)
                                     :hash-fun hash-fun
                                     :key-test key-test
                                     :val-test #'eq)))

(defun fset (&rest initial-contents)
  (apply #'add-key* (fset-ex) initial-contents))

(defmethod coerce-fset ((x fvec) &key (key-test #'equal) (hash-fun #'sxhash))
  (fold x #'add-key
        :init (fset-ex :key-test key-test
                       :hash-fun hash-fun)))

(defmethod coerce-fset ((x list) &key (key-test #'equal) (hash-fun #'sxhash))
  (reduce #'add-key x
          :initial-value (fset-ex :key-test key-test
                                  :hash-fun hash-fun)))

;;;;;;;;;;

(defstruct (fset (:constructor %make-fset))
  (tab-ctx (error "no context") :type tab-ctx))

(defmethod print-object ((x fset) s)
  (let* (((:slotval count key-test hash-fun) (fset-tab-ctx x)))
    (print-unreadable-object (x s :type t :identity t)
      (format s ":COUNT ~A :KEY-TEST ~A :HASH-FUN ~A"
              count
              (function-name key-test)
              (function-name hash-fun)))))

;;;;;;;;;;

(defmethod size ((x fset))
  (tab-ctx-count (fset-tab-ctx x)))

(defmethod empty ((x fset))
  (zerop (size x)))

(defmethod keys ((x fset))
  (let ((result '()))
    (tab-ctx-map (fset-tab-ctx x)
                 (lambda (n)
                   (push (leaf-node-key n) result)))
    result))

(defmethod add-key ((x fset) key)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (tab-ctx-add tab-ctx key t)))
    (if (eq tab-ctx new-tab-ctx)
        x
        (%make-fset :tab-ctx new-tab-ctx))))

(defmethod add-key* ((x fset) &rest keys)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx tab-ctx))
    (dolist (key keys)
      (setf new-tab-ctx (tab-ctx-add new-tab-ctx key t)))
    (if (eq tab-ctx new-tab-ctx)
        x
        (%make-fset :tab-ctx new-tab-ctx))))

(defmethod ref ((x fset) key)
  (let* (((:mval value found) (tab-ctx-ref (fset-tab-ctx x) key)))
    (declare (ignore value))
    (if found
        t
        (error 'not-found :key key))))

(defmethod ref* ((x fset) &rest keys)
  (mapcar (lambda (key) (ref x key)) keys))

(defmethod ref-opt ((x fset) optional key)
  (handler-case
      (values (ref x key) t)
    (not-found ()
      (values optional nil))))

(defmethod ref-opt* ((x fset) optional &rest keys)
  (mapcar (lambda (key) (ref-opt x optional key)) keys))

(defmethod ref-opt-fn* ((x fset) optional-fn &rest keys)
  (declare (function optional-fn))
  (flet ((f (key)
           (handler-case (ref x key)
             (not-found ()
               (funcall optional-fn key)))))
    (mapcar #'f keys)))

(defmethod has-key ((x fset) key)
  (handler-case
      (progn (ref x key) t)
    (not-found ()
      nil)))

(defmethod has-key* ((x fset) &rest keys)
  (mapcar (lambda (key) (has-key x key)) keys))

(defmethod del ((x fset) key)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (tab-ctx-del tab-ctx key)))
    (if (eq tab-ctx new-tab-ctx)
        x
        (%make-fset :tab-ctx new-tab-ctx))))

(defmethod del* ((x fset) &rest keys)
  (dolist (key keys x)
    (setf x (del x key))))

(defmethod fmap ((x fset) function &key from-end)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (make-tab-ctx :root (make-empty-node)
                                    :hash-fun (tab-ctx-hash-fun tab-ctx)
                                    :key-test (tab-ctx-key-test tab-ctx)
                                    :val-test (tab-ctx-val-test tab-ctx))))
    (flet ((f (node)
             (let* (((:slotval key) node))
               (setf new-tab-ctx
                     (tab-ctx-add new-tab-ctx (funcall function key) t)))))
      (declare (dynamic-extent #'f))
      (tab-ctx-map tab-ctx #'f from-end)
      (%make-fset :tab-ctx new-tab-ctx))))

(defmethod fmap-to (result-type (x fset) function &key from-end)
  (declare (function function))
  (let (result cursor)
    (labels ((wrap (n)
               (funcall function (leaf-node-key n)))
             (list-accumulate (n)
               (rplaca cursor (wrap n))
               (setf cursor (cdr cursor)))
             (vector-accumulate (n)
               (setf (aref result cursor) (wrap n))
               (incf cursor)))
      (declare (inline wrap list-accumulate vector-accumulate)
               (dynamic-extent #'wrap #'list-accumulate #'vector-accumulate))
      (let* ((function1
              (cond ((null result-type) #'wrap)
                    ((eq result-type 'list)
                     (setf result (make-list (size x))
                           cursor result)
                     #'list-accumulate)
                    ((subtypep result-type 'array)
                     (setf result (make-sequence result-type (size x))
                           cursor 0)
                     #'vector-accumulate)
                    (t (error "Invalid RESULT-TYPE: ~A" result-type)))))
        (tab-ctx-map (fset-tab-ctx x) function1 from-end)
        result))))

(defmethod fold ((x fset) function &key (init 0) from-end)
  (declare (function function))
  (let ((acc init))
    (flet ((f (key)
             (setf acc (funcall function acc key))))
      (fmap-to nil x #'f :from-end from-end)
      acc)))

(defmethod filter ((x fset) predicate &key from-end)
  (declare (function predicate))
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx tab-ctx))
    (flet ((f (node)
             (let* (((:slotval key) node))
               (unless (funcall predicate key)
                 (setf new-tab-ctx (tab-ctx-del new-tab-ctx key))))))
      (declare (dynamic-extent #'f))
      (tab-ctx-map tab-ctx #'f from-end)
      (if (eq tab-ctx new-tab-ctx)
          x
          (%make-fset :tab-ctx new-tab-ctx)))))

(defmethod iterator ((x fset))
  (tab-ctx-iterator (fset-tab-ctx x)))

;;;;;;;;;; FSET COMMON UTILS

(defmethod sequence-fset ((xs sequence) &key (key-test #'equal) (hash-fun #'sxhash))
  (let ((result (fset-ex :key-test key-test
                         :hash-fun hash-fun)))
    (map nil (lambda (x)
               (setf result (add-key result x)))
         xs)
    result))

(defmethod fset-list ((x fset))
  (fmap-to 'list x #'identity))

(defmethod fset-vector ((x fset))
  (fmap-to 'simple-vector x #'identity))

(defmethod fset-difference ((x fset) &rest fsets)
  (labels ((difference-1 (fset-1 fset-2)
             (filter fset-1 (lambda (key) (has-key fset-2 key)))))
    (reduce #'difference-1 fsets :initial-value x)))

(defmethod fset-exclusive-or ((x fset) (y fset))
  (fold y
        (lambda (acc key)
          (if (has-key acc key)
              (del acc key)
              (add-key acc key)))
        :init x))

(defmethod fset-union (container &rest containers)
  (let ((result container))
    (dolist (c containers)
      (setf result (fold c #'add-key :init result)))
    ;;(fmap-to nil c (lambda (key) (setf result (add-key result key)))))
    result))

