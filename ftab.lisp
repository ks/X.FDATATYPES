(in-package :x.fdatatypes)

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

;;;;;;;;;; USER CONSTRUCTORS:

(defun ftab-ex (&key (key-test #'equal) (val-test #'eq) (hash-fun #'sxhash))
  (%make-ftab :tab-ctx (make-tab-ctx :root (make-empty-node)
                                     :hash-fun hash-fun
                                     :key-test key-test
                                     :val-test val-test)))

(defun ftab (&rest initial-contents)
  (map-associations-1 #'add initial-contents (ftab-ex)
                      :bad-value-fn #'bad-value-for-key
                      :bad-value-datum "Odd number of elements in INITIAL-CONTENTS"))

(defmethod coerce-ftab ((x hash-table) &key (key-test #'equal) (val-test #'eq) (hash-fun #'sxhash))
  (let ((ctx (make-tab-ctx :root (make-empty-node)
                           :hash-fun hash-fun
                           :key-test key-test
                           :val-test val-test)))
    (maphash (lambda (key val) (setf ctx (tab-ctx-add ctx key val))) x)
    (%make-ftab :tab-ctx ctx)))

;;;;;;;;;;

(defstruct (ftab (:constructor %make-ftab))
  (tab-ctx (error "no context") :type tab-ctx))

(defmethod print-object ((x ftab) s)
  (let* (((:slotval count key-test val-test hash-fun) (ftab-tab-ctx x)))
    (print-unreadable-object (x s :type t :identity t)
      (format s ":COUNT ~A :KEY-TEST ~A :VAL-TEST ~A :HASH-FUN ~A"
              count
              (function-name key-test)
              (function-name val-test)
              (function-name hash-fun)))))

;;;;;;;;;;

(defmethod size ((x ftab))
  (tab-ctx-count (ftab-tab-ctx x)))

(defmethod empty ((x ftab))
  (zerop (size x)))

(defmethod keys ((x ftab))
  (let ((result '()))
    (tab-ctx-map (ftab-tab-ctx x)
                 (lambda (n)
                   (push (leaf-node-key n) result)))
    result))

(defmethod vals ((x ftab))
  (let ((result '()))
    (tab-ctx-map (ftab-tab-ctx x)
                 (lambda (n)
                   (push (leaf-node-val n) result)))
    result))

(defmethod add ((x ftab) key val)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (tab-ctx-add tab-ctx key val)))
    (if (eq tab-ctx new-tab-ctx)
        x
        (%make-ftab :tab-ctx new-tab-ctx))))

(defmethod add* ((x ftab) &rest contents)
  (map-associations-1 #'add contents x
                      :bad-value-fn #'bad-value-for-key
                      :bad-value-datum "Odd number of elements in CONTENTS"))

(defmethod ref ((x ftab) key)
  (let* (((:mval value found) (tab-ctx-ref (ftab-tab-ctx x) key)))
    (if found
        value
        (error 'not-found :key key))))

(defmethod ref* ((x ftab) &rest keys)
  (mapcar (lambda (key) (ref x key)) keys))

(defmethod ref-opt ((x ftab) optional key)
  (handler-case
      (values (ref x key) t)
    (not-found ()
      (values optional nil))))

(defmethod ref-opt* ((x ftab) optional &rest keys)
  (mapcar (lambda (key) (ref-opt x optional key)) keys))

(defmethod ref-opt-fn* ((x ftab) optional-fn &rest keys)
  (declare (function optional-fn))
  (flet ((f (key)
           (handler-case (ref x key)
             (not-found ()
               (funcall optional-fn key)))))
    (mapcar #'f keys)))

(defmethod del ((x ftab) key)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (tab-ctx-del tab-ctx key)))
    (if (eq tab-ctx new-tab-ctx)
        x
        (%make-ftab :tab-ctx new-tab-ctx))))

(defmethod del* ((x ftab) &rest keys)
  (dolist (key keys x)
    (setf x (del x key))))

(defmethod fmap ((x ftab) function &key from-end)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx (make-tab-ctx :root (make-empty-node)
                                    :hash-fun (tab-ctx-hash-fun tab-ctx)
                                    :key-test (tab-ctx-key-test tab-ctx)
                                    :val-test (tab-ctx-val-test tab-ctx))))
    (flet ((f (node)
             (let* (((:slotval key val) node))
               (setf new-tab-ctx
                     (tab-ctx-add new-tab-ctx key (funcall function key val))))))
      (declare (dynamic-extent #'f))
      (tab-ctx-map tab-ctx #'f from-end)
      (%make-ftab :tab-ctx new-tab-ctx))))

(defmethod fmap-to ((x ftab) function &key result-type from-end)
  (declare (function function))
  (let (result cursor)
    (labels ((wrap (n)
               (funcall function (leaf-node-key n) (leaf-node-val n)))
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
        (tab-ctx-map (ftab-tab-ctx x) function1 from-end)
        result))))

(defmethod fold ((x ftab) function &key (init 0) from-end)
  (declare (function function))
  (let ((acc init))
    (flet ((f (key val)
             (setf acc (funcall function acc key val))))
      (fmap-to x #'f :result-type nil :from-end from-end)
      acc)))

(defmethod filter ((x ftab) predicate &key from-end)
  (let* (((:slotval tab-ctx) x)
         (new-tab-ctx tab-ctx))
    (flet ((f (node)
             (let* (((:slotval key val) node))
               (unless (funcall predicate key val)
                 (setf new-tab-ctx (tab-ctx-del new-tab-ctx key))))))
      (declare (dynamic-extent #'f))
      (tab-ctx-map tab-ctx #'f from-end)
      (if (eq tab-ctx new-tab-ctx)
          x
          (%make-ftab :tab-ctx new-tab-ctx)))))

;;;;;;;;;; FTAB COMMON UTILS

(defmethod ftab-alist ((x ftab))
  (fmap-to x (lambda (k v) (cons k v)) :result-type 'list))


