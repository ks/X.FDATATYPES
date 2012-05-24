(in-package :x.fdatatypes)

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

;;;;;;;;;; SPECIFIC TO FVEC

(defgeneric update (container index val))
(defgeneric update* (container &rest contents))
(defgeneric add-tail (container val))
(defgeneric add-tail* (container &rest vals))
(defgeneric del-tail (container))
(defgeneric del-tail* (container count))

;;;;;;;;;; USER CONSTRUCTORS:

(defun fvec (&rest initial-contents)
  (let (chunk chunk-length)
    (nconcing (:into content :count content-length :before-last before-last)
      (loop
         (multiple-value-setq (chunk initial-contents chunk-length)
           (maybe-nth-split initial-contents 32))
         (setf chunk (coerce (the list chunk) 'simple-vector))
         (if initial-contents
             (nconc-it chunk)
             (return-from fvec
               (%make-fvec :vec-ctx (build-vec-ctx content
                                                   content-length
                                                   before-last
                                                   chunk
                                                   (length chunk)))))))))

(defmethod coerce-fvec ((x sequence))
  (let ((result (make-vec-ctx :shift 5 :root #() :tail #())))
    (map nil (lambda (val) (setf result (vec-ctx-add-tail result val))) x)
    (%make-fvec :vec-ctx result)))

;;;;;;;;;;

(defstruct (fvec (:constructor %make-fvec))
  (vec-ctx (error "no context") :type vec-ctx))

(defmethod print-object ((x fvec) s)
  (print-unreadable-object (x s :type t :identity t)
    (format s ":COUNT ~A" (vec-ctx-count (fvec-vec-ctx x)))))


;;;;;;;;;;

(defmethod size ((x fvec))
  (vec-ctx-count (fvec-vec-ctx x)))

(defmethod empty ((x fvec))
  (zerop (the fixnum-1 (size x))))

(defmethod vals ((x fvec))
  (nconcing (:into res)
    (vec-ctx-map (fvec-vec-ctx x) #'nconc-it)
    res))

(defmethod add ((x fvec) index val)
  (%make-fvec :vec-ctx (vec-ctx-add (fvec-vec-ctx x) index val)))

(defmethod add* ((x fvec) &rest contents)
  (%make-fvec :vec-ctx (vec-ctx-add* (fvec-vec-ctx x) contents)))

(defmethod ref ((x fvec) index)
  (vec-ctx-ref (fvec-vec-ctx x) index))

(defmethod ref* ((x fvec) &rest indexes)
  (mapcar (lambda (index) (ref x index)) indexes))

(defmethod ref-opt ((x fvec) optional index)
  (handler-case
      (values (ref x index) t)
    (out-of-bounds ()
      (values optional nil))))

(defmethod ref-opt* ((x fvec) optional &rest indexes)
  (mapcar (lambda (index) (ref-opt x optional index)) indexes))

(defmethod ref-opt-fn* ((x fvec) optional-fn &rest indexes)
  (declare (function optional-fn))
  (flet ((f (index)
           (handler-case (ref x index)
             (not-found ()
               (funcall optional-fn index)))))
    (mapcar #'f indexes)))

(defmethod del ((x fvec) index)
  (%make-fvec :vec-ctx (vec-ctx-del (fvec-vec-ctx x) index)))

(defmethod del* ((x fvec) &rest indexes)
  (%make-fvec :vec-ctx (vec-ctx-del* (fvec-vec-ctx x) indexes)))

(defmethod fmap ((x fvec) function &key from-end)
  (let ((new-vec-ctx +empty-vec-ctx+))
    (flet ((f (val)
             (setf new-vec-ctx
                   (vec-ctx-add-tail new-vec-ctx (funcall function val)))))
      (vec-ctx-map (fvec-vec-ctx x) #'f from-end)
      (%make-fvec :vec-ctx new-vec-ctx))))

(defmethod fmap-to (result-type (x fvec) function &key from-end)
  (declare (function function))
  (let (result cursor)
    (labels ((list-accumulate (val)
               (rplaca cursor (funcall function val))
               (setf cursor (cdr cursor)))
             (vector-accumulate (val)
               (setf (aref result cursor) (funcall function val))
               (incf cursor)))
      (declare (inline list-accumulate vector-accumulate)
               (dynamic-extent #'list-accumulate #'vector-accumulate))
      (let* ((function1
              (cond ((null result-type) function)
                    ((eq result-type 'list)
                     (setf result (make-list (size x))
                           cursor result)
                     #'list-accumulate)
                    ((subtypep result-type 'array)
                     (setf result (make-sequence result-type (size x))
                           cursor 0)
                     #'vector-accumulate)
                    (t (error "Invalid RESULT-TYPE: ~A" result-type)))))
        (vec-ctx-map (fvec-vec-ctx x) function1 from-end)
        result))))

(defmethod fold ((x fvec) function &key (init 0) from-end)
  (declare (function function))
  (let ((acc init))
    (flet ((f (val)
             (setf acc (funcall function acc val))))
      (fmap-to nil x #'f :from-end from-end)
      acc)))

(defmethod filter ((x fvec) predicate &key from-end)
  (let ((new-vec-ctx +empty-vec-ctx+))
    (flet ((f (val)
             (when (funcall predicate val)
               (setf new-vec-ctx (vec-ctx-add-tail new-vec-ctx val)))))
      (vec-ctx-map (fvec-vec-ctx x) #'f from-end)
      (%make-fvec :vec-ctx new-vec-ctx))))

(defmethod update ((x fvec) index val)
  (%make-fvec :vec-ctx (vec-ctx-update (fvec-vec-ctx x) index val)))

(defmethod update* ((x fvec) &rest contents)
  (%make-fvec :vec-ctx (vec-ctx-update* (fvec-vec-ctx x) contents)))
  
(defmethod add-tail ((x fvec) val)
  (%make-fvec :vec-ctx (vec-ctx-add-tail (fvec-vec-ctx x) val)))

(defmethod add-tail* ((x fvec) &rest vals)
  (%make-fvec :vec-ctx (vec-ctx-add-tail* (fvec-vec-ctx x) vals)))
  
(defmethod del-tail ((x fvec))
  (%make-fvec :vec-ctx (vec-ctx-del-tail (fvec-vec-ctx x))))

(defmethod del-tail* ((x fvec) count)
  (if (> count (size x))
      (error 'invalid-arguments)
      (let ((res-ctx (fvec-vec-ctx x)))
        (dotimes (i count)
          (setf res-ctx (vec-ctx-del-tail res-ctx)))
        (%make-fvec :vec-ctx res-ctx))))

(defmethod iterator ((x fvec))
  (vec-ctx-iterator (fvec-vec-ctx x)))

;;;;;;;;;; FVEC COMMON UTILS

(defmethod fvec-iota (n &key (start 0) (step 1))
  (%make-fvec :vec-ctx (vec-ctx-iota n start step)))

(defmethod fvec-list ((x fvec))
  (fmap-to 'list x #'identity))

(defmethod fvec-vector ((x fvec))
  (fmap-to 'vector x #'identity))

