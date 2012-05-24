(in-package :x.fdatatypes)

;;;;;;;;;; INTERFACE TO FTAB, FVEC, FSET

(defgeneric size (container))
(defgeneric empty (container))

(defgeneric ref (container key))
(defgeneric ref* (container &rest keys))
(defgeneric ref-opt (container optional key))
(defgeneric ref-opt* (container optional &rest keys))
(defgeneric ref-opt-fn* (container optional-fn &rest keys))

(defgeneric del (container key))
(defgeneric del* (container &rest keys))

(defgeneric fmap-to (result-type container function &key from-end))
(defgeneric fold (container function &key init from-end))
(defgeneric filter (container predicate &key from-end))

(defgeneric iterator (container))
(defgeneric iterator-next (iterator))

;;;;;;;;;; INTERFACE TO FTAB, FVEC

(defgeneric vals (container))

(defgeneric add (container key val))
(defgeneric add* (container &rest contents))

(defgeneric fmap (container function &key from-end))

;;;;;;;;;; INTERFACE TO FTAB, FSET

(defgeneric keys (container))

;;;;;;;;;;

(define-condition return-container-as-is () ())

;;;;;;;;;; ERRORS

(define-condition invalid-arguments (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Invalid arguments for this operation."))))

(define-condition not-found (error)
  ((key :initarg :key :reader key))
  (:report (lambda (c s)
             (format s "No value for key ~S was found." (key c)))))

(define-condition out-of-bounds (error)
  ((bad-index :initarg :bad-index :reader bad-index)
   (limit :initarg :limit :reader limit))
  (:report (lambda (c s)
             (format s "index ~A is out of bounds for VEC-CTX, should be nonnegative and <~A"
                     (bad-index c) (limit c)))))

(define-condition container-empty (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "invalid operation on empty container"))))

;;;;;;;;;;

(defun function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defun bad-value-for-key (key container datum)
  (flet ((read-new-value ()
           (multiple-value-list (eval (read)))))
    (restart-case (error datum)
      (use-value-for-key (new-val)
        :interactive read-new-value
        :report (lambda (stream)
                  (format stream "Use value for key ~S" key))
        new-val)
      (return-as-is ()
        :report (lambda (stream)
                  (format stream "Return ~A as is" container))
        (signal 'return-container-as-is)))))

(defun map-associations (fn sequence container)
  (let ((mode :key)
        (key nil))
    (map nil (lambda (elem)
               (ecase mode
                 (:key (setf key elem
                             mode :val))
                 (:val (setf container (funcall fn container key elem)
                             mode :key))))
         sequence)
    (if (eq mode :key)
        container
        (error 'not-found :key key))))

(defun map-associations-1 (fn sequence container &key bad-value-fn bad-value-datum)
  (handler-case
      (map-associations fn sequence container)
    (not-found (c)
      (handler-case 
          (let ((val (funcall bad-value-fn (key c) container bad-value-datum)))
            (funcall fn container (key c) val))
        (return-container-as-is ()
          container)))))

(defun nth-split (list n)
  (values (subseq list 0 n) (nthcdr n list)))

(defun maybe-nth-split (list n)
  (let ((current list))
    (nconcing (:into part1 :count count)
      (dotimes (i n)
        (cond ((null current)
               (return (values part1 nil i)))
              (t
               (nconc-it (car current))
               (setf current (cdr current)))))
      (values part1 current count))))

(defun unzip-alist (alist)
  (nconcing (:into l1 :call nconc-1)
    (nconcing (:into l2 :call nconc-2)
      (map nil (lambda (pair)
                 (nconc-1 (car pair))
                 (nconc-2 (cdr pair)))
           alist)
      (values l1 l2))))

;;;;;;;;;;

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

(deftype fixnum-1 ()
  `(integer 0 ,(1- most-positive-fixnum)))

(deftype non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(deftype shift ()
  `(mod 800))

(declaim (inline mask))
(defun mask (hash shift)
  (declare (shift shift) (fixnum hash))
  (logand (ash hash (- shift)) #x1f))

(declaim (inline bit-position))
(defun bit-position (hash shift)
  (declare (shift shift) (fixnum hash))
  (ash 1 (mask hash shift)))

(declaim (inline shrink-clone-simple-vector))
(defun shrink-clone-simple-vector (orig index)
  (declare (simple-vector orig) (fixnum-1 index))
  (let ((new (make-array (1- (length orig)))))
    (replace new orig :end1 index)
    (replace new orig :start1 index :start2 (1+ index))
    new))

(declaim (inline expand-clone-simple-vector))
(defun expand-clone-simple-vector (orig index value)
  (declare (simple-vector orig) (fixnum-1 index))
  (let ((new (make-array (1+ (length orig)))))
    (replace new orig :end2 index)
    (setf (svref new index) value)
    (replace new orig :start1 (1+ index) :start2 index)
    new))

(declaim (inline map-simple-vector-from-end))
(defun map-simple-vector-from-end (function vector)
  (declare (simple-vector vector) (function function))
  (loop :for i :from (1- (length vector)) :downto 0
        :do (funcall function (svref vector i))))

