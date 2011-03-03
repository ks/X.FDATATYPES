(in-package :x.fdatatypes)

;;;; some simple tests

(defun test-ftab-add (n &key result)
  (let ((tab (ftab)))
    (time
     (dotimes (i n)
       (setf tab (add tab i (* i i)))))
    (when result tab)))

(defun test-ftab-ref (tab n)
  (time
   (dotimes (i n)
     (ref tab i)))
  (values))

(defun test-ftab-del (tab n)
  (time
   (dotimes (i n)
     (setf tab (del tab i))))
  (values))

(defun test-fvec-add-tail (n &key result)
  (let ((vec (fvec)))
    (time
     (dotimes (i n)
       (setf vec (add-tail vec (* i i)))))
    (when result vec)))

(defun test-fvec-ref (vec n)
  (time
   (dotimes (i n)
     (ref vec i)))
  (values))

(defun test-fvec-del-tail (vec n)
  (time
   (dotimes (i n)
     (setf vec (del-tail vec))))
  (values))

(defun test-hashtable-add (n &key result)
  (let ((h (make-hash-table :test #'equal)))
    (time
     (dotimes (i n)
       (setf (gethash i h) (* i i))))
    (when result h)))

(defun test-hashtable-ref (h n)
  (time
   (dotimes (i n)
     (gethash i h)))
  (values))

(defun test-hashtable-del (h n)
  (time
   (dotimes (i n)
     (remhash i h)))
  (values))




