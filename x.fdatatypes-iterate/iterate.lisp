(in-package :x.fdatatypes-iterate)

(defmacro-driver (for key/val :in-fdatatype container)
  (destructuring-bind (key val) key/val
    (let ((kwd (if generate 'generate 'for))
          (iterator (gensym))
          (result-template (gensym))
          (tmp-key (gensym))
          (tmp-val (gensym))
          (validp (gensym)))
      `(progn
         (with ,iterator = (iterator ,container))
         (while ,iterator)
         (with ,result-template = (list nil nil))
         (with ,tmp-key)
         (with ,tmp-val)
         (with ,validp)
         (,kwd (,key ,val) next
               (progn
                 (multiple-value-setq (,iterator ,tmp-key ,tmp-val ,validp)
                   (iterator-next ,iterator))
                 (unless ,validp (terminate))
                 (setf (first ,result-template) ,tmp-key
                       (second ,result-template) ,tmp-val)
                 ,result-template))))))

(defmacro-driver (for key :in-fdatatype-keys container)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd (,key _) :in-fdatatype ,container))))

(defmacro-driver (for val :in-fdatatype-vals container)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd (_ ,val) :in-fdatatype ,container))))

(defmacro-driver (for val :in-fvec fvec)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,val :in-fdatatype-vals (the fvec ,fvec)))))

(defmacro-driver (for key :in-fset fset)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,key :in-fdatatype-keys (the fset ,fset)))))

(defmacro-driver (for key/val :in-ftab ftab)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,key/val :in-fdatatype (the ftab ,ftab)))))
