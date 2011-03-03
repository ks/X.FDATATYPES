(in-package :x.fdatatypes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %reevaluate-constant (name value test)
    (if (not (boundp name))
        value
        (let ((old (symbol-value name))
              (new value))
          (if (not (constantp name))
              (prog1 new
                (cerror "Try to redefine the variable as a constant."
                        "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
              (if (funcall test old new)
                  old
                  (restart-case
                      (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                    (ignore ()
                      :report "Retain the current value."
                      old)
                    (continue ()
                      :report "Try to redefine the constant."
                      new))))))))

(defmacro define-constant (name initial-value &key (test ''eql) documentation)
  `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
     ,@(when documentation `(,documentation))))

(defmacro nconcing ((&key (init nil)
                          (into 'nconc-result)
                          (call 'nconc-it)
                          (count nil)
                          (last nil)
                          (before-last nil))
                    &body body)
  (let ((head-sym (gensym "HEAD"))
        (tail-sym (gensym "TAIL")))
    `(let* ((,head-sym (cons nil ,init))
            (,tail-sym (last ,head-sym))
            (,into (cdr ,head-sym))
            ,@(when count `((,count (length ,init))))
            ,@(when last `((,last nil)))
            ,@(when before-last `((,before-last (last ,init 2)))))
       (flet ((,call (x)
                ,@(when before-last
                        `((setf ,before-last 
                                (unless (eq ,head-sym ,tail-sym)
                                  ,tail-sym))))
                (rplacd ,tail-sym (setf ,tail-sym (list x)))
                (setf ,into (cdr ,head-sym))
                ,@(when last `((setf ,last ,tail-sym)))
                ,@(when count `((incf ,count)))))
         ,@body))))

