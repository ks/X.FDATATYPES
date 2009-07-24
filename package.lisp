(in-package :cl-user)

;; Clojure like (Bagwell Tries) functional hash-table
;; Clojure like (?) functional array
(defpackage :x.fdatatypes
  (:use :common-lisp :x.let-star)
  (:shadowing-import-from x.let-star let*)
  (:export
   ;; FSET
   "FSET"             ;; constructor
   "FSET-EX"          ;; constructor
   "COERCE-FSET"      ;; converts data type to fset
   "FSET-LIST"        ;; converts fset to list
   "FSET-DIFFERENCE"  ;; fset difference
   "FSET-EXCLUSIVE-OR";; fset exlusive or
   "FSET-UNION"       ;; fset union
   "ADD-KEY"          ;; adds key to fset
   "ADD-KEY*"         ;; adds more keys to fset
   "HAS-KEY"          ;; tests if key is in fset
   "HAS-KEY*"         ;; tests more keys at once
   
   ;; FTAB   
   "FTAB"             ;; constructor
   "FTAB-EX"          ;; constructor
   "COERCE-FTAB"      ;; converts data type to ftab
   "FTAB-ALIST"       ;; converts ftab to alist
   
   ;; FVEC
   "FVEC"             ;; constructor
   "COERCE-FVEC"      ;; converts data type to fvec
   "FVEC-LIST"        ;; converts fvec to list
   "FVEC-VECTOR"      ;; converts fvec to vector
   "FVEC-IOTA"        ;; creates fvec with contents from start to n by step
   "UPDATE"           ;; replaces value at index (slow but sometimes useful operation)
   "UPDATE*"          ;; replaces values at indices in one go (slow but sometimes useful operation)
   "ADD-TAIL"         ;; adds value to the end - a LOT faster than add* on fvec - use this to add stuff to fvec
   "ADD-TAIL*"        ;; adds more values to the end in one go - a LOT faster than add* on fvec
   "DEL-TAIL"         ;; removes element from tail - a LOT faster than del on fvec
   "DEL-TAIL*"        ;; removes more values from tail - s LOT faster than del* on fvec

   ;; FSET FTAB
   "KEYS"             ;; returns keys as list

   ;; FTAB FVEC
   "FMAP"             ;; maps function over container, returns new container
   "VALS"             ;; returns values of container as list
   
   ;; *
   "SIZE"             ;; number of elements/associations in fvec/ftab 
   "EMPTY"            ;; empty predicate
   "ADD"              ;; adds value with key/index to container (use add-tail for fvec for efficiency)
   "ADD*"             ;; adds more key/index value pairs to container (use add-tail* for fvec for efficiency)
   "REF"              ;; gets value at key/index, error if not found
   "REF*"             ;; gets more values at keys/indexes as list, error if not found
   "REF-OPT"          ;; gets value at key/index, returns optional if not found
   "REF-OPT*"         ;; gets more values at keys/indexes as list, returns optional if not found
   "REF-OPT-FN*"      ;; gets more values at keys/indexes as list, calls function if not found
   "DEL"              ;; removes element from container (use del-tail for fvec for effeciency if you can)
   "DEL*"             ;; removes more elements from container in one go (use del-tail* for fvec for effeciency if you can)
   "FMAP-TO"          ;; maps function over container, returns type supplied as argument (nil, list, vector, bit-vector, ...)
   "FOLD"             ;; fold function over container
   "FILTER"           ;; filter function over container, returns new container
   ))
   
   

   

