(in-package :cl-user)

;; Clojure like (Bagwell Tries) functional hash-table
;; Clojure like (?) functional array
(defpackage :x.fdatatypes
  (:use :common-lisp :x.let-star)
  (:shadowing-import-from x.let-star let*)
  (:export
   ;; FTAB SPECIFIC   
   "FTAB"             ;; constructor
   "FTAB-EX"          ;; constructor
   "FTAB-ALIST"       ;; converts ftab to alist
   "KEYS"             ;; returns keys of ftab as list
   ;; FVEC SPECIFIC
   "FVEC"             ;; constructor
   "FVEC-IOTA"        ;; iota, what more to say?
   "UPDATE"           ;; replaces value at index (slow but sometimes useful operation)
   "UPDATE*"          ;; replaces values at indices in one go (slow but sometimes useful operation)
   "ADD-TAIL"         ;; adds value to the end - a LOT faster than add* on fvec - use this to add stuff to fvec
   "ADD-TAIL*"        ;; adds more values to the end in one go - a LOT faster than add* on fvec
   "DEL-TAIL"         ;; removes element from tail - a LOT faster than del on fvec
   "DEL-TAIL*"        ;; removes more values from tail - s LOT faster than del* on fvec
   ;; COMMON FOR BOTH
   "SIZE"             ;; number of elements/associations in fvec/ftab 
   "EMPTY"            ;; empty predicate
   "VALS"             ;; returns values of container as list
   "ADD"              ;; adds value with key/index to container (use add-tail for fvec for efficiency)
   "ADD*"             ;; adds more key/index value pairs to container (use add-tail* for fvec for efficiency)
   "REF"              ;; gets value at key/index, error if not found
   "REF*"             ;; gets more values at keys/indexes as list, error if not found
   "REF-OPT"          ;; gets value at key/index, returns optional if not found
   "REF-OPT*"         ;; gets more values at keys/indexes as list, returns optional if not found
   "REF-OPT-FN*"      ;; gets more values at keys/indexes as list, calls function if not found
   "DEL"              ;; removes element from container (use del-tail for fvec for effeciency if you can)
   "DEL*"             ;; removes more elements from container in one go (use del-tail* for fvec for effeciency if you can)
   "FMAP"             ;; maps function over container, returns new container
   "FMAP-TO"          ;; maps function over container, returns type supplied as argument (nil, list, vector, bit-vector, ...)
   "FOLD"             ;; fold function over container
   "FILTER"))         ;; filter function over container, returns new container


