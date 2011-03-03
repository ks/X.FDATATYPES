(in-package :x.fdatatypes)

(defpackage :x.fdatatypes-iterate
  (:use :cl :iterate :x.fdatatypes))

;; This package does not export anything in itself,
;; it uses x.fdatatypes:iterator and x.fdatatypes:iterator-next
;; function to create iterate drivers.
;;
;; These iterate drivers also support iterate generators, when the key, value or both
;; are updated on demand with (next key) ;; or (next val) 
;; in iterate loop.
;;
;; Wildcards '_' can be used instead of key or val when they are not needed
;;
;; Iterate's drivers are recognized by keywords, and x.fdatatype's iterate
;; drivers are:
;;
;; ------------------------
;; :IN-FDATATYPE
;; ex: (iter (for (key val) :in-fdatatype ftab/fset/fvec) ;; or instead 'for' - 'generate'
;;
;; ------------------------
;; :IN-FDATATYPE-KEYS
;; ex: (iter (for key :in-fdatatype-keys ftab/fset/fvec) ;; or instead 'for' - 'generate'
;;
;; for fvec, it's key is it's index
;;
;; ------------------------
;; :IN-FDATATYPE-VALS
;; ex: (iter (for val :in-fdatatype-vals ftab/fset/fvec) ;; or instead 'for' - 'generate'
;;
;; for fset, it's value it always T
;;
;; ------------------------
;; :IN-FVEC
;; ex: (iter (for val :in-fvec fvec) ;; or instead 'for' - 'generate'
;;
;; ------------------------
;; :IN-FSET
;; ex: (iter (for key :in-fset fset) ;; or instead 'for' - 'generate'
;;
;; ------------------------
;; :IN-FTAB
;; ex: (iter (for (key val) :in-ftab ftab) ;; or instead 'for' - 'generate'
;;
;;
;; Drivers :IN-FVEC, :IN-FSET and :IN-FTAB accepts only fvec, respectively fset and ftab,
;; or fail otherwise.
;;
;; With :IN-FDATATYPE, :IN-FDATATYPE-KEYS and :IN-FDATATYPE-VALS any x.fdatatype can be used.
