(in-package :cl-user)

(asdf:defsystem :x.fdatatypes
  :depends-on (:x.let-star)
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "tab-ctx")
               (:file "vec-ctx")
               (:file "ftab")
               (:file "fvec")
               (:file "fset")
               (:file "test")))

