(in-package :cl-user)

(asdf:defsystem :x.fdatatypes
  :depends-on (:x.let-star)
  :serial t
  :components ((:module "x.fdatatypes"
                :components ((:file "package")
                             (:file "macros")
                             (:file "common")
                             (:file "tab-ctx")
                             (:file "vec-ctx")
                             (:file "ftab")
                             (:file "fvec")
                             (:file "fset")
                             (:file "test")))))

