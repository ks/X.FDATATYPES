(in-package :cl-user)

(asdf:defsystem :x.fdatatypes
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "macros")
                             (:file "common")
                             (:file "tab-ctx")
                             (:file "vec-ctx")
                             (:file "ftab")
                             (:file "fvec")
                             (:file "fset")
                             (:file "test")))))

