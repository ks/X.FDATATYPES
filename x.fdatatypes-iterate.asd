(in-package :cl-user)

(asdf:defsystem :x.fdatatypes-iterate
  :depends-on (:x.let-star :x.fdatatypes :iterate)
  :serial t
  :components ((:module "x.fdatatypes-iterate"
                :components ((:file "package")
                             (:file "iterate")))))
