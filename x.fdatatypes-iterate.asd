(in-package :cl-user)

(asdf:defsystem :x.fdatatypes-iterate
  :description "iterate support for x.fdatatypes collections"
  :author "karol.skocik@gmail.com"
  :license "BSD compatible"
  :depends-on (:x.let-star :x.fdatatypes :iterate)
  :serial t
  :components ((:module "x.fdatatypes-iterate"
                :components ((:file "package")
                             (:file "iterate")))))
