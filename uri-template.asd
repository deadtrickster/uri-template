;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :uri-template
  :serial t
  :components ((:file "package")
               (:file "uri-template")
               (:file "destructure-uri"))
  :depends-on (:cl-ppcre :named-readtables))

(asdf:defsystem :uri-template.test
  :serial t
  :components ((:file "test-package")
               (:file "uri-template-test"))
  :depends-on (:uri-template))
