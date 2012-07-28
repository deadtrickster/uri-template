;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :uri-template.test
  :license "Public Domain"
  :serial t
  :components ((:file "test-package")
               (:file "uri-template-test"))
  :depends-on (:uri-template :eos))
