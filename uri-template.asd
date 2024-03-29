;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem :uri-template
  :description "An implementation of the URI Template proposed standard draft version 01."
  :long-description "An implementation of the URI Template proposed standard draft version 01.
Lets you easily create and parse URIs by using the URI Template reader macro syntax."
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :license "LLGPLv3"
  :serial t
  :components ((:file "package")
               (:file "uri-template")
               (:file "destructure-uri")
               #+parenscript (:file "parenscript-implementation")
               )
  :depends-on (:cl-ppcre :named-readtables :flexi-streams))
