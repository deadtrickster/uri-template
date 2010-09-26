(in-package #:cl)

(defpackage #:uri-template.test
  (:use #:cl #:uri-template #:named-readtables)
  (:export #:run-tests))

(in-package #:uri-template.test)

(defreadtable uri-template.test
  (:merge :standard uri-template:uri-template))
