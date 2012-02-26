(in-package #:cl)

(defpackage #:uri-template
  (:use #:cl #:cl-ppcre #:named-readtables)
  (:export
   ;; common
   #:uri-template

   ;; interpolation
   #:enable-uri-template-syntax
   #:read-uri-template
   #:uri-encode?
   #:uri-encode

   ;; destructuring
   #:uri-template-bind
   #:uri-decode?
   #:uri-decode

   ;; RFC 2396 standard URI components
   #:%uri-scheme
   #:%uri-authority
   #:%uri-path
   #:%uri-query
   #:%uri-fragment

   ;; extended components
   #:%uri-head
   #:%uri-tail
   #:%uri-user
   #:%uri-host
   #:%uri-port
   #:%uri-directory
   #:%uri-file))

(in-package #:uri-template)

(defreadtable uri-template
  (:merge :standard)
  (:dispatch-macro-char #\# #\U
                        (lambda (&rest args)
                          (apply #'uri-template-reader args))))
