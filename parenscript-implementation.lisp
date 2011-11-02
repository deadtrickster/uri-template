(in-package #:uri-template)

(parenscript:defpsmacro maybe-uri-encode (x)
  (if uri-encode? `(encode-u-r-i-component ,x) x))

(parenscript:defpsmacro uri-template (&rest template-args)
  `(+ ,@template-args))
