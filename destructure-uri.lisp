(in-package #:uri-template)

(defvar uri-decode? t
  "Controls whether URI decoding/unescaping is done on the templated value when destructuring.
True by default.")

(defun uri-decode (str)
  "Decodes URI encoded/escaped characters in the given string."
  (let ((octets (make-array (length str)
                            :element-type 'flexi-streams:octet
                            :adjustable t :fill-pointer 0)))
    (do ((i 0 (incf i)))
        ((>= i (length str)))
      (let ((c (aref str i)))
        (vector-push
         (if (eql c #\%)
             (prog1 (parse-integer str :start (1+ i) :end (+ i 3)
                                   :radix 16)
               (incf i 2))
             (char-code c))
         octets)))
    (flexi-streams:octets-to-string octets :external-format :utf8)))

(defmacro weak-register-groups-bind (vars regex str &body body)
  `(destructuring-bind (&optional ,@vars)
      (coerce (nth-value 1 (scan-to-strings ,regex ,str)) 'list)
     (declare (ignorable ,@vars))
     ,(when uri-decode?
       `(setf ,@(loop for var in vars append
                     `(,var (when ,var (uri-decode ,var))))))
     ,@body))

(defmacro uri-template-bind ((template) uri &body body)
  "Binds URI template placeholders (which must be symbols) in given
URI, as well as attempting to bind a set of standard URI components to
their respective parts of the given URI. Body executes only if all
explicitly specified URI template placeholders can be bound.

Given the example URI http://user@www.foo.com:8080/dir/abc?bar=baz&xyz=1#hash
The standard URI components look like:

%uri-scheme     http
%uri-authority  user@www.foo.com:8080
%uri-user       user
%uri-host       www.foo.com
%uri-port       8080
%uri-path       /dir/abc
%uri-directory  /dir/
%uri-file       abc
%uri-query      bar=baz&xyz=1
%uri-fragment   hash
%uri-head       http://user@www.foo.com:8080
%uri-tail       /dir/abc?bar=baz&xyz=1#hash"
  (let* ((template (cdr template)) ;; template is expected to look like output of #U: '(uri-template &rest args)
         (template-vars (mapcar (lambda (x)
                                  (car (last (second x))))
                                (remove-if #'stringp template)))
         (uri-var (gensym)))
    `(let ((,uri-var ,uri))
       (weak-register-groups-bind (%uri-head x1 %uri-scheme x2 %uri-authority %uri-tail %uri-path x3 %uri-query x4 %uri-fragment)
           ;; regex adapted from RFC 2396: "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
           "^((([^:/?#]+):)?(//([^/?#]*)))?(([^?#]*)(\\?([^#]*))?(#(.*))?)"
           ,uri-var
         (weak-register-groups-bind (x1 %uri-user %uri-host x2 %uri-port)
             "(([^@]+)@)?([^\\:]+)(\\:(\\d+))?"
             %uri-authority
           (weak-register-groups-bind (%uri-directory %uri-file)
               "(.*/)([^/]+)?"
               %uri-path
             (weak-register-groups-bind ,template-vars
                 '(:sequence
                   :start-anchor
                   ,@(substitute-if
                      '(:register (:greedy-repetition 0 nil :everything))
                      (complement #'stringp)
                      template)
                   :end-anchor) ,uri-var
               (when (and ,@template-vars)
                 ,@body))))))))
