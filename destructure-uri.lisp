(in-package #:uri-template)

(defvar uri-decode? t
  "Controls whether URI decoding/unescaping is done on the templated value when destructuring.
True by default.")

(defun uri-decode (str)
  "Decodes URI encoded/escaped characters in the given string."
  (regex-replace-all "%[\\d|a-f|A-F]{2}" str (lambda (match) (string (code-char (parse-integer match :start 1 :radix 16)))) :simple-calls t))

(defmacro weak-register-groups-bind (vars regex str &body body)
  `(destructuring-bind (&optional ,@vars)
      (coerce (nth-value 1 (scan-to-strings ,regex ,str)) 'list)
     (declare (ignorable ,@vars))
     ,(when uri-decode?
       `(setf ,@(loop for var in vars append `(,var (when ,var (uri-decode ,var))))))
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
         (template-vars (mapcar (lambda (x) (car (last (second x)))) (remove-if #'stringp template)))
         (uri-var (gensym)))
    `(let ((,uri-var ,uri))
       (weak-register-groups-bind (%uri-head x1 %uri-scheme x2 %uri-authority %uri-tail %uri-path x3 %uri-query x4 %uri-fragment)
           ;; regex adapted from RFC 2396: "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
           "^((([^:/?#]+):)?(//([^/?#]*)))?(([^?#]*)(\\?([^#]*))?(#(.*))?)" ,uri-var
         (weak-register-groups-bind (x1 %uri-user %uri-host x2 %uri-port)
             "(([^@]+)@)?([^\\:]+)(\\:(\\d+))?" %uri-authority
           (weak-register-groups-bind (%uri-directory %uri-file)
               "(.*/)([^/]+)?" %uri-path
             (weak-register-groups-bind ,template-vars
                 '(:sequence :start-anchor ,@(substitute-if '(:register (:greedy-repetition 0 nil :everything)) (complement #'stringp) template) :end-anchor) ,uri-var
               (when (and ,@template-vars)
                 ,@body))))))))
