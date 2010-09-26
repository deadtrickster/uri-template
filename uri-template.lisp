(in-package #:uri-template)

(defvar uri-encode? t)

(defun uri-encode (str)
  (regex-replace-all '(:alternation #\Space #\! #\* #\' #\( #\) #\; #\: #\@ #\& #\= #\+ #\$ #\, #\/ #\? #\# #\[ #\])
                     str
                     (lambda (match) (format nil "%~16R" (char-code (elt match 0))))
                     :simple-calls t))

(defun read-uri-template (stream &optional recursive-p)
  (let ((*readtable* (copy-readtable))
        (template-accumulator ())
        (string-accumulator ())
        (next-char))
    (set-syntax-from-char #\} #\))
    (flet ((collect-string ()
             (when string-accumulator
               (push (coerce (reverse string-accumulator) 'string) template-accumulator)
               (setf string-accumulator ()))))
      (loop until (member (setf next-char (read-char stream nil #\Space recursive-p)) '(#\Space #\Newline #\Tab #\)))
            do (if (char= #\{ next-char)
                   (progn (collect-string)
                          (push `(maybe-uri-encode
                                  (progn ,@(loop until (char= #\} (peek-char t stream))
                                                 collect (read stream t nil recursive-p)
                                                 finally (read-char stream nil))))
                                template-accumulator))
                   (push next-char string-accumulator))
            finally (unread-char next-char stream) (collect-string))
      (reverse template-accumulator))))

(defmacro maybe-uri-encode (x)
  (if uri-encode? `(uri-encode (princ-to-string ,x)) x))

#+parenscript (parenscript:defpsmacro maybe-uri-encode (x)
                (if uri-encode? `(encode-u-r-i-component ,x) x))

(defun uri-template (&rest template-args)
  (format nil "~{~A~}" template-args))

#+parenscript (parenscript:defpsmacro uri-template (&rest template-args)
                `(+ ,@template-args))

(defun uri-template-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(uri-template ,@(read-uri-template stream t)))

(defun enable-uri-template-syntax ()
  (set-dispatch-macro-character #\# #\U 'uri-template-reader)
  (values))
