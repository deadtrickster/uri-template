(in-package #:uri-template)

(defvar uri-encode? t
  "Controls whether URI encoding/escaping is done on the templated value.
True by default.")

;;; stolen from hunchentoot
(defun uri-encode (str)
  "URI encodes/escapes the given string."
  (with-output-to-string (s)
    (loop for c across (flexi-streams:string-to-octets str :external-format :utf-8)
       do (if (or (<= 48 c 57)
                  (<= 65 c 90)
                  (<= 97 c 122)
                  (find c '(36 45 95 46 33 42 39 40 41)))
              (write-char (code-char c) s)
              (format s "%~2,'0x" c)))))

(defun read-uri-template (stream &optional recursive-p)
  "A function suitable for inserting into the readtable so you can
read URI templates from your own dispatch character."
  (let ((*readtable* (copy-readtable))
        (template-accumulator ())
        (string-accumulator #1=(make-array 10
                                           :element-type 'character
                                           :adjustable t :fill-pointer 0))
        (next-char))
    (set-syntax-from-char #\} #\))
    (flet ((collect-string ()
             (when (< 0 (length string-accumulator))
               (push string-accumulator template-accumulator)
               (setf string-accumulator #1#))))
      (loop until (member
                   (setf next-char (read-char stream nil #\Space recursive-p))
                   '(#\Space #\Newline #\Tab #\)))
            do (if (char= #\{ next-char)
                   (progn (collect-string)
                          (push `(maybe-uri-encode
                                  (progn ,@(read-delimited-list #\} stream))
                                  ) template-accumulator))
                   (vector-push-extend next-char string-accumulator))
            finally (unread-char next-char stream) (collect-string))
      (reverse template-accumulator))))

(defmacro maybe-uri-encode (x)
  (if uri-encode? `(uri-encode (princ-to-string ,x)) x))

(defun uri-template (&rest template-args)
  "The car of the list that the URI template reader produces. A
function or macro.

This symbol also names the named-readtables readtable that provides
the #U dispatch macro."
  (format nil "~{~A~}" template-args))

(defun uri-template-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(uri-template ,@(read-uri-template stream t)))

(defun enable-uri-template-syntax ()
  "Binds the #U dispatch character to read a URI template."
  (set-dispatch-macro-character #\# #\U 'uri-template-reader)
  (values))
