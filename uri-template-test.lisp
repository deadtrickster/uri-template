;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty provided
;; this notice is preserved. This file is offered as-is, without any
;; warranty.

(in-package #:uri-template.test)
(in-readtable uri-template.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite uri-template))

(in-suite uri-template)

(defun run-tests ()
  (run! 'uri-template))

(test interpolation3
  (let ((bar "bar")
        (baz 1))
   (is (string= #Uhttp://www.foo.com/bar/{bar}{baz}
                "http://www.foo.com/bar/bar1"))))

(test interpolation2
  (let ((bar "bar")
        (baz 1))
    (is (string= #Uhttp://www.foo.com/bar/{bar}/{baz}
                 "http://www.foo.com/bar/bar/1"))))

(test interpolation1
  (let ((baz 1))
    (is (string= #Uhttp://www.foo.com/bar/{baz}
                 "http://www.foo.com/bar/1"))))

(test interpolation-encoding
  (is (string= #Uhttp://www.foo.com/bar?foo={"^BAZ !bar"}
               "http://www.foo.com/bar?foo=%5EBAZ%20%21bar")))

(test destructuring1
      (is (equal (uri-template-bind (#Uhttp://www.factory.com/orders/{part}/{number})
                     "http://www.factory.com/orders/widget/1234"
                   (list part (parse-integer number) %uri-host))
                 '("widget" 1234 "www.factory.com"))))

(test destructuring2
      (is (equal (uri-template-bind (#U{uri})
                     "https://www.google.com/dir/1/2/search.html?arg=0-a&arg1=1-b&amp;arg3-c#hash"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("https" "www.google.com" "/dir/1/2/search.html" "/dir/1/2/" "search.html" "arg=0-a&arg1=1-b&amp;arg3-c" "hash"))))

(test destructuring3
      (is (equal (uri-template:uri-template-bind (#U/apps/{app-name}/{table-template}/{table-id})
                     "/apps/EVWeb/Fixed%20Cost/20"
                   (list app-name table-template table-id %uri-path))
                 '("EVWeb" "Fixed Cost" "20" "/apps/EVWeb/Fixed Cost/20"))))

(test destructuring4
      (is (equal (uri-template-bind (#U{uri})
                     "http://files3.dsv.data.cod.ru/?WyIyNWQ3NWU5NTRmZDU1MWIzYmQ5NzVjNzJhZjRkZmNhZSIsMTI1MTA5NjMxNCwiXHUwNDEwXHUwNDNiXHUwNDRjXHUwNDRmXHUwNDNkXHUwNDQxIFx1MDQ0MVx1MDQzNVx1MDQ0MFx1MDQzZVx1MDQzYVx1MDQ0MFx1MDQ0Ylx1MDQzYlx1MDQ0Ylx1MDQ0NS5yYXIiLCJrTzNqSUo3bUN5WlBPenlBVGdcL0M3UkZVWHdXYkN6SWtEYzUweTl5a1lOVCtTRmlwVFdsN1UxWlVybGVLNjMyaGlYc0hvVDhGZitGWUt6eGVVRGxOVkxUN3R0MndLYjg4VGFjYmZSVnhrZjNYQXdZalpYemVEQXM4bmxzK0RCbnZEcnZQTmRMKytDS05pNjVJXC8yb2JnY0N1RmdyK1lpS0VSak8rNVZSeTIrcz0iXQ%3D%3D"
                   (list %uri-scheme %uri-authority %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("http" "files3.dsv.data.cod.ru" "/" "/" nil "WyIyNWQ3NWU5NTRmZDU1MWIzYmQ5NzVjNzJhZjRkZmNhZSIsMTI1MTA5NjMxNCwiXHUwNDEwXHUwNDNiXHUwNDRjXHUwNDRmXHUwNDNkXHUwNDQxIFx1MDQ0MVx1MDQzNVx1MDQ0MFx1MDQzZVx1MDQzYVx1MDQ0MFx1MDQ0Ylx1MDQzYlx1MDQ0Ylx1MDQ0NS5yYXIiLCJrTzNqSUo3bUN5WlBPenlBVGdcL0M3UkZVWHdXYkN6SWtEYzUweTl5a1lOVCtTRmlwVFdsN1UxWlVybGVLNjMyaGlYc0hvVDhGZitGWUt6eGVVRGxOVkxUN3R0MndLYjg4VGFjYmZSVnhrZjNYQXdZalpYemVEQXM4bmxzK0RCbnZEcnZQTmRMKytDS05pNjVJXC8yb2JnY0N1RmdyK1lpS0VSak8rNVZSeTIrcz0iXQ==" nil))))

(test destructuring5
      (is (equal (uri-template-bind (#U{uri})
                     "http://www.foo.com/abc?bar=baz&xyz=1#hash"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment %uri-head %uri-tail))
                 '("http" "www.foo.com" "/abc" "/" "abc" "bar=baz&xyz=1" "hash" "http://www.foo.com" "/abc?bar=baz&xyz=1#hash"))))

(test destructuring6
      (is (equal (uri-template-bind (#U{uri})
                     "http://user@www.foo.com:8080/dir/abc?bar=baz&xyz=1#hash"
                   (list %uri-scheme %uri-authority %uri-user %uri-host %uri-port %uri-path %uri-directory %uri-file %uri-query %uri-fragment %uri-head %uri-tail))
                 '("http" "user@www.foo.com:8080" "user" "www.foo.com" "8080" "/dir/abc" "/dir/" "abc" "bar=baz&xyz=1" "hash" "http://user@www.foo.com:8080" "/dir/abc?bar=baz&xyz=1#hash"))))

(test destructuring7
      (is (equal (uri-template-bind (#U{uri})
                     "http://www.foo.com/?bar=baz&xyz=1"
                   (list %uri-scheme %uri-host %uri-path %uri-directory %uri-file %uri-query %uri-fragment))
                 '("http" "www.foo.com" "/" "/" nil "bar=baz&xyz=1" nil))))

(test destructuring8
      (is (equal (uri-template-bind (#U{uri}) "http://user@host.com:8080/dir1/dir2/file?query=want&a=b#hash"
                   (list %uri-scheme %uri-authority (list %uri-user %uri-host %uri-port)
                         %uri-path (list %uri-directory %uri-file) %uri-query %uri-fragment))
                 '("http" "user@host.com:8080" ("user" "host.com" "8080") "/dir1/dir2/file"
                   ("/dir1/dir2/" "file") "query=want&a=b" "hash"))))

(test destructuring9
      (is (equal (uri-template-bind (#U{uri}) "/foo/bar"
                   (list %uri-head %uri-tail %uri-scheme
                         %uri-authority (list %uri-user %uri-host %uri-port)
                         %uri-path (list %uri-directory %uri-file) %uri-query %uri-fragment))
                 '(NIL "/foo/bar" NIL NIL (NIL NIL NIL) "/foo/bar" ("/foo/" "bar") NIL NIL))))


(test encoding1
      (is (string= "abc123" (uri-encode "abc123"))))

(test encoding2
      (is (string= "abc%20123" (uri-encode "abc 123"))))

(test encoding3
      (is (string= "abc123" (uri-decode "abc123"))))

(test encoding4
      (is (string= "abc 123" (uri-decode "abc%20123"))))

(test unicode-encoding1
      (is (string= "%D1%84%D0%BE%D0%BE" (uri-encode "фоо"))))

(test unicode-encoding2
      (is (string= "бар" (uri-decode "%D0%B1%D0%B0%D1%80"))))
