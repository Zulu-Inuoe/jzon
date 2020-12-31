(defpackage #:com.inuoe.jzon-parsing
  (:use #:cl)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:import-from
   #:alexandria
   #:eswitch
   #:read-stream-content-into-string)
  (:import-from #:cl-json)
  (:import-from #:jonathan)
  (:import-from #:jsown)
  (:import-from #:yason)
  (:export
   #:main))

(in-package #:com.inuoe.jzon-parsing)

(defun parser-fn (name)
  (eswitch (name :test #'string-equal)
    ("cl-json" #'cl-json:decode-json-from-source)
    ("jzon" #'jzon:parse)
    ("jonathan" (lambda (s) (jonathan:parse (read-stream-content-into-string s))))
    ("jsown" (lambda (s) (jsown:parse (read-stream-content-into-string s))))
    ("yason" #'yason:parse)))

(defun main (&rest argv)
  (prog ((file (first argv))
         (parser-name (or (second argv) "jzon")))
     (unless file
       (format *error-output* "Missing input file.~%")
       (return 2))
     (when (string= file "--help")
       (format t "usage:
  jzon-parsing test-file [parser-name]
")
       (return 0))
     (let ((parser (parser-fn parser-name)))
       (with-open-file (stream file :direction :input :if-does-not-exist nil :external-format :utf8)
         (unless stream
           (format *error-output* "Error opening '~A'" file)
           (return 2))
         (return
           (if (ignore-errors (funcall parser stream)
                              t)
               0
               1))))))
