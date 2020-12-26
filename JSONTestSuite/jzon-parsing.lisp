(defpackage #:com.inuoe.jzon-parsing
  (:use #:cl)
  (:import-from
   #:com.inuoe.jzon
   #:parse)
  (:export
   #:main))

(in-package #:com.inuoe.jzon-parsing)

(defun main (&rest argv)
  (prog ((file (first argv)))
     (unless file (return 2))
     (with-open-file (stream file :direction :input :if-does-not-exist nil)
       (unless stream (return 2))
       (return
         (if (ignore-errors (com.inuoe.jzon:parse stream)
                            t)
             0
             1)))))
