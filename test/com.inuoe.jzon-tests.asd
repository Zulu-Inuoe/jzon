(defsystem #:com.inuoe.jzon-tests
  :version "0.0.0"
  :description "Tests for the jzon library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :components
  ((:file "jzon-tests"))
  :depends-on
  (#:alexandria
   #:fiveam
   #:com.inuoe.jzon))
