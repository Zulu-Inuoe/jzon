(defsystem #:com.inuoe.jzon
  :version "1.0.0"
  :description "JSON read/write"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :depends-on (#:closer-mop
               #:flexi-streams
               #:float-features
               #:uiop)
  :components ((:file "schubfach")
               (:file "jzon" :depends-on ("schubfach"))))
