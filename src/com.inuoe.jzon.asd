(defsystem #:com.inuoe.jzon
  :version "1.1.0"
  :description "A correct and safe(er) JSON RFC 8259 parser with sane defaults."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :depends-on (#:closer-mop
               #:flexi-streams
               (:feature (:not :ecl) #:float-features)
               #:trivial-gray-streams
               #:uiop)
  :components ((:file "eisel-lemire")
               (:file "ratio-to-double")
               (:file "schubfach")
               (:file "jzon" :depends-on ("eisel-lemire" "ratio-to-double" "schubfach"))))
