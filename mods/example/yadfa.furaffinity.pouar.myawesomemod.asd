(defsystem "yadfa.furaffinity.pouar.myawesomemod"
  :name "Example Mod"
  :version "1.0"
  :author "Pouar"
  :mailto "pouar@pouar.net"
  :license "MIT"
  :depends-on ("yadfa")
  :components ((:file "package")
               (:file "example" :depends-on ("package"))))
