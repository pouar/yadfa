(defsystem "yadfa-mod-example"
    :name "Example Mod"
    :version "1.0"
    :author "Pouar"
    :mailto "pouar@pouar.net"
    :license "MIT"
    :components ((:file "package")
                    (:file "example" :depends-on ("package"))))
