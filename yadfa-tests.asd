;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(asdf:defsystem :yadfa-tests
  :depends-on (:yadfa :fiveam)
  :defsystem-depends-on (:fiveam-asdf)
  :class :fiveam-tester-system
  :test-package :yadfa-tests
  :test-names (#:all-tests)
  :components ((:module "t" :serial t
                :components ((:file "package")
                             (:file "main")))))
