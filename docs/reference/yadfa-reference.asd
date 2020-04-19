;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(defsystem "yadfa-reference"
  :defsystem-depends-on ("yadfa-asdf")
  :depends-on ("yadfa" "yadfa/docs")
  :components (("yadfa-asdf:declt-texi" "yadfa-reference")
               ("yadfa-asdf:make" "Makefile"
                                  :depends-on ("yadfa-reference")
                                  :outputs #.(loop :for i :in '("dvi" "html" "info" "pdf" "ps")
                                                   :collect `(:type ,i :name "yadfa-reference")))))
