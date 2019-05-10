;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(defsystem "yadfa-guide"
  :defsystem-depends-on ("yadfa-asdf")
  :depends-on ("yadfa" "yadfa/docs")
  :components (("yadfa-asdf:bibtex-texi" "pouar")
               ("yadfa-asdf:make" "Makefile"
                                  :depends-on ("pouar")
                                  :outputs #.`(,@(loop :for i :in '("dvi" "html" "info" "pdf" "ps")
                                                       :collect `(:type ,i :name "yadfa-guide"))
                                               ,@(loop :for i :in '("fig1" "title")
                                                       :appending (loop :for j :in '("eps" "png" "pdf")
                                                                        :unless (and (equal i "fig1") (equal j "pdf"))
                                                                        :collect `(:name ,i :type ,j :directory (:relative "yadfa-guide-figures"))))))))
