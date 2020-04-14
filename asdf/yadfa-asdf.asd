;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(defsystem "yadfa-asdf"
  :description "ASDF extensions intended to the Makefiles for the docs with ASDF. Used internally."
  :long-description "ASDF extensions intended to integrate the docs and it's dependencies with ASDF, as make has absolutely no knowledge of the rest of the game which these docs depend on in order to generate the references. Giving ASDF knowledge of this and let it invoke make instead of having make guess that these dependencies are satisfied seemed like the most reliable solution."
  :depends-on ("asdf" "uiop" "iterate" "bibtex" "cl-date-time-parser" "local-time" "net.didierverna.declt")
  :components ((:file "yadfa-asdf")))
