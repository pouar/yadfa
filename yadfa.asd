;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(defsystem "yadfa"
  :name "YADFA"
  :version "0.9"
  :author "Pouar"
  :mailto "pouar@pouar.net"
  :licence "GPL"
  :description "Yet Another Diaperfur Adventure"
  :long-description "Yet Another Diaperfur Adventure"
  :build-operation :program-op
  :build-pathname "yadfa"
  :entry-point "yadfa::main"
  :depends-on ("marshal" "iterate" "ugly-tiny-infix-macro" "closer-mop" "trivial-features" "clim-listener" "trivial-garbage" "macro-level" "cl-ansi-text" "alexandria")
  :components ((:file "packages")
               (:file "main" :depends-on ("packages" "core"))
               (:module "core"
                :depends-on ("packages")
                :components ((:file "util" :depends-on ("init"))
                             (:file "structs" :depends-on ("init"))
                             (:file "init" :depends-on ("patches"))
                             (:file "libexec" :depends-on ("util" "classes" "init" "structs"))
                             (:file "classes" :depends-on ("util" "patches" "init"))
                             (:file "game" :depends-on ("classes" "init"))
                             (:file "bin" :depends-on ("libexec" "init"))
                             (:file "patches")
                             (:file "mcclim" :depends-on ("patches" "bin" "init"))))
               (:module "data"
                :depends-on ("core")
                :components ((:module "moves"
                              :depends-on ("init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "moves")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "items"
                              :depends-on ("moves" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "items")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "init"
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "init")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "enemies"
                              :depends-on ("moves" "items" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "enemies")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "team-members"
                              :depends-on ("moves" "items" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "team-members")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "props"
                              :depends-on ("items" "enemies" "team-members" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "props")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "events"
                              :depends-on ("moves" "items" "enemies" "team-members" "props" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "events")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "map"
                              :depends-on ( "moves" "items" "enemies" "team-members" "props" "events" "init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "map")
                                                      :name :wild
                                                      :type "lisp"))))
                             (:module "status-conditions"
                              :depends-on ("init")
                              :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                    (directory-files
                                                     (pathname-directory-pathname
                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                     (make-pathname
                                                      :directory '(:relative "data" "status-conditions")
                                                      :name :wild
                                                      :type "lisp"))))))))
(defsystem "yadfa/docs"
  :depends-on ("net.didierverna.declt")
  :description "Used for building the docs. Contains patches to Declt for using Texinfo commands in docstrings"
  :components ((:module "core"
                :components ((:file "declt-patches")))))
