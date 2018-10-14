;;;; -*- Mode: Common-Lisp; -*-
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
    :serial t
    :depends-on ("marshal" "iterate" "ugly-tiny-infix-macro" "closer-mop" "trivial-features" "ironclad" "clim-listener" "trivial-garbage" "macro-level" "cl-ansi-text" "alexandria"
                    (:feature :sbcl "sb-aclrepl") (:feature :slynk "slynk")
                    (:feature :swank "swank") (:feature :yadfa/docs "net.didierverna.declt"))
    :components (
                    (:file "packages")
                    (:file "gui/mcclim")
                    (:file "main" :depends-on ("packages" "gui/mcclim"))
                    (:module "core"
                        :depends-on ("packages" "main")
                        :components (
                                        (:file "structs")
                                        (:file "init")
                                        (:file "libexec" :depends-on ("classes"))
                                        (:file "classes")
                                        (:file "bin")))
                    (:module "data"
                        :depends-on ("core")
                        :components (
                                        (:module "moves"
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/moves/*.lisp")))
                                        (:module "items"
                                            :depends-on ("moves")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/items/*.lisp")))
                                        (:module "enemies"
                                            :depends-on ("moves" "items")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/enemies/*.lisp")))
                                        (:module "props"
                                            :depends-on ("items" "enemies")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/props/*.lisp")))
                                        (:module "events"
                                            :depends-on ("moves" "items" "enemies" "props")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/events/*.lisp")))
                                        (:module "map"
                                            :depends-on ( "moves" "items" "enemies" "props" "events")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/map/*.lisp")))
                                        (:module "status-conditions"
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/status-conditions/*.lisp")))))))
