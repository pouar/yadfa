;;;; -*- Mode: Common-Lisp; -*
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
    :depends-on ("marshal" "iterate" "ugly-tiny-infix-macro" "closer-mop" "trivial-features" "clim-listener" "trivial-garbage" "macro-level" "cl-ansi-text" "alexandria"
                    (:feature :slynk "slynk")
                    (:feature :swank "swank") (:feature :yadfa-docs "net.didierverna.declt"))
    :components ((:file "packages")
                    (:file "main" :depends-on ("packages"))
                    (:module "core"
                        :depends-on ("packages" "main")
                        :components ((:file "util")
                                        (:file "structs")
                                        (:file "init" :depends-on ("patches"))
                                        (:file "libexec" :depends-on ("util" "classes" "init" "structs"))
                                        (:file "classes" :depends-on ("util" "patches"))
                                        (:file "game" :depends-on ("classes"))
                                        (:file "bin" :depends-on ("libexec"))
                                        (:file "patches")
                                        (:file "mcclim" :depends-on ("patches" "bin"))))
                    (:module "data"
                        :depends-on ("core")
                        :components ((:module "moves"
                                         :depends-on ("init")
                                         :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                           (directory-files
                                                               (pathname-directory-pathname
                                                                   (uiop/lisp-build:current-lisp-file-pathname))
                                                               "data/moves/*.lisp")))
                                        (:module "items"
                                            :depends-on ("moves" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/items/*.lisp")))
                                        (:module "init"
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/init/*.lisp")))
                                        (:module "enemies"
                                            :depends-on ("moves" "items" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/enemies/*.lisp")))
                                        (:module "team-members"
                                            :depends-on ("moves" "items" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/team-members/*.lisp")))
                                        (:module "props"
                                            :depends-on ("items" "enemies" "team-members" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/props/*.lisp")))
                                        (:module "events"
                                            :depends-on ("moves" "items" "enemies" "team-members" "props" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/events/*.lisp")))
                                        (:module "map"
                                            :depends-on ( "moves" "items" "enemies" "team-members" "props" "events" "init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/map/*.lisp")))
                                        (:module "status-conditions"
                                            :depends-on ("init")
                                            :components #.(mapcar (lambda (p) (list :file (pathname-name p)))
                                                              (directory-files
                                                                  (pathname-directory-pathname
                                                                      (uiop/lisp-build:current-lisp-file-pathname))
                                                                  "data/status-conditions/*.lisp")))))))
