;;;; -*- mode: Common-Lisp; sly-buffer-package: "asdf-user"; coding: utf-8-unix; -*-
(defsystem :yadfa
  :name "YADFA"
  :version "0.9"
  :author "Pouar"
  :mailto "pouar@pouar.net"
  :licence "GPL"
  :description "Yet Another Diaperfur Adventure"
  :long-description "Yet Another Diaperfur Adventure"
  :build-operation :program-op
  :in-order-to ((test-op (test-op :yadfa-tests)))
  :build-pathname "yadfa"
  :entry-point "yadfa::main"
  :depends-on (:marshal :iterate :ugly-tiny-infix-macro :closer-mop :clim-listener :mcclim-raster-image :trivial-garbage :cl-ansi-text
               :alexandria :serapeum :global-vars :float-features :asdf :uiop :illogical-pathnames :fmt
                        (:feature :sbcl :yadfa/docs) (:feature :ecl :fare-quasiquote-extras))
  :components ((:file "packages")
               (:file "main" :depends-on ("packages" "core"))
               (:module "core"
                :depends-on ("packages")
                :components ((:file "util" :depends-on ("init"))
                             (:file "structs" :depends-on ("init"))
                             (:file "init")
                             (:file "libexec" :depends-on ("util" "classes" "patches" "init" "structs"))
                             (:file "classes" :depends-on ("util" "patches" "init"))
                             (:file "game" :depends-on ("classes" "init"))
                             (:file "bin" :depends-on ("libexec" "init"))
                             (:file "patches" :depends-on ("init"))
                             (:file "mcclim" :depends-on ("patches" "bin" "init"))))
               (:module "data"
                :depends-on ("core")
                :components ((:module "moves"
                              :depends-on ("prolog")
                              :components ((:file "dbz") (:file "haunted") (:file "pokemon") (:file "regular")))
                             (:module "items"
                              :depends-on ("moves" "prolog")
                              :components ((:file "abdl") (:file "armor") (:file "clothes") (:file "consumable")
                                                          (:file "debug") (:file "diaper") (:file "misc") (:file "weapons")))
                             (:module "prolog"
                              :components ((:file "allies") (:file "enemies") (:file "map")))
                             (:module "enemies"
                              :depends-on ("moves" "items" "prolog")
                              :components ((:file "eggbots") (:file "fursuiters") (:file "haunted") (:file "navy")
                                                             (:file "pirates") (:file "pokemon") (:file "raccoon-bandits")
                                                             (:file "rpgmaker")))
                             (:module "team-members"
                              :depends-on ("moves" "items" "prolog")
                              :components ((:file "allies") (:file "catchables")))
                             (:module "props"
                              :depends-on ("items" "enemies" "team-members" "prolog")
                              :components ((:file "base") (:file "toilets") (:file "washers")))
                             (:module "events"
                              :depends-on ("moves" "items" "enemies" "team-members" "props" "prolog")
                              :components ((:file "bandits-domain") (:file "base") (:file "debug") (:file "dirty-chasm")
                                                                    (:file "ironside") (:file "lukurbo") (:file "peachs-castle-wannabe")
                                                                    (:file "pirates-cove") (:file "pyramid") (:file "secret-underground")
                                                                    (:file "silver-cape")))
                             (:module "map"
                              :depends-on ( "moves" "items" "enemies" "team-members" "props" "events" "prolog")
                              :components ((:file "bandits-domain") (:file "debug-map") (:file "dirty-chasm")
                                                                    (:file "haunted-forest") (:file "haunted-house") (:file "home")
                                                                    (:file "ironside") (:file "lukurbo") (:file "peachs-castle-wannabe")
                                                                    (:file "pirates-cove") (:file "pyramid") (:file "rpgmaker-dungeon")
                                                                    (:file "secret-underground") (:file "silver-cape") (:file "sky")
                                                                    (:file "your-ship")))
                             (:module "status-conditions"
                              :depends-on ("prolog")
                              :components ((:file "abdl") (:file "misc") (:file "pokemon")))
                             (:module "epilog"
                              :depends-on ("prolog" "enemies" "events" "items" "map" "moves" "props" "status-conditions" "team-members")
                              :components ((:file "allies") (:file "blackjack") (:file "enemies") (:file "items")
                                                            (:file "puzzle" :depends-on ("pyramid")) (:file "pyramid")))))))
(defsystem :yadfa/docs
  :depends-on (:net.didierverna.declt)
  :description "Used for building the docs. Contains patches to Declt for using Texinfo commands in docstrings"
  :components ((:module "core"
                :components ((:file "declt-patches")))))
