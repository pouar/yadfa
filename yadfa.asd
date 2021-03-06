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
                :components ((:file "util")
                             (:module "libexec" :depends-on ("util" "patches")
                              :components ((:file "declares")
                                           (:file "macros" :depends-on ("declares" "init"))
                                           (:file "functions" :depends-on ("macros" "declares" "init"))
                                           (:file "conditions" :depends-on ("declares"))
                                           (:file "methods" :depends-on ("classes" "generic-functions" "macros" "declares" "functions" "conditions"))
                                           (:file "generic-functions" :depends-on ("classes" "macros" "declares" "functions"))
                                           (:file "classes" :depends-on ("init" "declares"))
                                           (:file "hooks" :depends-on ("declares" "init"))
                                           (:file "game" :depends-on ("classes" "init" "declares"))
                                           (:file "mcclim" :depends-on ("init" "declares" "functions" "generic-functions" "macros"))
                                           (:file "structs" :depends-on ("init" "declares"))
                                           (:file "init" :depends-on ("declares"))))
                             (:module "bin" :depends-on ("libexec")
                              :components ((:file "bin") (:file "world") (:file "battle")))
                             (:file "patches")))
               (:module "data"
                :depends-on ("core")
                :components ((:module "moves"
                              :depends-on ("prolog" "element-types")
                              :components ((:file "dbz") (:file "haunted") (:file "pokemon") (:file "regular")))
                             (:module "items"
                              :depends-on ("moves" "prolog")
                              :components ((:file "abdl") (:file "armor") (:file "clothes") (:file "consumable")
                                           (:file "debug") (:file "diaper") (:file "misc") (:file "weapons")))
                             (:module "prolog"
                              :components ((:file "allies") (:file "enemies") (:file "map")))
                             (:module "enemies"
                              :depends-on ("moves" "items" "prolog" "element-types")
                              :components ((:file "eggbots") (:file "fursuiters") (:file "haunted") (:file "navy")
                                           (:file "pirates") (:file "pokemon") (:file "raccoon-bandits")
                                           (:file "rpgmaker")))
                             (:module "element-types"
                              :depends-on ("prolog")
                              :components ((:file "pokemon")))
                             (:module "team-members"
                              :depends-on ("moves" "items" "prolog")
                              :components ((:file "allies") (:file "catchables")))
                             (:module "props"
                              :depends-on ("items" "enemies" "team-members" "prolog")
                              :components ((:file "base")
                                           (:module "props"
                                            :depends-on ("base")
                                            :components ((:file "toilets") (:file "washers") (:file "beds")))))
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
                              :depends-on ("prolog" "enemies" "events" "items" "map" "moves" "props" "status-conditions" "team-members" "element-types")
                              :components ((:file "allies") (:file "blackjack") (:file "enemies") (:file "items")
                                           (:file "puzzle" :depends-on ("pyramid")) (:file "pyramid")))))))
(defsystem :yadfa/docs
  :depends-on (:net.didierverna.declt)
  :description "Used for building the docs. Contains patches to Declt for using Texinfo commands in docstrings"
  :components ((:module "core"
                :components ((:file "declt-patches")))))
