;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package #:yadfa)
(defun main ()
  (proclaim '(optimize safety (debug 2)))
  (when yadfa::*immutable*
    (map () 'asdf:register-immutable-system (asdf:already-loaded-systems)))
  (pushnew
   'yadfa::find-mod
   asdf:*system-definition-search-functions*)
  (uiop:register-clear-configuration-hook 'clear-configuration-hook)
  (asdf:clear-configuration)
  (setf *random-state* (make-random-state t))
  (when (and (not (find "texi" (uiop:command-line-arguments) :test #'string=))
             (position "slynk" (uiop:command-line-arguments) :test #'string=))
    (when (or (and (uiop:featurep :slynk) uiop:*image-dumped-p* (not (symbol-value (uiop:find-symbol* '#:*servers* '#:slynk))))
              (when (and (asdf:find-system "slynk" nil)
                         (not (asdf:component-loaded-p "slynk")))
                (asdf:load-system "slynk")))
      (uiop:symbol-call '#:slynk '#:create-server :dont-close t)))
  (when (and (not (find "texi" (uiop:command-line-arguments) :test #'string=))
             (position "swank" (uiop:command-line-arguments) :test #'string=))
    (when (or (and (uiop:featurep :swank) uiop:*image-dumped-p* (not (symbol-value (uiop:find-symbol* '#:*servers* '#:swank))))
              (when (and (asdf:find-system "swank" nil)
                         (not (asdf:component-loaded-p "swank")))
                (asdf:load-system "swank")))
      (uiop:symbol-call '#:swank '#:create-server :dont-close t)))
  (in-package #:yadfa)
  (when (position "wait" (uiop:command-line-arguments) :test #'string=)
    (sleep 2))
  (trigger-event 'yadfa-zones::create-rpgmaker-dungeon)
  (load-mods)
  (load #P"yadfa:config;yadfarc" :if-does-not-exist nil)
  (in-package :yadfa-user)
  (when (find "test" (uiop:command-line-arguments) :test #'string=)
    (handler-case (progn (asdf:test-system :yadfa)
                         (uiop:quit))
      (error () (uiop:quit 1))))
  (when (uiop:featurep :mcclim-ffi-freetype)
    (setf (symbol-value (uiop:find-symbol* '#:*library* '#:freetype2))
          (uiop:symbol-call '#:freetype2 '#:make-freetype)))
  (yadfa-clim:run-listener))
