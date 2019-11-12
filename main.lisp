;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package #:yadfa)
(defun build-texi ()
  (if (asdf:component-loaded-p "yadfa/docs")
      (uiop:symbol-call '#:net.didierverna.declt '#:declt :yadfa
                        :license :gpl
                        :texi-name "yadfa-reference"
                        :texi-directory (translate-logical-pathname "yadfa:home;docs;reference;")
                        :introduction "Yadfa is yet another diaperfur game, written in Common Lisp. This here is the reference manual for it which is generated automatically")
      (format *error-output* "Can't build texi file on ~a~%" (lisp-implementation-type))))
(defun main ()
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
  (when (find "texi" (uiop:command-line-arguments) :test #'string=)
    (build-texi)
    (uiop:quit))
  (switch-user-packages)
  (when (featurep :mcclim-ffi-freetype)
    (setf (symbol-value (uiop:find-symbol* '#:*library* '#:freetype2))
          (uiop:symbol-call '#:freetype2 '#:make-freetype)))
  (clim-listener:run-listener
   :package :yadfa-user
   :process-name "yadfa"
   :height 768
   :width 1024))
