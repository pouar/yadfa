;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package #:yadfa)
(defun main ()
  (pushnew
   'yadfa::find-mod
   asdf:*system-definition-search-functions*)
  (uiop:register-clear-configuration-hook 'initialize-mod-registry)
  (uiop:register-clear-configuration-hook 'clear-pattern-cache)
  (uiop:register-clear-configuration-hook 'set-logical-pathnames)
  (asdf:clear-configuration)
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
  (load-mods)
  (let ((file #P"yadfa:config;yadfarc"))
    (when (probe-file file)
      (load file)))
  (in-package :yadfa-user)
  (when (find "texi" (uiop:command-line-arguments) :test #'string=)
    (when (asdf:component-loaded-p "yadfa/docs")
      (uiop:symbol-call '#:net.didierverna.declt '#:declt :yadfa
                        :license :gpl
                        :texi-name "yadfa-reference"
                        :texi-directory (translate-logical-pathname "yadfa:home;docs;reference;")
                        :introduction "Yadfa is yet another diaperfur game, written in Common Lisp. This here is the reference manual for it which is generated automatically"))
    (uiop:quit))
  (use-package :yadfa-world :yadfa-user)
  (when (featurep :mcclim-ffi-freetype)
    (setf (symbol-value (uiop:find-symbol* '#:*library* '#:freetype2))
          (uiop:symbol-call '#:freetype2 '#:make-freetype)))
  (clim-listener:run-listener
   :package :yadfa-user
   :process-name "yadfa"
   :height 768
   :width 1024))
