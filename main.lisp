(in-package #:yadfa)
(defun main ()
    #+sbcl (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning sb-ext:code-deletion-note))
    #+slynk (when (and
                      #+yadfa/docs (not (position "texi" (uiop:command-line-arguments) :test #'string=))
                      #-yadfa/docs t
                      uiop:*image-dumped-p*
                      (not slynk::*servers*)
                      (position "slynk" (uiop:command-line-arguments) :test #'string=))
                (slynk::create-server :dont-close t))
    #+swank (when (and #+yadfa/docs (not (position "texi" (uiop:command-line-arguments) :test #'string=))
                      #-yadfa/docs t
                      uiop:*image-dumped-p*
                      (not swank::*servers*)
                      (position "swank" (uiop:command-line-arguments) :test #'string=))
                (swank::create-server :dont-close t))
    (in-package #:yadfa)
    (when (position "wait" (uiop:command-line-arguments) :test #'string=)
        (sleep 2))
    (init-game)
    (in-package :yadfa-user)
    #+yadfa/docs (when (position "texi" (uiop:command-line-arguments) :test #'string=)
                     (net.didierverna.declt:declt :yadfa :license :gpl :introduction "Yadfa is yet another diaperfur game, written in Common Lisp. This here is the reference manual for it which is generated automatically")
                     (uiop:quit))
    (use-package :yadfa/world :yadfa-user)
    #+mcclim-ffi-freetype (setf freetype2:*library* (freetype2:make-freetype))
    #-mcclim-ffi-freetype (mcclim-truetype::register-all-ttf-fonts (clim:find-port)
                              (if uiop:*image-dumped-p*
                                  (pathname (directory-namestring (truename (uiop:argv0))))
                                  (asdf:system-source-directory :yadfa)))
    (setf clim-listener::*default-text-style*
        (clim-listener::make-text-style "Fantasque Sans Mono" "Regular" 12))
    (clim-listener:run-listener
        :package :yadfa-user
        :process-name "yadfa"
        :height 600
        :width 800))
