(in-package #:yadfa)
(defun main ()
    #+slynk (when (and
                      #+yadfa-docs (not (position "texi" (uiop:command-line-arguments) :test #'string=))
                      #-yadfa-docs t
                      uiop:*image-dumped-p*
                      (not slynk::*servers*)
                      (position "slynk" (uiop:command-line-arguments) :test #'string=))
                (slynk::create-server :dont-close t))
    #+swank (when (and #+yadfa-docs (not (position "texi" (uiop:command-line-arguments) :test #'string=))
                      #-yadfa-docs t
                      uiop:*image-dumped-p*
                      (not swank::*servers*)
                      (position "swank" (uiop:command-line-arguments) :test #'string=))
                (swank::create-server :dont-close t))
    (in-package #:yadfa)
    (when (position "wait" (uiop:command-line-arguments) :test #'string=)
        (sleep 2))
    #+ironclad (setf ironclad:*prng* (ironclad:make-prng :os)) ; work around crash
    (load-mods)
    (let ((file (uiop:xdg-config-home "yadfa/yadfarc")))
        (when (probe-file file)
            (load file)))
    (in-package :yadfa-user)
    #+yadfa-docs (when (position "texi" (uiop:command-line-arguments) :test #'string=)
                     (net.didierverna.declt:declt :yadfa
                         :license :gpl
                         :texi-name "yadfa-reference"
                         :texi-directory (uiop:merge-pathnames*
                                             "docs/reference/"
                                             (if uiop:*image-dumped-p*
                                                 (pathname (directory-namestring (truename (uiop:argv0))))
                                                 (asdf:system-source-directory :yadfa)))
                         :introduction "Yadfa is yet another diaperfur game, written in Common Lisp. This here is the reference manual for it which is generated automatically")
                     (uiop:quit))
    (use-package :yadfa-world :yadfa-user)
    #+mcclim-ffi-freetype (setf freetype2:*library* (freetype2:make-freetype))
    (clim-listener:run-listener
        :package :yadfa-user
        :process-name "yadfa"
        :height 768
        :width 1024))
