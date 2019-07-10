(in-package #:yadfa)
(defun main ()
  #+slynk (when (and #+yadfa-docs (not (find "texi" (uiop:command-line-arguments) :test #'string=))
                     #-yadfa-docs t
                     uiop:*image-dumped-p*
                     (not slynk::*servers*)
                     (position "slynk" (uiop:command-line-arguments) :test #'string=))
            (slynk::create-server :dont-close t))
  #+swank (when (and #+yadfa-docs (not (find "texi" (uiop:command-line-arguments) :test #'string=))
                     #-yadfa-docs t
                     uiop:*image-dumped-p*
                     (not swank::*servers*)
                     (position "swank" (uiop:command-line-arguments) :test #'string=))
            (swank::create-server :dont-close t))
  (in-package #:yadfa)
  (when (position "wait" (uiop:command-line-arguments) :test #'string=)
    (sleep 2))
  (set-logical-pathnames)
  (load-mods)
  (let ((file #P"yadfa:config;yadfarc"))
    (when (probe-file file)
      (load file)))
  (in-package :yadfa-user)
  #+yadfa-docs (when (find "texi" (uiop:command-line-arguments) :test #'string=)
                 (net.didierverna.declt:declt :yadfa
                                              :license :gpl
                                              :texi-name "yadfa-reference"
                                              :texi-directory (translate-logical-pathname "yadfa:home;docs;reference;")
                                              :introduction "Yadfa is yet another diaperfur game, written in Common Lisp. This here is the reference manual for it which is generated automatically")
                 (uiop:quit))
  (use-package :yadfa-world :yadfa-user)
  #+mcclim-ffi-freetype (setf freetype2:*library* (freetype2:make-freetype))
  (clim-listener:run-listener
   :package :yadfa-user
   :process-name "yadfa"
   :height 768
   :width 1024))
