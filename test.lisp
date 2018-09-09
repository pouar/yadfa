;; -*- mode: common-lisp; -*-
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                          (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init)))
#+(and gmp sbcl) (require 'sb-gmp)
#+(and sbcl gmp) (sb-gmp:install-gmp-funs)
(handler-bind ((error #'(lambda (c) (continue))))
    (ql:quickload :clim-listener))
(ql:quickload :yadfa)
(in-package #:yadfa)
(init-game)
#+(and ccl win32) (#__exit 0) ; damn it Windows
(uiop:quit)
