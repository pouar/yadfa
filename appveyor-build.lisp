;; -*- mode: common-lisp; -*-
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                          (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init)))
#+(and gmp sbcl) (require 'sb-gmp)
#+(and sbcl gmp) (sb-gmp:install-gmp-funs)
(pushnew :yadfa/mods *features*)
(pushnew :ironclad *features*)
(asdf:make :yadfa)
(#__exit 0)
