;; -*- mode: common-lisp; -*-
(declaim (optimize (safety 3)))
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
#+(and gmp sbcl) (require 'sb-gmp)
#+(and sbcl gmp) (sb-gmp:install-gmp-funs)
(ql:update-client
 :prompt nil)
(ql:update-all-dists
 :prompt nil)
(when (and
       (ql-dist:find-dist "ultralisp")
       (ql-dist:installedp (ql-dist:find-dist "ultralisp")))
  (ql-dist:install-dist "http://dist.ultralisp.org/"
                        :prompt nil))
(pushnew :yadfa-mods *features*)
(asdf:make :yadfa)
(#__exit 0)
