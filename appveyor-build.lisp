;; -*- mode: common-lisp; -*-
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
#|
(when (and
       (ql-dist:find-dist "ultralisp")
       (ql-dist:installedp (ql-dist:find-dist "ultralisp")))
  (ql-dist:install-dist "http://dist.ultralisp.org/"
                        :prompt nil))
|#
(ql:quickload (loop for i in (asdf:system-depends-on (asdf:find-system :yadfa))
                    when (stringp i) collect i
                    when (and (listp i) (eq (first i) :feature) (uiop:featurep (second i))) collect (third i)))
(declaim (optimize (debug 2) safety))
(setf *read-default-float-format* 'long-float)
(ql:quickload :yadfa)
(setf yadfa::*immutable* t)
(asdf:make :yadfa)
(uiop:quit)
