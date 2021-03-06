;; -*- mode: common-lisp; -*-
#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning)
         (optimize sb-c::recognize-self-calls))
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
#+ccl (ccl:set-current-compiler-policy (ccl:new-compiler-policy :trust-declarations (lambda (env)
                                                                                      (declare (ignore env)) nil)))

#+(and sbcl (not sb-gmp))
(macrolet ((a ()
             `(progn
                ,(when (some #'(lambda (pathname)
                                 (handler-case
                                     (sb-alien:load-shared-object pathname :dont-save t)
                                   (error (e) (declare (ignore e)) nil)))
                             #-(or os-windows :os-macosx) '("libgmp.so" "libgmp.so.10" "libgmp.so.3")
                             #+:os-macosx '("libgmp.dylib" "libgmp.10.dylib" "libgmp.3.dylib")
                             #+os-windows '("libgmp.dll" "libgmp-10.dll" "libgmp-3.dll"))
                   '(asdf:load-system :sb-gmp)))))
  (a))
#+sb-gmp (unless (eq (fdefinition 'sb-bignum:multiply-bignums) (fdefinition 'sb-gmp::gmp-mul))
           (sb-gmp:install-gmp-funs))
(when (find "slynk" (uiop:command-line-arguments) :test #'string=)
  (ql:quickload "slynk"))
(when (find "swank" (uiop:command-line-arguments) :test #'string=)
  (ql:quickload "swank"))
(when (find "ft" (uiop:command-line-arguments) :test #'string=)
  (pushnew :mcclim-ffi-freetype *features*))
(setf ql:*quickload-verbose* t)
(let ((*compile-verbose* nil) (*compile-print* nil))
  (ql:quickload (loop for i in (asdf:system-depends-on (asdf:find-system :yadfa))
                      when (stringp i) collect i
                        when (and (listp i) (eq (first i) :feature) (uiop:featurep (second i))) collect (third i)))
  (declaim (optimize (debug 2) safety))
  (ql:quickload :yadfa))
(when (find "immutable" (uiop:command-line-arguments) :test #'string=)
  (setf yadfa::*immutable* t))
(when (find "docs" (uiop:command-line-arguments) :test #'string=)
  (declaim (optimize (debug 1) (safety 1)))
  (ql:quickload :yadfa-reference)
  (declaim (optimize (debug 2) safety)))
(when (probe-file (uiop:merge-pathnames* (make-pathname :name "yadfa") (asdf:component-pathname (asdf:find-system "yadfa"))))
  (delete-file (uiop:merge-pathnames* (make-pathname :name "yadfa") (asdf:component-pathname (asdf:find-system "yadfa")))))
(asdf:make :yadfa :force (when (find "force" (uiop:command-line-arguments) :test #'string=) t))
