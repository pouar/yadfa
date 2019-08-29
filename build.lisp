;; -*- mode: common-lisp; -*-
#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning sb-ext:code-deletion-note))
(setf *read-default-float-format* 'long-float)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(macrolet ((a ()
             `(progn
                ,(when (position "debug" (uiop:command-line-arguments) :test #'string=)
                   '(declaim (optimize (debug 3))))
                #+sbcl
                ,(when (some #'(lambda (pathname)
                                 (handler-case
                                     (sb-alien:load-shared-object pathname :dont-save t)
                                   (error (e) (declare (ignore e)) nil)))
                             #-(or win32 darwin) '("libgmp.so" "libgmp.so.10" "libgmp.so.3")
                             #+darwin '("libgmp.dylib" "libgmp.10.dylib" "libgmp.3.dylib")
                             #+win32 '("libgmp.dll" "libgmp-10.dll" "libgmp-3.dll"))
                   '(asdf:load-system :sb-gmp)))))
  (a))
#+sb-gmp (sb-gmp:install-gmp-funs)
(when (position "slynk" (uiop:command-line-arguments) :test #'string=)
  (ql:quickload "slynk"))
(when (position "swank" (uiop:command-line-arguments) :test #'string=)
  (ql:quickload "swank"))
(when (position "ft" (uiop:command-line-arguments) :test #'string=)
  (pushnew :mcclim-ffi-freetype *features*))
(ql:quickload :yadfa)
(when (probe-file (uiop:merge-pathnames* (make-pathname :name "yadfa") (asdf:component-pathname (asdf:find-system "yadfa"))))
  (delete-file (uiop:merge-pathnames* (make-pathname :name "yadfa") (asdf:component-pathname (asdf:find-system "yadfa")))))
(asdf:make :yadfa :force (when (position "force" (uiop:command-line-arguments) :test #'string=) t))
