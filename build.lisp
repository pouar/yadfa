;; -*- mode: common-lisp; -*-

;;; https://bugs.launchpad.net/sbcl/+bug/1811623
#+sbcl
(in-package :sb-c)
#+sbcl
(defun convert-mv-bind-to-let (call)
    (declare (type mv-combination call))
    (let* ((args (basic-combination-args call))
              (use (lvar-uses (first args))))
        (when (and (singleton-p args)
                  (combination-p use)
                  (eq (lvar-fun-name (combination-fun use))
                      'values))
            (setf (lvar-reoptimize (car args)) nil)
            (let* ((fun (combination-lambda call))
                      (vars (lambda-vars fun))
                      (vals (combination-args use))
                      (nvars (length vars))
                      (nvals (length vals)))
                (cond ((> nvals nvars)
                          (mapc #'flush-dest (subseq vals nvars))
                          (setq vals (subseq vals 0 nvars)))
                    ((< nvals nvars)
                        (with-ir1-environment-from-node use
                            (let ((node-prev (node-prev use)))
                                (setf (node-prev use) nil)
                                (setf (ctran-next node-prev) nil)
                                (collect ((res vals))
                                    (loop for count below (- nvars nvals)
                                        for prev = node-prev then ctran
                                        for ctran = (make-ctran)
                                        and lvar = (make-lvar use)
                                        do (reference-constant prev ctran lvar nil)
                                        (res lvar)
                                        finally (link-node-to-previous-ctran
                                                    use ctran))
                                    (setq vals (res)))))))
                (setf (combination-args use) vals)
                (flush-dest (combination-fun use))
                (let ((fun-lvar (basic-combination-fun call)))
                    (setf (lvar-dest fun-lvar) use)
                    (setf (combination-fun use) fun-lvar)
                    (flush-lvar-externally-checkable-type fun-lvar))
                (setf (combination-kind use) :local)
                (setf (functional-kind fun) :let)
                (flush-dest (first (basic-combination-args call)))
                (unlink-node call)
                (when vals
                    (reoptimize-lvar (first vals)))
                ;; Propagate derived types from the VALUES call to its args:
                ;; transforms can leave the VALUES call with a better type
                ;; than its args have, so make sure not to throw that away.
                (let ((types (values-type-types (node-derived-type use))))
                    (dolist (val vals)
                        (when types
                            (let ((type (pop types)))
                                (assert-lvar-type val type **zero-typecheck-policy**)))))
                ;; Propagate declared types of MV-BIND variables.
                (propagate-to-args use fun)
                (reoptimize-call use))
            t)))
#+sbcl
(compile 'convert-mv-bind-to-let)
#+sbcl
(in-package :cl-user)


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
(when (position "ironclad" (uiop:command-line-arguments) :test #'string=)
    (pushnew :ironclad *features*))
(when (position "slynk" (uiop:command-line-arguments) :test #'string=)
    (pushnew :slynk *features*))
(when (position "swank" (uiop:command-line-arguments) :test #'string=)
    (pushnew :swank *features*))
(when (position "ft" (uiop:command-line-arguments) :test #'string=)
    (pushnew :mcclim-ffi-freetype *features*))
(when (position "texi" (uiop:command-line-arguments) :test #'string=)
    (pushnew :yadfa/docs *features*))
(when (position "mods" (uiop:command-line-arguments) :test #'string=)
    (pushnew :yadfa/mods *features*))
(ql:quickload :yadfa)
(asdf:make :yadfa :force (when (position "force" (uiop:command-line-arguments) :test #'string=) t))
