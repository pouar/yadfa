;; -*- mode: common-lisp; -*-

;;; https://bugs.launchpad.net/sbcl/+bug/1811623
;;; should be fixed in SBCL 1.4.16
#+sbcl
(in-package :sb-c)
#+sbcl
(defun substitute-single-use-lvar (arg var)
    (declare (type lvar arg) (type lambda-var var))
    (binding* ((ref (first (leaf-refs var)))
                  (lvar (node-lvar ref) :exit-if-null)
                  (dest (lvar-dest lvar))
                  (dest-lvar (when (valued-node-p dest) (node-lvar dest))))
        (when (and
                  (or (eq (lvar-uses lvar) ref)
                      )
                  (not (block-delete-p (node-block ref)))
                  (or (not dest-lvar)
                      (not (lvar-dynamic-extent dest-lvar))
                      (lvar-dynamic-extent lvar))
                  (typecase dest
                      (cast
                          (and (type-single-value-p (lvar-derived-type arg))
                              (multiple-value-bind (pdest pprev)
                                  (principal-lvar-end lvar)
                                  (declare (ignore pdest))
                                  (lvar-single-value-p pprev))
                              (or (null dest-lvar)
                                  (atom (lvar-uses dest-lvar)))))
                      (mv-combination
                          (or (eq (basic-combination-fun dest) lvar)
                              (and (eq (basic-combination-kind dest) :local)
                                  (type-single-value-p (lvar-derived-type arg)))))
                      ((or creturn exit)
                          (and (eql (nth-value 1 (values-types (lvar-derived-type arg)))
                                   1)
                              (almost-immediately-used-p lvar (lambda-bind (lambda-var-home var)))
                              (singleton-p (block-pred (node-block dest)))
                              (do-uses (use arg t)
                                  (unless (almost-immediately-used-p arg use)
                                      (return)))))
                      (t
                          (aver (lvar-single-value-p lvar))
                          t))
                  (eq (node-home-lambda ref)
                      (lambda-home (lambda-var-home var))))
            (let ((ref-type (single-value-type (node-derived-type ref))))
                (cond ((csubtypep (single-value-type (lvar-type arg)) ref-type)
                          (substitute-lvar-uses lvar arg
                              t)
                          (delete-lvar-use ref))
                    (t
                        (let* ((value (make-lvar))
                                  (cast (insert-cast-before ref value ref-type
                                            *policy*)))
                            (setf (cast-type-to-check cast) *wild-type*)
                            (substitute-lvar-uses value arg
                                ;; FIXME
                                t)
                            (%delete-lvar-use ref)
                            (add-lvar-use cast lvar)))))
            (setf (node-derived-type ref) *wild-type*)
            (change-ref-leaf ref (find-constant nil))
            (delete-ref ref)
            (unlink-node ref)
            (when (return-p dest)
                (do-uses (use lvar)
                    (when (and (basic-combination-p use)
                              (eq (basic-combination-kind use) :local))
                        (merge-tail-sets use))))
            (reoptimize-lvar lvar)
            t)))
#+sbcl
(defun convert-mv-bind-to-let (call)
  (declare (type mv-combination call))
  (let* ((args (basic-combination-args call))
         (uses (ensure-list (lvar-uses (first args)))))
    (when (and (singleton-p args)
               (singleton-p uses)
               (loop for use in uses
                     always (and (combination-p use)
                                 (eq (lvar-fun-name (combination-fun use))
                                     'values))))
      (with-ir1-environment-from-node call
        (let* ((fun-lvar (mv-combination-fun call))
               (fun (ref-leaf (lvar-uses fun-lvar)))
               (vars (lambda-vars fun))
               (nvars (length vars))
               (new-call (make-combination fun-lvar))
               (new-lvars (loop repeat nvars
                                collect (make-lvar new-call))))
          (setf (functional-kind fun) :let)
          (setf (combination-kind new-call) :local)
          (setf (combination-args new-call) new-lvars)
          (setf (lvar-dest fun-lvar) new-call)
          (insert-node-before call new-call)
          (unlink-node call)
          (loop for use in uses
                for args = (combination-args use)
                for types = (values-type-types (node-derived-type use))
                for lvars = new-lvars
                do
                (loop while (and args lvars)
                      do
                      (let ((arg (pop args))
                            (new-lvar (pop lvars))
                            (type (pop types)))
                        (if (and type
                                 (not (type-asserted-p arg type)))
                            (use-lvar (insert-cast-before use arg type **zero-typecheck-policy**)
                                      new-lvar)
                            (substitute-lvar-uses new-lvar arg nil))))
                (loop for arg in args
                      do (flush-dest arg))
                (when lvars
                  (let ((node-prev (node-prev use)))
                    (setf (node-prev use) nil)
                    (setf (ctran-next node-prev) nil)
                    (loop for lvar in lvars
                          for prev = node-prev then ctran
                          for ctran = (make-ctran)
                          do
                          (reference-constant prev ctran lvar nil)
                          finally
                          (link-node-to-previous-ctran use ctran))))
                (flush-dest (combination-fun use))
                (unlink-node use))
          (propagate-to-args new-call fun)
          (reoptimize-call new-call)))
      t)))
#+sbcl
(compile 'convert-mv-bind-to-let)
#+sbcl
(compile 'substitute-single-use-lvar)
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
