;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-allies"; coding: utf-8-unix; -*-
(in-package :yadfa-allies)
(defunassert (yadfa-world-commands:disown-adopted-enemies (&optional allies))
    (enemies (or list type-specifier))
  (setf allies
        (typecase allies
          (null (accept-with-frame-resolved
                  (clim:accepting-values (*query-io*  :resynchronize-every-pass t)
                    (clim:accept `(clim:subset-alist ,(iter (for enemy in (allies-of *game*))
                                                        (when (typep (class-of enemy) 'adopted-enemy)
                                                          (collect (cons (name-of enemy) enemy)))))
                                 :prompt "Enemies to disown"
                                 :stream *query-io*
                                 :view clim:+check-box-view+))))
          (type-specifier (iter (for enemy in (allies-of *game*))
                            (when (typep enemy `(and ,allies adopted-enemy))
                              (collect enemy))))
          (list (iter
                  (for enemy in (allies-of *game*))
                  (generate current in allies)
                  (for index upfrom 0)
                  (cond ((typep current '(not unsigned-byte))
                         (error "ENEMIES must be a list of unsigned-bytes"))
                        ((and (eql index current) (typep enemy '(not adopted-enemy)))
                         (let ((*package* (find-package :yadfa-user)))
                           (error "ALLY at index ~d isn't an ~w" yadfa-allies::index 'yadfa-allies:adopted-enemy)))
                        ((eql index current)
                         (collect enemy)
                         (next current)))))
          (t (error "ALLIES must either be a list of unsigned-bytes or a type specifier"))))
  (removef (allies-of *game*) allies :test (lambda (o e)
                                             (member e o)))
  (removef (team-of *game*) allies :test (lambda (o e)
                                             (member e o))))
