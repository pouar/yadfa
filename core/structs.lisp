;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defstruct event
  "An event in the game"
  (id nil
   :type symbol)
  (lambda '(lambda (self)
            (declare (ignore self)) nil)
    :type (or list symbol function))
  (predicate '(lambda (self)
               (declare (ignore self)) t)
   :type (or list function))
  (repeatable nil :type boolean)
  (major nil :type boolean)
  (major-depends nil :type symbol)
  (optional nil)
  (finished-depends '() :type list)
  (attributes ()))
(defstruct action
  "An action for a prop or item"
  (documentation nil :type (or null simple-string))
  (attributes ())
  (lambda '(lambda (prop))
    :type (or list symbol function)))
(defstruct (yadfa-hook (:constructor make-yadfa-hook%))
  "wrapper for hooks which also contains the docs"
  (documentation nil :type (or null simple-string))
  (value nil :type list)
  (name nil :type symbol))
(defun make-yadfa-hook (name &optional (value nil value-supplied-p) (documentation nil documentation-supplied-p))
  (apply #'make-yadfa-hook% :name name (serapeum:collecting
                                   (when value-supplied-p
                                     (collect :value)
                                     (collect value))
                                   (when documentation-supplied-p
                                     (collect :documentation)
                                     (collect documentation)))))
(define-compiler-macro make-yadfa-hook (name &optional (value nil value-supplied-p) (documentation nil documentation-supplied-p))
  `(make-yadfa-hook% :name ,name
               ,@(when value-supplied-p
                   `(:value ,value))
               ,@(when documentation-supplied-p
                   `(:documentation ,documentation))))
