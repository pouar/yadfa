;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defvar yadfa-clim::*records* ())
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro ref (symbol type)
    `(if (asdf:component-loaded-p "yadfa/docs")
         (format nil "@ref{~a,@code{~a} in @code{~a},@code{~a} in @code{~a}}"
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape-anchor
                                   (uiop:symbol-call '#:net.didierverna.declt '#:anchor-name
                                                     (uiop:symbol-call '#:net.didierverna.declt '
                                                                       ,(make-symbol (string-upcase (format nil "make-~a-definition" type)))
                                                                       :symbol ',symbol)))
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ',symbol)
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ,(package-name (symbol-package symbol)))
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ',symbol)
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ,(package-name (symbol-package symbol))))
         (let ((*package* (find-package :cl)))
           (format nil "~s" ',symbol))))
  (defmacro xref (symbol type)
    `(if (asdf:component-loaded-p "yadfa/docs")
         (format nil "@xref{~a,@code{~a} in @code{~a},@code{~a} in @code{~a}}"
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape-anchor
                                   (uiop:symbol-call '#:net.didierverna.declt '#:anchor-name
                                                     (uiop:symbol-call '#:net.didierverna.declt
                                                                       ',(make-symbol (string-upcase (format nil "make-~a-definition" type)))
                                                                       :symbol ',symbol)))
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ',symbol)
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ,(package-name (symbol-package symbol)))
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ',symbol)
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape ,(package-name (symbol-package symbol))))
         (let ((*package* (find-package :cl)))
           (format nil "See ~s" ',symbol)))))
(defvar *battle* nil)
(defvar *mod-registry* nil)
(defvar *pattern-cache* (make-hash-table :test #'equal))
(defvar *mods* '())
(defvar *game* nil
  "contains the information to be serialized when saving and loading a game")
