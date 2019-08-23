;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro ref (symbol type)
    #+yadfa-docs
    `(format nil "@ref{~a,@code{~a} in @code{~a},@code{~a} in @code{~a}}"
             (net.didierverna.declt::escape-anchor
              (net.didierverna.declt::anchor-name
               (,(intern (string-upcase (format nil "make-~a-definition" type)) :net.didierverna.declt) :symbol ',symbol)))
             (net.didierverna.declt::escape ',symbol)
             (net.didierverna.declt::escape (package-name (symbol-package ',symbol)))
             (net.didierverna.declt::escape ',symbol)
             (net.didierverna.declt::escape (package-name (symbol-package ',symbol))))
    #-yadfa-docs `(let ((*package* (find-package :cl)))
                    (format nil "~s" ',symbol)))
  (defmacro xref (symbol type)
    #+yadfa-docs
    `(format nil "@xref{~a,@code{~a} in @code{~a},@code{~a} in @code{~a}}"
             (net.didierverna.declt::escape-anchor
              (net.didierverna.declt::anchor-name
               (,(intern (string-upcase (format nil "make-~a-definition" type)) :net.didierverna.declt) :symbol ',symbol)))
             (net.didierverna.declt::escape ',symbol)
             (net.didierverna.declt::escape (package-name (symbol-package ',symbol)))
             (net.didierverna.declt::escape ',symbol)
             (net.didierverna.declt::escape (package-name (symbol-package ',symbol))))
    #-yadfa-docs `(let ((*package* (find-package :cl)))
                    (format nil "~s" ',symbol))))
(defvar *battle* nil)
(defvar *mod-registry* nil)
(defvar *pattern-cache* (make-hash-table :test 'equal))
(defvar *records* ())
#+yadfa-mods
(defvar *mods* '())
