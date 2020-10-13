;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa-clim)
(defclass stat-view (c:view) ())
(defconstant +stat-view+ (make-instance 'stat-view))
(in-package :yadfa)
(s:eval-always
  (defmacro ref (symbol type)
    (if (asdf:component-loaded-p "yadfa/docs")
        (let* ((package-name (uiop:symbol-call '#:net.didierverna.declt '#:escape (package-name (symbol-package symbol))))
               (symbol-name (uiop:symbol-call '#:net.didierverna.declt '#:escape symbol))
               (external (if (eq (nth-value 1 symbol) :external)
                             2 1)))
          (f:fmt nil "@ref{"
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape-anchor
                                   (uiop:symbol-call '#:net.didierverna.declt '#:anchor-name
                                                     (uiop:symbol-call '#:net.didierverna.declt
                                                                       (make-symbol (string-upcase (format nil "make-~a-definition" type)))
                                                                       :symbol symbol)))
                 ",@code{" package-name (:times "∶" external) symbol-name "},@code{" package-name (:times ":" external) symbol-name "}}"))
        (let ((*package* (find-package :cl)))
          (format nil "~s" symbol))))
  (defmacro xref (symbol type)
    (if (asdf:component-loaded-p "yadfa/docs")
        (let* ((package-name (uiop:symbol-call '#:net.didierverna.declt '#:escape (package-name (symbol-package symbol))))
               (symbol-name (uiop:symbol-call '#:net.didierverna.declt '#:escape symbol))
               (external (if (eq (nth-value 1 symbol) :external)
                             2 1)))
          (f:fmt nil "@xref{"
                 (uiop:symbol-call '#:net.didierverna.declt '#:escape-anchor
                                   (uiop:symbol-call '#:net.didierverna.declt '#:anchor-name
                                                     (uiop:symbol-call '#:net.didierverna.declt
                                                                       (make-symbol (string-upcase (format nil "make-~a-definition" type)))
                                                                       :symbol symbol)))
                 ",@code{" package-name (:times "∶" external) symbol-name "},@code{" package-name (:times ":" external) symbol-name "}}"))
        (let ((*package* (find-package :cl)))
          (format nil "See ~s" symbol)))))
(g:define-global-var *events* (make-hash-table :test #'equal :size 100))
(g:define-global-var yadfa-clim::*records* ())
(g:define-global-var *battle* nil)
(g:define-global-var *mod-registry* (make-hash-table :test #'equal))
(g:define-global-var *pattern-cache* (make-hash-table :test #'equal))
(g:define-global-var *mods* '())
(g:define-global-var *cheat-hooks* '()
  "Put functions that sets crap to fixed values here. Intended to be used similar to the feature in Cheat Engine used to set variables to a constant value, although will probably run less frequently and is generally easier to use since it doesn't involve working with memory addresses")
(g:define-global-var *game* nil
  "contains the information to be serialized when saving and loading a game")
(g:define-global-var *battle-packages* '(:yadfa-battle :yadfa-battle-commands)
  "contains the package designators for the exported symbols to be used as commands in battle")
(g:define-global-var *world-packages* '(:yadfa-world :yadfa-world-commands)
  "contains the package designators for the exported symbols to be used as commands outside of battle")
(g:define-global-var *command-packages* '(:yadfa-bin)
  "contains the package designators for the exported symbols to be used as commands")
(g:define-global-var *last-rng-update* 0)
(g:define-global-var *immutable* nil)
(g:define-global-var *element-types* (make-hash-table :test 'eq)
  "Hash table containing preinitialized element types. Used instead of defining a bunch of constants.")
