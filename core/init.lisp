;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(eval-always
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
		   gghjghj
(define-global-var yadfa-clim::*records* ())
(define-global-var *battle* nil)
(define-global-var *mod-registry* (make-hash-table :test #'equal))
(define-global-var *pattern-cache* (make-hash-table :test #'equal))
(define-global-var *mods* '())
(define-global-var *read-frame-command-hooks* '()
  "Hooks that run after @code{READ-FRAME-COMMAND} is called in @code{DEFAULT-FRAME-TOP-LEVEL} for @code{CLIM-LISTENER::LISTENER}")
(define-global-var *execute-frame-command-hooks* '()
  "Hooks that run after @code{EXECUTE-FRAME-COMMAND} is called in @code{DEFAULT-FRAME-TOP-LEVEL} for @code{CLIM-LISTENER::LISTENER}")
(define-global-var *redisplay-frame-panes-hooks* '()
  "Hooks that run after @code{REDISPLAY-FRAME-PANES} is called in @code{DEFAULT-FRAME-TOP-LEVEL} for @code{CLIM-LISTENER::LISTENER}")
(define-global-var *cheat-hooks* '()
  "Put functions that sets crap to fixed values here. Intended to be used similar to the feature in Cheat Engine used to set variables to a constant value, although will probably run less frequently and is generally easier to use since it doesn't involve working with memory addresses")
(define-global-var *move-hooks* '()
  "Hooks that run each time the player moves to another zone")
(define-global-var *process-potty-hooks* '()
  "Hooks that run each time @code{YADFA::PROCESS-POTTY} is run. Called with @var{USER} a cons containing the return values of @code{YADFA:WET} and @code{YADFA:MESS} as aruments")
(define-global-var *game* nil
  "contains the information to be serialized when saving and loading a game")
