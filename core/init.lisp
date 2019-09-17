;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
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
(defmacro defglobal (name value &optional doc)
  #+sbcl `(sb-ext:defglobal ,name ,value ,doc)
  #+ccl `(ccl:defstatic ,name ,value ,doc)
  #-(or sbcl ccl) `(defvar ,name ,value ,doc))
(defglobal yadfa-clim::*records* ())
(defglobal *battle* nil)
(defglobal *mod-registry* (make-hash-table :test #'equal))
(defglobal *pattern-cache* (make-hash-table :test #'equal))
(defglobal *mods* '())
(defglobal *game* nil
  "contains the information to be serialized when saving and loading a game")
