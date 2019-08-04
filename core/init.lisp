;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defvar *battle* nil)
(defvar *mod-registry* nil)
(defvar *pattern-cache* (make-hash-table :test 'equal))
(defvar *records* ())
#+yadfa-mods
(defvar *mods* '())
