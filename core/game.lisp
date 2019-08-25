;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defvar *game* (make-instance 'game)
  "contains the information to be serialized when saving and loading a game")
