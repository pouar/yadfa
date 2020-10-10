;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(sc:define-hook-type move-command (function (symbol list) (values &rest t)))
(sc:define-hook 'hook-move-command 'yadfa-world:move)
