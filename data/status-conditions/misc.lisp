;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-status-conditions"; coding: utf-8-unix; -*-
(in-package :yadfa-status-conditions)
(defclass skunked (status-condition)
  ()
  (:default-initargs
   :name "Skunked"
   :description "User's defenses are reduced due to the smell"
   :duration 5
   :accumulative 3
   :stat-delta '(:defense -5)))
