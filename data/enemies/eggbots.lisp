;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass egg-pawn (enemy) ()
  (:default-initargs
   :name "Egg Pawn"
   :description "One of Eggman's robots"
   :species "Egg Pawn"
   :male t
   :attributes (list :not-ticklish t)
   :bitcoins-per-level 40
   :element-types (make-instances yadfa-element-types:steel)))
