;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 haunted-forest)
  :name "Haunted Forest Entrance"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You enter the haunted forest"
  :events '(yadfa-events:secret-underground-pipe-haunted-forest))
