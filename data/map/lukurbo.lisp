;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 lukurbo)
  :name "Lukubro Street"
  :description "You see many diapered furries and diapered fursuiters"
  :enter-text "You're wondering around the street"
  :events '(yadfa-events:enter-lukurbo-1 yadfa-events:secret-underground-pipe-lukurbo)
  :warp-points (list 'rpgmaker-dungeon '(9 5 0 rpgmaker-dungeon)))
