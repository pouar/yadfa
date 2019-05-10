;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent rpgmaker-dungeon-1
  :lambda '(lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:rpgmaker-dungeon)
             '(0 31 0 yadfa-zones:bandits-domain))))
(defevent rpgmaker-dungeon-2
  :lambda '(lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:lukurbo)
             '(0 0 0 yadfa-zones:lukurbo))))
(defevent rpgmaker-dungeon-3
  :lambda '(lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:silver-cape)
             '(0 0 0 yadfa-zones:silver-cape))))
