;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent secret-underground-pipe-rpgmaker-dungeon
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:rpgmaker-dungeon)
                  '(0 31 0 yadfa-zones:bandits-domain))))
(defevent secret-underground-pipe-lukurbo
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:lukurbo)
                  '(0 0 0 yadfa-zones:lukurbo))))
(defevent secret-underground-pipe-silver-cape
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:silver-cape)
                  '(0 0 0 yadfa-zones:silver-cape))))
(defevent secret-underground-pipe-haunted-forest
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:haunted-forest)
                  '(0 0 0 yadfa-zones:haunted-forest))))
(defevent secret-underground-pipe-haunted-house
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:haunted-house)
                  '(0 0 0 yadfa-zones:haunted-forest))))
(defevent secret-underground-pipe-candle-carnival
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:candle-carnival)
                  '(0 0 0 yadfa-zones:candle-carnival))))
(defevent secret-underground-pipe-sky-base
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:sky-base)
                  '(0 0 0 yadfa-zones:sky-base))))
(defevent secret-underground-pipe-star-city
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of '(0 0 0 yadfa-zones:secret-underground)) 'yadfa-zones:star-city)
                  '(0 0 0 yadfa-zones:star-city))))
