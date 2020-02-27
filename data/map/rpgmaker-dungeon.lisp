;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(defevent create-rpgmaker-dungeon
  :lambda (lambda (self)
            (declare (ignore self))
            (let ((width 10) (height 10))
              (declare (type fixnum width height))
              (labels ((neighbors (x y width height)
                         (declare (type fixnum x y width height))
                         (remove-if
                          (lambda (x-y)
                            (declare (type list x-y))
                            (not (and (< -1 (first x-y) width)
                                      (< -1 (second x-y) height))))
                          `((,x ,(1+ y) :south) (,(1- x) ,y :west) (,x ,(1- y) :north) (,(1+ x) ,y :east))))
                       (remove-wall (width height &optional visited)
                         (labels ((walk (x y width height)
                                    (push (list x y) visited)
                                    (iter (for (u v w) in (alexandria:shuffle (neighbors x y width height)))
                                      (unless (member (list u v) visited :test #'equal)
                                        (setf (getf-direction `(,x ,y 0 rpgmaker-dungeon) w :hidden) nil
                                              (getf-direction `(,u ,v 0 rpgmaker-dungeon)
                                                              (getf '(:south :north
                                                                      :north :south
                                                                      :east :west
                                                                      :west :east) w) :hidden) nil)
                                        (walk u v width height)))))
                           (walk (random width) (random height) width height))))
                (iter (for x from 0 to (1- width))
                  (iter (for y from 0 to (1- height))
                    (defzone* `(,x ,y 0 rpgmaker-dungeon)
                      :name "Generic Cookie Cutter Dungeon"
                      :description "Time for an \"adventure\""
                      :direction-attributes (list :north (list :hidden t)
                                                  :south (list :hidden t)
                                                  :east (list :hidden t)
                                                  :west (list :hidden t))
                      :enemy-spawn-list 'rpgmaker-dungeon)))
                (remove-wall width height)
                (setf (getf (warp-points-of (get-zone '(5 9 0 rpgmaker-dungeon))) 'bandits-domain)
                      '(0 31 0 bandits-domain)
                      (getf (warp-points-of (get-zone '(9 5 0 rpgmaker-dungeon))) 'lukurbo)
                      '(0 0 0 lukurbo)
                      (getf (warp-points-of (get-zone '(0 5 0 rpgmaker-dungeon))) 'silver-cape)
                      '(0 0 0 silver-cape)
                      (getf (warp-points-of (get-zone '(5 0 0 rpgmaker-dungeon))) 'haunted-forest)
                      '(0 0 0 haunted-forest))))))
