;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 debug-map)
  :name "zone-0-0-0-debug-map"
  :description "zone-0-0-0-debug-map"
  :enter-text "zone-0-0-0-debug-map"
  :warp-points (list '|1| '(1 1 0 debug-map) '|2| '(0 -1 -1 debug-map))
  :props (list :dresser (make-instance 'prop
                                       :name "Dresser"
                                       :description "A dresser"
                                       :placeable t
                                       :items (append (iter (for i from 1 to 20)
                                                        (collect (make-instance 'yadfa-items:diaper)))
                                                      (iter (for i from 1 to 20)
                                                        (collect (make-instance 'yadfa-items:pullups)))
                                                      (iter (for i from 1 to 5)
                                                        (collect (make-instance 'yadfa-items:thick-rubber-diaper)))
                                                      (list (make-instance 'yadfa-items:sundress)
                                                            (make-instance 'yadfa-items:toddler-dress)
                                                            (make-instance 'yadfa-items:rubber-onesie))))
               :toilet (make-instance 'yadfa-props:toilet)
               :bed (make-instance 'yadfa-props:bed)
               :shop (make-instance 'yadfa-props:debug-shop
                                    :actions (list :talk (make-action :documentation "Say hi"
                                                                      :lambda '(lambda
                                                                                (prop &rest keys &key &allow-other-keys)
                                                                                (declare (ignore prop keys))
                                                                                (format t "Hello World~%")))))))
(ensure-zone (0 0 -1 debug-map)
  :name "zone-0-0--1-debug-map"
  :description "zone-0-0--1-debug-map"
  :enter-text "zone-0-0--1-debug-map"
  :underwater t)
(ensure-zone (0 -1 -1 debug-map)
  :name "zone-0--1--1-debug-map"
  :description "zone-0--1--1-debug-map"
  :enter-text "zone-0--1--1-debug-map"
  :hidden t)
(ensure-zone (0 1 0 debug-map)
  :name "zone-0-1-0-debug-map"
  :description "zone-0-1-0-debug-map"
  :enter-text "zone-0-1-0-debug-map"
  :enemy-spawn-list (list (list :max-random 1 :enemies '((enemy)))))
(ensure-zone (1 0 0 debug-map)
  :name "zone-1-0-0-debug-map"
  :description "zone-1-0-0-debug-map"
  :enter-text "zone-1-0-0-debug-map"    )
(ensure-zone (1 1 0 debug-map)
  :name "zone-1-1-0-debug-map"
  :description "zone-1-1-0-debug-map"
  :enter-text "zone-1-1-0-debug-map"
  :events (list 'yadfa-events:test-battle-1)
  :warp-points '(|1| (0 0 0 debug-map)))
(ensure-zone (1 1 1 debug-map)
  :name "zone-1-1-1-debug-map"
  :description "zone-1-1-1-debug-map"
  :enter-text "zone-1-1-1-debug-map")
