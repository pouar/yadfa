;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 home)
  :name "Bedroom"
  :description "Your house only has a bedroom and a bathroom. Because Pouar was too lazy to code you a real house."
  :enter-text "You enter your bedroom."
  :props (list :bed (make-instance 'yadfa-props:bed
                                   :name "Your bed"
                                   :description "Pouar wasn't sure what design to put on the sheets, so he decided to leave that up to the player's interpretation.")
               :dresser (make-instance 'prop
                                       :name "Dresser"
                                       :placeable t
                                       :description "Has all your clothes and diapers in here, until you take them out."
                                       :items ())
               :checkpoint (make-instance 'yadfa-props:checkpoint)))
(ensure-zone (1 0 0 home)
  :name "Bathroom"
  :description "Your bathroom"
  :enter-text "You enter the bathroom"
  :props (list :toilet (make-instance 'yadfa-props:toilet
                                      :name "Toilet"
                                      :description "You can use this so you don't wet or mess yourself")
               :cupboard (make-instance 'prop
                                        :name "Cupboard"
                                        :placeable t
                                        :description "A cupboard located on the sink"
                                        :items (list (make-instance 'yadfa-items:potion)))
               :washer (make-instance 'yadfa-props:washer
                                      :name "Washer"
                                      :description "A place to wash all the clothes that you've ruined")))
(ensure-zone (0 1 0 home)
  :name "Street"
  :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
  :enter-text "You enter the street outside your house"
  :warp-points (list 'ironside '(0 0 0 ironside)))
(ensure-zone (0 2 0 home)
  :name "Pool area"
  :description "A pool to go swimming in"
  :enter-text "You enter the pool area"
  :stairs (list :down))
(ensure-zone (0 2 -1 home)
  :name "Pool"
  :description "A pool"
  :enter-text "You dive in the pool"
  :stairs (list :up)
  :underwater t)
