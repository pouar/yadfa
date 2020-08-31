;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 secret-underground)
             :name "Secret Underground"
             :description "You see several warp pipes in here going to various places"
             :enter-text "You're wandering around in the secret underground"
             :warp-points (list 'home '(0 1 0 home)
                                'ironside '(2 0 0 ironside)
                                'bandits-domain '(-3 21 0 bandits-domain)))
(ensure-zone (0 1 0 secret-underground)
             :name "Secret Underground Path"
             :description "A path"
             :enter-text "You're wandering around in the secret underground")
(ensure-zone (-1 1 0 secret-underground)
             :name "Secret Underground Base"
             :description "A place where you can rest"
             :enter-text "You're wandering around in the secret underground"
             :placable t
             :props (list :changing-table (make-instance 'yadfa-props:automatic-changing-table)
                          :chest (make-instance 'prop
                                                :name "Dresser"
                                                :placeable t
                                                :description "You can store your items here")
                          :checkpoint (make-instance 'yadfa-props:checkpoint)
                          :diaper-dispenser (make-instance 'prop
                                                           :name "Diaper/Pullup Dispenser"
                                                           :description "Provides an infinite supply of diapers and pullups"
                                                           :actions
                                                           (list :get-diaper (make-action
                                                                              :documentation "Get a diaper from the dispenser, pass :diaper to OUT-ITEM to get a diaper or :pullups to get pullups"
                                                                              :lambda '(lambda
                                                                                        (prop &rest keys &key (out-item :diaper) &allow-other-keys)
                                                                                        (declare (type prop prop)
                                                                                         (type (or (eql :diaper) (eql :pullup)) out-item)
                                                                                         (ignore prop))
                                                                                        (check-type prop prop)
                                                                                        (check-type out-item '(or (eql :diaper) (eql :pullup)))
                                                                                        (push (make-instance
                                                                                               (cond ((eq out-item :diaper) 'yadfa-items:diaper)
                                                                                                     ((eq out-item :pullups)  'yadfa-items:pullups)))
                                                                                         (inventory-of (player-of *game*)))))))))
(ensure-zone (1 1 0 secret-underground)
             :name "Secret Underground Shop"
             :description "A shop where you can buy stuff. Be sure to buy a training potty for your base. You don't want to have an accident now do you?"
             :enter-text "You enter the shop"
             :props (list :shop (make-instance 'yadfa-props:shop
                                               :items-for-sale '((yadfa-props:pet-bed)
                                                                 (yadfa-props:training-potty)))))
