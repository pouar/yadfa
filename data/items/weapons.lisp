;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass 7.62×39mm (ammo) ()
  (:default-initargs
   :name "7.62x39mm Rounds"
   :plural-name "7.62x39mm Rounds"
   :value 400
   :description "7.62x39mm bullets"
   :ammo-power 150))
(defclass box-of-7.62×39mm (item) ()
  (:default-initargs
   :name "Box of 7.62x39mm Rounds"
   :plural-name "Boxes of 7.62x39mm Rounds"
   :description "Contains 60 rounds of ammunition"
   :consumable t
   :value 2000
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (format t "You open the box and dump all the ammunition out of it.~%")
                 (iter (for i from 1 to 6)
                   (push (make-instance '7.62×39mm) (inventory-of user))))))
(defclass ak47 (weapon) ()
  (:default-initargs
   :name "AK-47"
   :value 6000
   :description "Hail a bunch of bullets at these motherfuckers"
   :ammo-type '7.62×39mm
   :ammo-capacity 3
   :power 40
   :attack-script '(lambda (target user self)
                    (let ((a (calculate-damage target user (if (first (ammo-of self))
                                                               (ammo-power-of (first (ammo-of self)))
                                                               (power-of self)))))
                      (apply #'format t
                             (if (first (ammo-of self))
                                 (list "~a fires ~a ~a at ~a~%"
                                       (name-of user)
                                       (if (malep user) "his" "her")
                                       (name-of self)
                                       (name-of target))
                                 (list "~a whacks ~a ~a at ~a~%"
                                       (name-of user)
                                       (if (malep user) "his" "her")
                                       (name-of self)
                                       (name-of target))))
                      (decf (health-of target) a)
                      (format t "~a received ~a damage~%" (name-of target) a)))))
(defclass exterminator-ammo (ammo) ()
  (:default-initargs
   :name "Exterminator Ammo"
   :plural-name "Exterminator Ammo"
   :value 100000
   :description "Ammo for the Exterminator"
   :ammo-power 500))
(defclass exterminator (weapon) ()
  (:default-initargs
   :name "Exterminator"
   :value 1000000
   :description "IT'S DA MOST POWEFUW WEAPON!!!! Too bad it takes forever to fire and reload, can only hold one round at a time, and ammo is super expensive, making it practically useless in battle."
   :wield-stats (list :speed -100)
   :ammo-type 'exterminator-ammo
   :ammo-capacity 1
   :power 0
   :attack-script '(lambda (target user self)
                    (let ((a (calculate-damage target user (if (first (ammo-of self))
                                                               (ammo-power-of (first (ammo-of self)))
                                                               (power-of self)))))
                      (apply #'format t
                             (if (first (ammo-of self))
                                 (list "*BOOOMKSSSHHH!!!!!*~%")
                                 (list "~a struggles to whack ~a with ~a ~a but barely taps ~a~%"
                                       (name-of user)
                                       (name-of target)
                                       (if (malep user) "his" "her")
                                       (name-of self)
                                       (if (malep target) "his" "her"))))
                      (decf (health-of target) a)
                      (format t "~a received ~a damage~%" (name-of target) a)))))
(defclass pink-sword (weapon) ()
  (:default-initargs
   :name "Pink Sword"
   :value 3000
   :power 80
   :description "'Cause it looks cute on the character"))
(defclass egg-spear (weapon) ()
  (:default-initargs
   :name "L̶a̶n̶c̶e̶ \"Egg Spear\""
   :value 1000
   :power 40
   :description "Used by egg pawns, for some reason, it does the exact same amount of damage as an egg pawn without one"))
(defclass hammer-gun (weapon) ()
  (:default-initargs
   :name "Hammer Gun"
   :value 1000
   :power 20
   :description "As seen in One Piece"))
(defclass wrench (weapon) ()
  (:default-initargs
   :name "Wrench"
   :value 500
   :power 1
   :description "You get to frag enemies using this beauty, won't that be fun?"))
(defclass three-swords (weapon) ()
  (:default-initargs
   :name "Three Swords"
   :value 10000
   :power 150
   :description "You're just like Roronoa Zoro with these"
   :attack-script '(lambda (target user self)
                    (let ((a (calculate-damage target user (if (first (ammo-of self))
                                                               (ammo-power-of (first (ammo-of self)))
                                                               (power-of self)))))
                      (format t "Three Sword Style!!!!~%")
                      (decf (health-of target) a)
                      (format t "~a received ~a damage~%" (name-of target) a)))))
