;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass ammo-box-mixin (item)
  ((ammo :type symbol
         :initarg :ammo)))
(defmethod use-script ((item ammo-box-mixin) (user base-character) (target base-character))
  (f:fmt t (name-of user) " open the box and dump all the ammunition out of it." #\Newline)
  (iter (for i from 1 to 6)
        (push (make-instance (slot-value item 'ammo)) (inventory-of target))))
(defclass 7.62×39mm (ammo) ()
  (:default-initargs
   :name "7.62x39mm Rounds"
   :plural-name "7.62x39mm Rounds"
   :value 400
   :description "7.62x39mm bullets"
   :ammo-power 150))
(defclass box-of-7.62×39mm (ammo-box-mixin) ()
  (:default-initargs
   :name "Box of 7.62x39mm Rounds"
   :plural-name "Boxes of 7.62x39mm Rounds"
   :description "Contains 60 rounds of ammunition"
   :consumable t
   :value 2000
   :ammo '7.62×39mm))
(defclass ak47 (weapon) ()
  (:default-initargs
   :name "AK-47"
   :value 6000
   :description "Hail a bunch of bullets at these motherfuckers"
   :ammo-type '7.62×39mm
   :ammo-capacity 3
   :power 40))
(defmethod attack ((target base-character) (user base-character) (self ak47))
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
    (format t "~a received ~a damage~%" (name-of target) a)))
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
   :power 0))
(defmethod attack ((target base-character) (user base-character) (self exterminator))
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
    (format t "~a received ~a damage~%" (name-of target) a)))
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
   :description "You're just like Roronoa Zoro with these"))
(defmethod attack ((target base-character) (user base-character) (self three-swords))
  (let ((a (calculate-damage target user (if (first (ammo-of self))
                                             (ammo-power-of (first (ammo-of self)))
                                             (power-of self)))))
    (format t "Three Sword Style!!!!~%")
    (decf (health-of target) a)
    (format t "~a received ~a damage~%" (name-of target) a)))
(defclass messing-laser (weapon) ()
  (:default-initargs
   :name "Messing Laser"
   :description "Causes enemies to mess themselves"
   :values 8000))
(defmethod attack ((target base-character) (user base-character) (weapon messing-laser))
  (f:fmt t (name-of user) " fires " (if (malep user) "his" "her") " laser at " (name-of target) #\Newline)
  (use-script weapon user target))
(defmethod use-script ((weapon messing-laser) (user base-character) (target base-character))
  (f:fmt t "It has no effect on " (name-of target) #\Newline))
(defmethod use-script ((weapon messing-laser) (user base-character) (target bowels-character))
  (f:fmt t (name-of target) " squats down and starts blorting " (if (malep target) "himself" "herself") " uncontrollably." #\Newline)
  (mess :force-fill-amount (bowels/maximum-limit-of target))
  (set-status-condition 'yadfa-status-conditions:messing target))
