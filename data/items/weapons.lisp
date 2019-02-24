(in-package :yadfa/items)
(defclass 7.62×39mm (ammo) ()
    (:default-initargs
        :name "7.62x39mm Rounds"
        :plural-name "7.62x39mm Rounds"
        :value 400
        :description "7.62x39mm bullets"
        :ammo-power-of 150))
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
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 40
                          :attack '(lambda (target user self)
                                       (let ((a (calculate-damage target user
                                                    (if (first (ammo-of (wield-of user)))
                                                        (ammo-power-of (first (ammo-of (wield-of user))))
                                                        (power-of self)))))
                                           (apply #'format t
                                               (if (first (ammo-of (wield-of user)))
                                                   (list "~a fires ~a ~a at ~a~%"
                                                       (name-of user)
                                                       (if (malep user) "his" "her")
                                                       (name-of (wield-of user))
                                                       (name-of target))
                                                   (list "~a whacks ~a ~a at ~a~%"
                                                       (name-of user)
                                                       (if (malep user) "his" "her")
                                                       (name-of (wield-of user))
                                                       (name-of target))))
                                           (decf (health-of target) a)
                                           (format t "~a received ~a damage~%" (name-of target) a))))))
(defclass exterminator-ammo (ammo) ()
    (:default-initargs
        :name "Exterminator Ammo"
        :plural-name "Exterminator Ammo"
        :value 100000
        :description "Ammo for the Exterminator"
        :ammo-power-of 500))
(defclass exterminator (weapon) ()
    (:default-initargs
        :name "Exterminator"
        :value 1000000
        :description "IT'S DA MOST POWEFUW WEAPON!!!! Too bad it takes forever to fire and reload, can only hold one round at a time, and ammo is super expensive, making it practically useless in battle."
        :wield-stats (list :speed -100)
        :ammo-type 'exterminator-ammo
        :ammo-capacity 1
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 0
                          :attack '(lambda (target user self)
                                       (let ((a (calculate-damage target user
                                                    (if (first (ammo-of (wield-of user)))
                                                        (ammo-power-of (first (ammo-of (wield-of user))))
                                                        (power-of self)))))
                                           (apply #'format t
                                               (if (first (ammo-of (wield-of user)))
                                                   (list "*BOOOMKSSSHHH!!!!!*~%")
                                                   (list "~a struggles to whack ~a with ~a ~a but barely taps ~a~%"
                                                       (name-of user)
                                                       (name-of target)
                                                       (if (malep user) "his" "her")
                                                       (name-of (wield-of user))
                                                       (if (malep target) "his" "her"))))
                                           (decf (health-of target) a)
                                           (format t "~a received ~a damage~%" (name-of target) a))))))
(defclass pink-sword (weapon) ()
    (:default-initargs
        :name "Pink Sword"
        :value 3000
        :description "'Cause it looks cute on the character"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 80)))
(defclass hammer-gun (weapon) ()
    (:default-initargs
        :name "Hammer Gun"
        :value 1000
        :description "As seen in One Piece"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 20)))
(defclass three-swords (weapon) ()
    (:default-initargs
        :name "Three Swords"
        :value 10000
        :description "You're just like Roronoa Zoro with these"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 150
                          :attack '(lambda (target user self)
                                       (let ((a (calculate-damage target user (power-of self))))
                                           (format t "Three Sword Style!!!!~%")
                                           (decf (health-of target) a)
                                           (format t "~a received ~a damage~%" (name-of target) a))))))
