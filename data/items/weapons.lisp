(in-package :yadfa/items)
(defclass 7.62×39mm (item) ()
    (:default-initargs
        :name "7.62x39mm Rounds"
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
(defclass ak47 (item) ()
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
(defclass pink-sword (item) ()
    (:default-initargs
        :name "Pink Sword"
        :value 3000
        :description "'Cause it looks cute on the character"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 80)))
(defclass hammer-gun (item) ()
    (:default-initargs
        :name "Hammer Gun"
        :value 1000
        :description "As seen in One Piece"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 20)))
(defclass three-swords (item) ()
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
