(in-package :yadfa/items)
(defclass ak47 (item) ()
    (:default-initargs
        :name "AK-47"
        :value 6000
        :description "Hail a bunch of bullets at these motherfuckers"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 100
                          :attack '(lambda (target user self)
                                       (let ((a (calculate-damage target user (power-of self))))
                                           (format t "~a fires ~a ~a at ~a~%"
                                               (name-of user)
                                               (if (malep user) "his" "her")
                                               (name-of (wield-of user))
                                               (name-of target))
                                           (decf (health-of target) a)
                                           (format t "~a received ~a damage~%" (name-of target) a))))))
(defclass pink-sword (item) ()
    (:default-initargs
        :name "Pink Sword"
        :value 3000
        :description "'Cause it looks cute on the character"
        :default-move (make-instance 'yadfa/moves:weapon-default
                          :power 80)))
