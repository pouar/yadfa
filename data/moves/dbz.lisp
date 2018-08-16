(in-package :yadfa/moves)
(defclass kamehameha (stat/move) ()
    (:default-initargs
        :name "Kamehameha Wave"
        :description "Say `Kamehameha!!!' and fire a huge burst of energy"
        :energy-cost 12
        :attack '(lambda (target user self)
                     (format t "~a used ~a~%" (name-of user) (name-of self))
                     (format t
                         "KAM-E-HAM-E-HA!!! *loud energy blast noise*~%Ok, not as dramatic in a text based game~%")
                     (decf (health-of target) (calculate-damage target user 200))
                     (decf (energy-of user) (energy-cost-of self)))))
