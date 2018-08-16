(in-package :yadfa/items)
(defclass bottle-of-milk (consumable) ()
    (:default-initargs
        :name "Bottle of milk"
        :description "A baby bottle filled with milk. Fills up your health and your bladder."
        :value 50
        :consumable t
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (incf (bladder/contents-of user) 100)
                         (incf (health-of user) 20))))
(defclass monster-energy-drink (consumable) ()
    (:default-initargs
        :name "Monster Energy Drink"
        :description "WARNING! NOT MEANT FOR HUMAN (or furry) CONSUMPTION. Fills up your energy and your bladder."
        :value 100
        :consumable t
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (incf (bladder/contents-of user) 175)
                         (incf (energy-of user) 20))))
(defclass spiked-bottle-of-milk (consumable) ()
    (:default-initargs
        :name "Spiked Bottle of milk"
        :description "A baby bottle filled with laxatives and diuretics. Fills up your bladder and bowels really quickly."
        :value 50
        :consumable t
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (setf (bladder/contents-of user)
                             (if (< (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                                 (-
                                     (bladder/maximum-limit-of user)
                                     (* (bladder/fill-rate-of user) 5))
                                 (+
                                     (bladder/contents-of user)
                                     (bladder/potty-dance-limit-of user))))
                         (setf (bowels/contents-of user)
                             (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                                 (-
                                     (bowels/maximum-limit-of user)
                                     (* (bowels/fill-rate-of user) 5))
                                 (+
                                     (bowels/contents-of user)
                                     (bowels/potty-dance-limit-of user)))))))
(defclass potion (consumable) ()
    (:default-initargs
        :name "Potion"
        :description "Heals 20 HP"
        :value 50
        :consumable t
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (incf (health-of user) 20))))
(defclass maximum-tomato (consumable) ()
    (:default-initargs
        :name "Maximum Tomato"
        :description "Restores Full HP"
        :value 50
        :consumable t
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (setf (health-of user) (calculate-stat user :health)))))
