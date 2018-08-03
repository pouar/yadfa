(in-package :yadfa/zones)
(defzone (0 0 0 home) ()
    (:default-initargs
        :name "Bedroom"
        :description "Your house only has a bedroom and a bathroom. Because Pouar was too lazy to code you a real house."
        :enter-text "You enter your bedroom."
        :props (list
                   :bed (make-instance 'bed
                            :name "Your bed"
                            :description "Pouar wasn't sure what design to put on the sheets, so he decided to leave that up to the player's interpretation.")
                   :dresser (make-instance 'prop
                                :name "Dresser"
                                :description "Has all your clothes and diapers in here, until you take them out.")
                   :checkpoint (make-instance 'checkpoint))))
(defzone (1 0 0 home) ()
    (:default-initargs
        :name "Bathroom"
        :description "Your bathroom"
        :enter-text "You enter the bathroom"
        :props (list
                   :toilet (make-instance 'toilet
                               :name "Toilet"
                               :description "You can use this so you don't wet or mess yourself")
                   :cupboard (make-instance 'prop
                                 :name "Cupboard"
                                 :description "A cupboard located over the sink")
                   :washer (make-instance 'washer
                                 :name "Washer"
                                 :description "A place to wash all the clothes that you've ruined"))))
(defzone (0 1 0 home) ()
    (:default-initargs
        :name "Street"
        :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
        :enter-text "You enter the street outside your house"
        :warp-points '(downtown (0 0 0 downtown))))
(defzone (0 2 0 home) ()
    (:default-initargs
        :name "Pool area"
        :description "A pool to go swimming in"
        :enter-text "You enter the pool area"))
(defzone (0 2 -1 home) ()
    (:default-initargs
        :name "Pool"
        :description "A pool"
        :enter-text "You dive in the pool"
        :underwater t))