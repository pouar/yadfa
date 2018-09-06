(in-package :yadfa/zones)
(defzone (0 0 0 secret-underground) ()
    (:default-initargs
        :name "Secret Underground"
        :description "You see several warp pipes in here going to various places"
        :enter-text "You're wandering around in the secret underground"
        :warp-points '(home (0 1 0 home)
                          ironside (2 0 0 ironside)
                          bandits-domain (-3 21 0 bandits-domain))))
(defzone (0 1 0 secret-underground) ()
    (:default-initargs
        :name "Secret Underground Path"
        :description ""
        :enter-text "You're wandering around in the secret underground"
        :warp-points '(home (0 1 0 home)
                          ironside (0 0 0 ironside)
                          bandits-domain (-3 21 0 bandits-domain))
        :props (list
                   :shop (make-instance 'shop
                             :items-for-sale (let ((a ()))
                                                 (iter
                                                     (for i in (list-all-packages))
                                                     (unless
                                                         (equal i (find-package :yadfa))
                                                         (do-external-symbols
                                                             (s i)
                                                             (when (and
                                                                       (find-class s nil)
                                                                       (subclassp
                                                                           (find-class s)
                                                                           (find-class 'item))
                                                                       (tossablep (make-instance s)))
                                                                 (push (cons s nil) a)))))
                                                 a))
                   :changing-table (make-instance 'automatic-changing-table)
                   :bed (make-instance 'bed)
                   :chest (make-instance 'prop
                                :name "Dresser"
                                :placeable t
                                :description "You can store your items here")
                   :checkpoint (make-instance 'checkpoint)
                   :diaper-dispenser
                   (make-instance 'prop
                       :name "Diaper Dispenser"
                       :description "Provides an infinite supply of diapers"
                       :actions
                       (list :get-diaper
                           (make-action
                               :documentation "Get a diaper from the dispenser"
                               :lambda '(lambda
                                            (prop &rest keys &key &allow-other-keys)
                                            (declare (type prop prop) (ignore prop))
                                            (check-type prop prop)
                                            (push (make-instance 'diaper) (inventory-of (player-of *game*))))))))))
