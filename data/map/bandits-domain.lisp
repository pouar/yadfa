(in-package :yadfa/zones)
(macro-level
    `(progn
         ,@(iter (for i from 0 to 20)
               (collect
                   `(ensure-zone (0 ,i 0 bandits-domain) ()
                        (:default-initargs
                            :name "Bandit's Way"
                            :description "A path filled with bandits"
                            :enter-text "You follow the path"
                            :warp-points ',(when (= i 0) '(ironside (2 0 0 ironside)))
                            :enemy-spawn-list
                            '((:max-random 8
                                  :enemies ((yadfa/enemies:female-diapered-raccoon-bandit .
                                                (list :level (random-from-range 2 5)))))
                                 (:max-random 8
                                     :enemies ((yadfa/enemies:rookie-diapered-raccoon-bandit .
                                                   (list :level (random-from-range 2 5))))))))))))

(macro-level
    `(progn
         ,@(iter (for i from -1 downto -10)
               (collect
                   `(ensure-zone (,i 21 0 bandits-domain) ()
                        (:default-initargs
                            :name "Bandit's Town"
                            :description "A town run by the Raccoon Bandits"
                            :enter-text "You're wander around Bandit's Town"))))))
(ensure-zone (-3 22 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Shop"
        :description "A local shop"
        :enter-text "You enter the Bandit's Shop"
        :diapers-only t
        :no-puddles t
        :props (list
                   :shop (make-instance 'shop
                             :items-for-sale '((yadfa/items:gold-pacifier)
                                                  (yadfa/items:recovering-pacifier)
                                                  (yadfa/items:healing-pacifier)
                                                  (yadfa/items:bandit-swimsuit)
                                                  (yadfa/items:bandit-uniform-tunic)
                                                  (yadfa/items:bandit-uniform-shirt)
                                                  (yadfa/items:bandit-uniform-sports-bikini-top)
                                                  (yadfa/items:monster-energy-drink)
                                                  (yadfa/items:spiked-bottle-of-milk)
                                                  (yadfa/items:bandit-diaper)
                                                  (yadfa/items:bandit-adjustable-diaper)
                                                  (yadfa/items:bandit-female-diaper)
                                                  (yadfa/items:bandit-swim-diaper-cover)
                                                  (yadfa/items:lower-bandit-swim-diaper-cover)
                                                  (yadfa/items:female-bandit-swim-diaper-cover)
                                                  (yadfa/items:gold-collar)
                                                  (yadfa/items:ak47)
                                                  (yadfa/items:pink-sword)
                                                  (yadfa/items:toddler-dress)))
                   :changing-table (make-instance 'automatic-changing-table)
                   :bed (make-instance 'bed)
                   :checkpoint (make-instance 'checkpoint))
        :events '(yadfa/events:enter-bandits-shop-1 yadfa/events:obtain-diaper-lock-1)))
(setf
    (getf (actions-of (getf (props-of (get-zone '(-3 22 0 bandits-domain))) :shop)) :ask-for-bathroom)
    (make-action
        :documentation "Ask the raccoons if you can use the bathroom."
        :lambda '(lambda
                     (prop &rest keys &key &allow-other-keys)
                     (declare (ignore prop))
                     (format t "Diapered Raccon Bandit Shop Owner: Sorry, only I'm allowed in there. Everyone else can just use their diapers. Isn't that right mushbutt?~%~%*The Shop Owner slaps the back of the Rookie's diaper*~%~%*Rookie yelps then grabs the back of his diaper and struggles to unmush it*~%~%*The Shop Owner laughs*~%~%Rookie Raccoon: Can I please get a diaper change now?~%~%Shop Owner: Keep asking me that and I'll make you sit in it in timeout again.~%~%Rookie Raccoon: NO! PLEASE! I'LL BE GOOD!~%~%")))
    (getf (actions-of (getf (props-of (get-zone '(-3 22 0 bandits-domain))) :shop)) :ask-why-youre-allowed-to-shop)
    (make-action
        :documentation "Ask the raccoons why you're allowed to shop here without the gang attacking you"
        :lambda '(lambda
                     (prop &rest keys &key &allow-other-keys)
                     (declare (ignore prop))
                     (format t "~a: You know how the gang seems to attack me everywhere I show up?~%~%"
                         (name-of (player-of *game*)))
                     (format t "Shop Owner: Yeah?~%~%")
                     (format t "~a: Well how come they're letting me shop here without attacking me?~%~%"
                         (name-of (player-of *game*)))
                     (format t "Shop Owner: Because money stupid."))))
(ensure-zone (-3 23 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Shop Bathroom"
        :description "CLOSED FOREVER!!!!! MUAHAHAHAHA!!!!"
        :locked '(or nil)))
(ensure-zone (-5 22 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Kennel"
        :description "A grungey looking kennel where the Raccoon Bandits keep their `pets'. Negleted so much that they literally forgot about their existence"
        :enter-text "You enter the kennel"
        :events '(yadfa/events:enter-bandits-kennel-1)))
(ensure-zone (0 21 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Town Entrance"
        :description "The enterance to Bandit Town"
        :enter-text "You're at the enterance of Bandit Town"
        :warp-points '(home (0 1 0 home))
        :events '(yadfa/events:enter-bandits-village-1)))
(macro-level
    `(progn
         ,@(iter (for i from 22 to 30)
               (collect
                   `(ensure-zone (0 ,i 0 bandits-domain) ()
                        (:default-initargs
                            :name "Bandit's Town"
                            :description "A town run by the Raccoon Bandits"
                            :enter-text "You're wander around Bandit's Town"
                            :warp-points ',(when (= i 30) '(silver-cape (0 0 0 silver-cape)))))))))
(ensure-zone (1 21 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Cove Dock"
        :description "The dock of Bandit's Cove"
        :enter-text "You're at a dock"))
(macro-level
    `(progn
         ,@(let ((a))
               (iter (for y from 19 to 23)
                   (setf a
                       (append
                           a
                           (iter
                               (for x from 2 to 6)
                               (collect
                                   `(ensure-zone (,x ,y 0 bandits-domain) ()
                                        (:default-initargs
                                            :name "Bandit's Cove"
                                            :description "A cove filled with bandits"
                                            :enter-text "You're at a cove runned by bandits"
                                            :enemy-spawn-list
                                            '((:max-random 10
                                                  :enemies
                                                  ((yadfa/enemies:rookie-diapered-raccoon-bandit .
                                                       (list
                                                           :level (random-from-range 2 5)
                                                           :wear (list
                                                                     (make-instance
                                                                         'yadfa/items:lower-bandit-swim-diaper-cover)
                                                                     (make-instance
                                                                         'yadfa/items:bandit-diaper
                                                                         :sogginess (strong-random 1000)
                                                                         :messiness (strong-random 6000)))
                                                           :level (random-from-range 2 5)))))
                                                 (:max-random 10
                                                     :enemies
                                                     ((yadfa/enemies:diapered-raccoon-bandit .
                                                          (list
                                                              :level (random-from-range 2 5)
                                                              :wear (list
                                                                        (make-instance
                                                                            'yadfa/items:bandit-swimsuit/closed)
                                                                        (make-instance
                                                                            'bandit-swim-diaper-cover)
                                                                        (make-instance
                                                                            'yadfa/items:bandit-diaper))
                                                              :level (random-from-range 2 5)))))
                                                 (:max-random 10
                                                     :enemies
                                                     ((yadfa/enemies:female-diapered-raccoon-bandit .
                                                          (list
                                                              :level (random-from-range 2 5)
                                                              :wear (list
                                                                        (make-instance
                                                                            'yadfa/items:bandit-uniform-sports-bikini-top)
                                                                        (make-instance
                                                                            'yadfa/items:female-bandit-swim-diaper-cover)
                                                                        (make-instance
                                                                            'yadfa/items:bandit-female-diaper
                                                                            :sogginess (strong-random 1000)
                                                                            :messiness (strong-random 6000)))
                                                              :level (random-from-range 2 5)))))))))))))
               a)))
(ensure-zone (6 24 0 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Cave Entrance"
        :description "A mysterious cave"
        :enter-text "You Enter the cave"))
(ensure-zone (6 24 -2 bandits-domain) ()
    (:default-initargs
        :name "Bandit's Cave"
        :description "A mysterious cave"
        :enter-text "You Enter the cave"
        :warp-points '(cave-entrance (6 24 0 bandits-domain)
                          descend (6 24 2 bandits-domain))
        :events '(yadfa/events:decend-bandits-cave-1)))
