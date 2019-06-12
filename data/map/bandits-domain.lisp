(in-package :yadfa-zones)
(macro-level
    `(progn
         ,@(iter (for i from 0 to 20)
               (collect
                   `(ensure-zone (0 ,i 0 bandits-domain)
                        :name "Bandit's Way"
                        :description "A path filled with bandits"
                        :enter-text "You follow the path"
                        :warp-points ,(when (= i 0) '(list 'ironside '(2 0 0 ironside)))
                        :enemy-spawn-list
                        (list '(:max-random 8
                                   :enemies ((yadfa-enemies:female-diapered-raccoon-bandit .
                                                 (list :level (random-from-range 2 5)))))
                            '(:max-random 8
                                 :enemies ((yadfa-enemies:rookie-diapered-raccoon-bandit .
                                               (list :level (random-from-range 2 5)))))))))))

(macro-level
    `(progn
         ,@(iter (for i from -1 downto -10)
               (collect
                   `(ensure-zone (,i 21 0 bandits-domain)
                        :name "Bandit's Town"
                        :description "A town run by the Raccoon Bandits"
                        :enter-text "You're wander around Bandit's Town"
                        ,@(when (= i -3)
                              '(:events (list 'yadfa-events:enter-bandits-shop-2))))))))
(ensure-zone (-3 22 0 bandits-domain)
    :name "Bandit's Shop"
    :description "A local shop"
    :enter-text "You enter the Bandit's Shop"
    :must-wear '(padding
                    . (lambda (user)
                          (declare (ignore user))
                          (write-line "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.")
                          nil))
    :must-wear* '(padding
                     . (lambda (user)
                           (declare (ignore user))
                           (write-line "This area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.")
                           nil))
    :must-not-wear '((and closed-bottoms (not incontinence-product))
                        . (lambda (user)
                              (declare (ignore user))
                              (write-line "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.")
                              nil))
    :must-not-wear* '((and closed-bottoms (not incontinence-product))
                         . (lambda (user)
                               (declare (ignore user))
                               (write-line "This area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.")
                               nil))
    :can-potty '(lambda
                     (prop &key wet mess pants-down user)
                     (declare (ignorable prop wet mess pants-down user))
                     (not
                         (when (or
                                   pants-down
                                   (not (filter-items (wear-of user) 'closed-bottoms)))
                             (format t "*The shopkeeper baps ~a on the nose with a newspaper before ~a gets the chance to go*~%"
                                 (name-of user)
                                 (name-of user))
                             (format t "Shoppkeeper: Bad ~a, no going potty inside~%"
                                 (species-of user))
                             (when (or
                                       (>=
                                           (bladder/contents-of user)
                                           (bladder/potty-dance-limit-of user))
                                       (>=
                                           (bowels/contents-of user)
                                           (bowles/potty-dance-limit-of user))))
                             (format t "*~a whines and continues ~a embarrassing potty dance while the shopkeeper watches in amusement*~%~%"
                                 (name-of user)
                                 (if (malep user)
                                     "his"
                                     "her"))
                             t)))
    :potty-trigger '(lambda (had-accident user)
                        (block nil
                            (when (not (filter-items (wear-of user) 'incontinence-product))
                                (format t "*The shopkeeper baps ~a on the nose with a newspaper*~%"
                                    (name-of user))
                                (format t "Shoppkeeper: Bad ~a, no going potty inside~%"
                                    (species-of user)))
                            (when (or (getf (car had-accident) :popped)
                                      (getf (cdr had-accident) :popped))
                                (format t "*The shopkeeper falls over laughing with his diaper clearly exposed from under his tunic, then gets an embarrased look on his face when he floods his diaper from the laughter, which is incredibly obvious from the wetness indicator changing color*~%~%")
                                (format t "*A random raccoon in the shop records the shopkeeper flooding his pamps then uploads it to the internet*~%~%")
                                (trigger-event 'yadfa-events:shopkeeper-floods-himself-1))
                            (when (> (getf (car had-accident) :leak-amount) 0)
                                (format t "*The shopkeeper laughs at ~a's misfortune*~%"
                                    (name-of user))
                                (return))
                            (when (> (getf (cdr had-accident) :leak-amount) 0)
                                (format t "Shopkeeper: Bad ~a!!! No going potty on the floor!!!~%~%"
                                    (name-of user))
                                (apply #'format t "*The Shopkeeper spanks ~a through ~a messy diaper and makes ~a sit in it in timeout*~%"
                                    (name-of user)
                                    (if (malep user)
                                        '("his" "him")
                                        '("her" "her")))
                                (return))
                            (when (> (getf (car had-accident) :wet-amount) 0)
                                (format t "Shopkeper: Aww, is ~a using ~a diapers like a baby?~%"
                                    (name-of user)
                                    (if (malep user)
                                        "his"
                                        "her"))
                                (return))
                            (when (> (getf (cdr had-accident) :mess-amount) 0)
                                (format t "Shopkeeper: Looks like ~a made a stinky!!!~%~%"
                                    (name-of user))
                                (format t "*The Shopkeeper mushes ~a's messy diaper who quickly jerks away and then grabs the back of ~a diaper struggling to unmush it*~%"
                                    (name-of user)
                                    (if (malep user)
                                        "his"
                                        "her"))
                                (return))))
    :props (list
               :shop (make-instance 'shop
                         :actions (list
                                      :ask-for-bathroom
                                      (make-action
                                          :documentation "Ask the raccoons if you can use the bathroom."
                                          :lambda '(lambda
                                                       (prop &rest keys &key &allow-other-keys)
                                                       (declare (ignore prop keys))
                                                       (format t "Diapered Raccon Bandit Shop Owner: Sorry, only I'm allowed in there. Everyone else can just use their diapers. Isn't that right mushbutt?~%~%*The Shop Owner slaps the back of the Rookie's diaper*~%~%*Rookie yelps then grabs the back of his diaper and struggles to unmush it*~%~%*The Shop Owner laughs*~%~%Rookie Raccoon: Can I please get a diaper change now?~%~%Shop Owner: Keep asking me that and I'll make you sit in it in timeout again.~%~%Rookie Raccoon: NO! PLEASE! I'LL BE GOOD!~%~%")))
                                      :ask-why-youre-allowed-to-shop
                                      (make-action
                                          :documentation "Ask the raccoons why you're allowed to shop here without the gang attacking you"
                                          :lambda '(lambda
                                                       (prop &rest keys &key &allow-other-keys)
                                                       (declare (ignore prop keys))
                                                       (format t "~a: You know how the gang seems to attack me everywhere I show up?~%~%"
                                                           (name-of (player-of *game*)))
                                                       (format t "Shop Owner: Yeah?~%~%")
                                                       (format t "~a: Well how come they're letting me shop here without attacking me?~%~%"
                                                           (name-of (player-of *game*)))
                                                       (format t "Shop Owner: Because money stupid.")))
                                      :ask-what-they-do-with-sold-items
                                      (make-action
                                          :documentation "Ask the raccoons what they do with the random crap you sell them"
                                          :lambda '(lambda
                                                       (prop &rest keys &key &allow-other-keys)
                                                       (declare (ignore prop keys))
                                                       (format t "~a: So what the hell do you do with all the random crap we sell you~%~%"
                                                           (name-of (player-of *game*)))
                                                       (format t "Shop Owner: We dump it all on ~a's garbage collector. Yes, I know, buying all this crap only to throw it out is dumb. Blame Pouar for designing it this way." (lisp-implementation-type)))))
                         :items-for-sale '((yadfa-items:gold-pacifier)
                                              (yadfa-items:recovering-pacifier)
                                              (yadfa-items:healing-pacifier)
                                              (yadfa-items:bandit-swimsuit)
                                              (yadfa-items:bandit-uniform-tunic)
                                              (yadfa-items:bandit-uniform-shirt)
                                              (yadfa-items:bandit-uniform-sports-bikini-top)
                                              (yadfa-items:monster-energy-drink)
                                              (yadfa-items:spiked-bottle-of-milk)
                                              (yadfa-items:bandit-diaper)
                                              (yadfa-items:bandit-adjustable-diaper)
                                              (yadfa-items:bandit-female-diaper)
                                              (yadfa-items:bandit-swim-diaper-cover)
                                              (yadfa-items:lower-bandit-swim-diaper-cover)
                                              (yadfa-items:female-bandit-swim-diaper-cover)
                                              (yadfa-items:gold-collar)
                                              (yadfa-items:ak47)
                                              (yadfa-items:box-of-7.62×39mm)
                                              (yadfa-items:pink-sword)
                                              (yadfa-items:toddler-dress)))
               :changing-table (make-instance 'automatic-changing-table)
               :bed (make-instance 'bed)
               :checkpoint (make-instance 'checkpoint))
    :events (list 'yadfa-events:enter-bandits-shop-1 'yadfa-events:obtain-diaper-lock-1 'yadfa-events:enter-bandits-shop-3))
(ensure-zone (-3 23 0 bandits-domain)
    :name "Bandit's Shop Bathroom"
    :description "CLOSED FOREVER!!!!! MUAHAHAHAHA!!!!"
    :locked nil)
(ensure-zone (-5 22 0 bandits-domain)
    :name "Bandit's Kennel"
    :description "A grungey looking kennel where the Raccoon Bandits keep their `pets'. Negleted so much that they literally forgot about their existence"
    :enter-text "You enter the kennel"
    :events (list 'yadfa-events:enter-bandits-kennel-1))
(ensure-zone (0 21 0 bandits-domain)
    :name "Bandit's Town Entrance"
    :description "The enterance to Bandit Town"
    :enter-text "You're at the enterance of Bandit Town"
    :warp-points (list 'home '(0 1 0 home))
    :events (list 'yadfa-events:enter-bandits-village-1))
(macro-level
    `(progn
         ,@(iter (for i from 22 to 30)
               (collect
                   `(ensure-zone (0 ,i 0 bandits-domain)
                        :name "Bandit's Town"
                        :description "A town run by the Raccoon Bandits"
                        :enter-text "You're wander around Bandit's Town"
                        :warp-points ',(when (= i 30) '(silver-cape (0 0 0 silver-cape))))))))
(ensure-zone (1 21 0 bandits-domain)
    :name "Bandit's Cove Dock"
    :description "The dock of Bandit's Cove"
    :enter-text "You're at a dock")
(macro-level
    `(progn
         ,@(let ((a ()))
               (iter (for y from 19 to 23)
                   (alexandria:appendf
                       a
                       (iter
                           (for x from 2 to 6)
                           (collect
                               `(ensure-zone (,x ,y 0 bandits-domain)
                                    :name "Bandit's Cove"
                                    :description "A cove filled with bandits"
                                    :enter-text "You're at a cove runned by bandits"
                                    :enemy-spawn-list
                                    (list '(:max-random 10
                                               :enemies
                                               ((yadfa-enemies:rookie-diapered-raccoon-bandit .
                                                    (list
                                                        :level (random-from-range 2 5)
                                                        :wear (list
                                                                  (make-instance
                                                                      'yadfa-items:lower-bandit-swim-diaper-cover)
                                                                  (make-instance
                                                                      'yadfa-items:bandit-diaper
                                                                      :sogginess (random 1000)
                                                                      :messiness (random 6000)))
                                                        :level (random-from-range 2 5)))))
                                        '(:max-random 10
                                             :enemies
                                             ((yadfa-enemies:diapered-raccoon-bandit .
                                                  (list
                                                      :level (random-from-range 2 5)
                                                      :wear (list
                                                                (make-instance
                                                                    'yadfa-items:bandit-swimsuit/closed)
                                                                (make-instance
                                                                    'bandit-swim-diaper-cover)
                                                                (make-instance
                                                                    'yadfa-items:bandit-diaper))
                                                      :level (random-from-range 2 5)))))
                                        '(:max-random 10
                                             :enemies
                                             ((yadfa-enemies:female-diapered-raccoon-bandit .
                                                  (list
                                                      :level (random-from-range 2 5)
                                                      :wear (list
                                                                (make-instance
                                                                    'yadfa-items:bandit-uniform-sports-bikini-top)
                                                                (make-instance
                                                                    'yadfa-items:female-bandit-swim-diaper-cover)
                                                                (make-instance
                                                                    'yadfa-items:bandit-female-diaper
                                                                    :sogginess (random 1000)
                                                                    :messiness (random 6000)))
                                                      :level (random-from-range 2 5)))))))))))
               a)))
(ensure-zone (6 24 0 bandits-domain)
    :name "Bandit's Cave Entrance"
    :description "A mysterious cave"
    :enter-text "You Enter the cave")
(ensure-zone (6 24 -2 bandits-domain)
    :name "Bandit's Cave"
    :description "A mysterious cave"
    :enter-text "You Enter the cave"
    :warp-points (list 'cave-entrance '(6 24 0 bandits-domain)
                     'descend '(6 24 2 bandits-domain))
    :events (list 'yadfa-events:decend-bandits-cave-1))
