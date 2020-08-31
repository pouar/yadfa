;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
#.`(progn
     ,@(iter (for i from 10 to 20)
             (collect
              `(ensure-zone (0 ,i 0 bandits-domain)
                            :name "Bandit's Way"
                            :description "A path filled with bandits"
                            :enter-text "You follow the path"
                            :warp-points ,(when (= i 10) '(list 'ironside '(2 0 0 ironside)))
                            :enemy-spawn-list 'bandits-way)))
     ,@(iter (for i from -1 downto -10)
             (collect
              `(ensure-zone (,i 21 0 bandits-domain)
                            :name "Bandit's Town"
                            :description "A town run by the Raccoon Bandits"
                            :enter-text "You're wander around Bandit's Town"
                            ,@(when (= i -3)
                                '(:events '(yadfa-events:enter-bandits-shop-2)))))))
(ensure-zone (-3 22 0 bandits-domain)
             :name "Bandit's Shop"
             :description "A local shop"
             :enter-text "You enter the Bandit's Shop"
             :must-wear (cons 'padding '(lambda (user)
                                         (declare (ignore user))
                                         (write-line "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is mandatory.")
                                         nil))
             :must-wear* (cons 'padding '(lambda (user)
                                          (declare (ignore user))
                                          (write-line "This area is a diapers only pants free zone. Pants are strictly prohibited and padding is mandatory.")
                                          nil))
             :must-not-wear (cons '(and closed-bottoms (not incontinence-product))
                                  '(lambda (user)
                                    (declare (ignore user))
                                    (write-line "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is mandatory.")
                                    nil))
             :must-not-wear* (cons '(and closed-bottoms (not incontinence-product))
                                   '(lambda (user)
                                     (declare (ignore user))
                                     (write-line "This area is a diapers only pants free zone. Pants are strictly prohibited and padding is mandatory.")
                                     nil))
             :can-potty '(lambda
                          (prop &key wet mess pants-down user)
                          (declare (ignorable prop wet mess pants-down user))
                          (not (when (or pants-down (not (filter-items (wear-of user) 'closed-bottoms)))
                                 (format t "*The shopkeeper baps ~a on the nose with a newspaper before ~a gets the chance to go*~%" (name-of user) (name-of user))
                                 (format t "Shopkeeper: Bad ~a, no going potty inside~%" (species-of user))
                                 (when (or (>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
                                           (>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))))
                                 (format t "*~a whines and continues ~a embarrassing potty dance while the shopkeeper watches in amusement*~%~%"
                                         (name-of user)
                                         (if (malep user)
                                             "his"
                                             "her"))
                                 t)))
             :potty-trigger '(lambda (had-accident user)
                              (block nil
                                (when (not (filter-items (wear-of user) 'incontinence-product))
                                  (format t "*The shopkeeper baps ~a on the nose with a newspaper*~%" (name-of user))
                                  (format t "Shopkeeper: Bad ~a, no going potty inside~%" (species-of user)))
                                (when (or (getf (car had-accident) :popped)
                                          (getf (cdr had-accident) :popped))
                                  (write-string #.(with-output-to-string (s)
                                                    (format s "*The shopkeeper falls over laughing with his diaper clearly exposed from under his tunic, then gets an embarrassed look on his face when he floods his diaper from the laughter, which is incredibly obvious from the wetness indicator changing color*~%~%")
                                                    (format s "*A random raccoon in the shop records the shopkeeper flooding his pamps then uploads it to the internet*~%~%")))
                                  (trigger-event 'yadfa-events:shopkeeper-floods-himself-1))
                                (when (> (getf (car had-accident) :leak-amount) 0)
                                  (format t "*The shopkeeper laughs at ~a's misfortune*~%" (name-of user))
                                  (return))
                                (when (> (getf (cdr had-accident) :leak-amount) 0)
                                  (format t "Shopkeeper: Bad ~a!!! No going potty on the floor!!!~%~%" (name-of user))
                                  (apply #'format t "*The Shopkeeper spanks ~a through ~a messy diaper and makes ~a sit in it in timeout*~%"
                                         (name-of user)
                                         (if (malep user)
                                             '("his" "him")
                                             '("her" "her")))
                                  (return))
                                (when (> (getf (car had-accident) :wet-amount) 0)
                                  (format t "Shopkeeper: Aww, is ~a using ~a diapers like a baby?~%"
                                          (name-of user)
                                          (if (malep user)
                                              "his"
                                              "her"))
                                  (return))
                                (when (> (getf (cdr had-accident) :mess-amount) 0)
                                  (format t "Shopkeeper: Looks like ~a made a stinky!!!~%~%" (name-of user))
                                  (format t "*The Shopkeeper mushes ~a's messy diaper who quickly jerks away and then grabs the back of ~a diaper struggling to unmush it*~%"
                                          (name-of user)
                                          (if (malep user)
                                              "his"
                                              "her"))
                                  (return))))
             :props (list :shop (make-instance 'yadfa-props:shop
                                               :actions (list :ask-for-bathroom
                                                              (make-action :documentation "Ask the raccoons if you can use the bathroom."
                                                                           :lambda '(lambda
                                                                                     (prop &rest keys &key &allow-other-keys)
                                                                                     (declare (ignore prop keys))
                                                                                     (write-string #.(with-output-to-string (s)
                                                                                                       (format s "Diapered Raccoon Bandit Shop Owner: Sorry, only I'm allowed in there. Everyone else can just use their diapers. Isn't that right mushbutt?~%~%")
                                                                                                       (format s "*The Shop Owner slaps the back of the Rookie's diaper*~%~%")
                                                                                                       (format s "*Rookie yelps then grabs the back of his diaper and struggles to unmush it*~%~%")
                                                                                                       (format s "*The Shop Owner laughs*~%~%")
                                                                                                       (format s "Rookie Raccoon: Can I please get a diaper change now?~%~%")
                                                                                                       (format s "Shop Owner: Keep asking me that and I'll make you sit in it in timeout again.~%~%")
                                                                                                       (format s "Rookie Raccoon: NO! PLEASE! I'LL BE GOOD!~%~%")))))
                                                              :ask-why-youre-allowed-to-shop (make-action :documentation "Ask the raccoons why you're allowed to shop here without the gang attacking you"
                                                                                                          :lambda '(lambda
                                                                                                                    (prop &rest keys &key &allow-other-keys)
                                                                                                                    (declare (ignore prop keys))
                                                                                                                    (format t "~a: You know how the gang seems to attack me everywhere I show up?~%~%"
                                                                                                                     (name-of (player-of *game*)))
                                                                                                                    (format t "Shop Owner: Yeah?~%~%")
                                                                                                                    (format t "~a: Well how come they're letting me shop here without attacking me?~%~%"
                                                                                                                     (name-of (player-of *game*)))
                                                                                                                    (format t "Shop Owner: Because money, stupid.")))
                                                              :ask-what-they-do-with-sold-items (make-action :documentation "Ask the raccoons what they do with the random crap you sell them"
                                                                                                             :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                                                                                       (declare (ignore prop keys))
                                                                                                                       (format t "~a: So what the hell do you do with all the random crap we sell you~%~%"
                                                                                                                        (name-of (player-of *game*)))
                                                                                                                       (format t "Shop Owner: We dump it all on ~a's garbage collector. Yes, I know, buying all this crap only to throw it out is dumb. Blame Pouar for designing it this way." (lisp-implementation-type))))
                                                              :ask-why-this-shop-exists (make-action :documentation "Ask the raccoons why they need to sell items for profit instead of just stealing everything."
                                                                                                     :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                                                                               (declare (ignore prop keys))
                                                                                                               (format t "~a: So why do you even need this shop? Why not just steal everything?~%~%"
                                                                                                                (name-of (player-of *game*)))
                                                                                                               (write-line "Shop Owner: In case you haven't noticed, being stealthy enough to steal everything isn't all that easy when your diaper crinkles with each and every step you take. *crinkles and blushes*"))))
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
                                                                 (yadfa-items:toddler-dress)
                                                                 (yadfa-props:placable-bed)))
                          :changing-table (make-instance 'yadfa-props:automatic-changing-table)
                          :bed (make-instance 'yadfa-props:bed)
                          :checkpoint (make-instance 'yadfa-props:checkpoint))
             :events '(yadfa-events:enter-bandits-shop-1 yadfa-events:obtain-diaper-lock-1 yadfa-events:enter-bandits-shop-3 yadfa-events:get-warp-pipe-summoner-1))
(ensure-zone (-3 23 0 bandits-domain)
             :name "Bandit's Shop Bathroom"
             :description "CLOSED FOREVER!!!!! MUAHAHAHAHA!!!!"
             :locked t)
(ensure-zone (-5 22 0 bandits-domain)
             :name "Bandit's Kennel"
             :description "A grungy looking kennel where the Raccoon Bandits keep their “pets”. Neglected so much that they literally forgot about their existence"
             :enter-text "You enter the kennel"
             :events '(yadfa-events:enter-bandits-kennel-1))
(ensure-zone (0 21 0 bandits-domain)
             :name "Bandit's Town Entrance"
             :description "The entrance to Bandit Town"
             :enter-text "You're at the entrance of Bandit Town"
             :warp-points (list 'home '(0 1 0 home))
             :events '(yadfa-events:enter-bandits-village-1))
#.`(progn
     ,@(iter (for i from 22 to 30)
             (collect
              `(ensure-zone (0 ,i 0 bandits-domain)
                            :name "Bandit's Town"
                            :description "A town run by the Raccoon Bandits"
                            :enter-text "You're wander around Bandit's Town"))))
(ensure-zone (0 31 0 bandits-domain)
             :name "Bandit's Town"
             :description "A town run by the Raccoon Bandits"
             :enter-text "You see a sign that says \"To the south lies your generic RPG Maker Dungeon. Get ready for a mediocre adventure!!!! OOOOOOOOO!!!!"
             :warp-points (list 'rpgmaker-dungeon '(5 9 0 rpgmaker-dungeon))
             :hidden t
             :events '(yadfa-events:secret-underground-pipe-rpgmaker-dungeon))
(ensure-zone (1 21 0 bandits-domain)
             :name "Bandit's Cove Dock"
             :description "The dock of Bandit's Cove"
             :enter-text "You're at a dock")
#.`(progn
     ,@(let ((a ()))
         (iter (for y from 19 to 23)
               (alexandria:appendf a (iter (for x from 2 to 6)
                                           (collect `(ensure-zone (,x ,y 0 bandits-domain)
                                                                  :name "Bandit's Cove"
                                                                  :description "A cove filled with bandits"
                                                                  :enter-text "You're at a cove run by bandits"
                                                                  :enemy-spawn-list 'bandits-cove)))))
         a))
(ensure-zone (6 24 0 bandits-domain)
             :name "Bandit's Cave Entrance"
             :description "A mysterious cave"
             :enter-text "You enter the cave"
             :warp-points (list 'descend '(6 24 -2 bandits-domain)))
(ensure-zone (6 24 -2 bandits-domain)
             :name "Bandit's Cave"
             :description "A mysterious cave"
             :enter-text "You enter the cave"
             :warp-points (list 'cave-entrance '(6 24 0 bandits-domain)
                                'descend '(6 24 -2 bandits-domain))
             :events '(yadfa-events:decend-bandits-cave-1))
