;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent enter-bandits-village-1
  :lambda (lambda (self)
            (declare (ignore self))
            (set-new-battle '((yadfa-enemies:diapered-raccoon-bandit . '(:level 5)))
                            :continuable t
                            :enter-battle-text (format nil "Raccoon Bandit: This area is under control of the Raccoon Bandits. If you want in you have to get past me~%*Cue battle transition. You can't see it because this is a text based game, but trust us, it's there.*~%"))))
(defevent enter-bandits-shop-1
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "Diapered Raccoon Bandit Shop Owner: Hey, we got a customer. Stop dancing around pulling on a locked bathroom door and help him out." #\Newline #\Newline
                   "Rookie Diapered Raccoon Bandit Servant: But I really gotta gooooo!!!!! *hops from foot to foot holding the front of his diaper*" #\Newline #\Newline
                   "*The shop owner walks over to the servant and starts tickling him*" #\Newline #\Newline
                   "Rookie Diapered Raccoon Bandit Servant: NOOOO!!! STOP!!!! I CAN'T HOLD IT!!!!" #\Newline #\Newline
                   "*The rookie starts laughing and thrashing about, the insignia that doubles as a wetness indicator on the front of his diaper then turns from blue to yellow.*" #\Newline #\Newline
                   "Shop Owner: There, you went, now go see to the customer." #\Newline #\Newline
                   "*The now blushy Rookie Diapered Raccoon Bandit waddles and squishes over to the back of the counter*" #\Newline #\Newline)))
(defevent enter-bandits-shop-3
  :predicate (lambda (self)
               (declare (ignore self))
               (= (random 5) 0))
  :finished-depends (list 'enter-bandits-shop-1)
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "*The shopkeeper hops around from foot to foot fiddling with the bathroom door with one paw while clutching the front of his diaper with the other*" #\Newline #\Newline
                   "Shopkeeper: I can't believe I lost the stupid key!!!!!" #\Newline #\Newline
                   "*The shopkeeper turns to you, pressing his knees together bobbing up and down with both paws pressed firmly on the front of his diaper*" #\Newline #\Newline
                   "Shopkeeper: HELP ME!!!!!" #\Newline #\Newline
                   "*" (name-of (player-of *game*)) " shrugs and tickles the shopkeeper*" #\Newline #\Newline
                   "Shopkeeper: NOOOO!!!!!! NOT WHAT I MEANT!!!!!" #\Newline #\Newline
                   "*The shopkeeper laughs and thrashes about and floods his diapers*"
                   (name-of (player-of *game*)) ": You're welcome" #\Newline #\Newline
                   "*The shopkeeper blushes heavily and grumbles*" #\Newline #\Newline)))
(defevent enter-bandits-shop-2
  :predicate (lambda (self)
               (declare (ignore self))
               (= (random 5) 0))
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "*A diapered Raccoon is doing a potty dance outside the store in a full diaper squishing with every step*" #\Newline #\Newline
                   "Raccoon: LET ME IN!!!! I CAN'T HOLD IT MUCH LONGER!!!!" #\Newline #\Newline
                   "Shop Owner: Sorry leaky, you're not allowed in the shop since you leave puddles everywhere" #\Newline #\Newline
                   "Raccoon: I'M NOT LEAKY!!!! *blushes angrily*" #\Newline #\Newline
                   "*The raccoon has an accident and his diaper leaks and leaves puddles everywhere*" #\Newline #\Newline
                   "Shop Owner: Then explain the state of your diapers" #\Newline #\Newline
                   "*The raccoon blushes then starts to \\sout{waddle away with his legs spread apart} \"sneak away\" hoping no one will notice what he did \\sout{even though the trail he's leaving makes it obvious}*" #\Newline #\Newline
                   "*" (name-of (player-of *game*)) " laughs at the leaky raccoon*" #\Newline #\Newline
                   "Leaky raccoon blushing angrily: What are you laughing at?" #\Newline #\Newline)
            (set-new-battle '((yadfa-enemies:diapered-raccoon-bandit . (list :level (random-from-range 2 5)
                                                                             :wear (list
                                                                                    (make-instance 'yadfa-items:bandit-uniform-tunic)
                                                                                    (make-instance 'yadfa-items:bandit-adjustable-diaper :sogginess 1400
                                                                                                                                         :messiness 8000)))))
                            :enter-battle-text (format nil "The leaky raccoon waddles over to you to fight~%"))))
(defevent decend-bandits-cave-1
  :repeatable t
  :lambda (lambda (self)
            (declare (ignorable self))
            (if (< (random 15) 1)
                (progn
                  (format t "You found a treasure chest, use “(interact :chest :take :all)” to take all the treasure from it~%")
                  (setf (getf (get-props-from-zone '(6 24 -2 "bandits-domain")) :chest)
                        (make-instance 'prop
                                       :name "Treasure Chest"
                                       :description "A treasure chest"
                                       :items (cond ((= (random 10) 0)
                                                     (list (make-instance 'yadfa-items:gold-collar
                                                                          :value (random-from-range 25000 50000))))
                                                    ((= (random 10) 0)
                                                     (list (make-instance 'yadfa-items:gold-pacifier
                                                                          :value (random-from-range 10000 20000))))
                                                    ((= (random 10) 0)
                                                     (list (make-instance 'yadfa-items:gem
                                                                          :value (random-from-range 25000 50000))))
                                                    ((= (random 20) 0)
                                                     (list (make-instance 'yadfa-items:gold-bar
                                                                          :value (random-from-range 50000 100000)))))
                                       :bitcoins (random-from-range 12500 25000))))
                (remf (get-props-from-zone '(6 24 -2 "bandits-domain")) :chest))
            (cond
              ((< (random 12) 1)
               (set-new-battle '((yadfa-enemies:diapered-raccoon-bandit . (list
                                                                           :level (random-from-range 2 5)
                                                                           :bladder/contents (random-from-range 450 550)))
                                 (yadfa-enemies:diapered-raccoon-bandit . (list
                                                                           :level (random-from-range 2 5)
                                                                           :bladder/contents (random 400)
                                                                           :bowels/contents (random 5000))))
                               :enter-battle-text (format nil "Random Raccoon 1: Hey, I need to take a break. I really gotta go.~%~%Random Raccoon 2: Well you're gonna have to hold it, as we seem to have an intruder.~%~%Random Raccoon 1: But.. but... ooooohhhh *clutches the front of his diaper pressing his knees together*~%~%Time for battle!!!~%~%")))
              ((< (random 12) 1)
               (format t "*~a hears grunting and looks to find one of the raccoons squatting down and messing his pamps. After finishing, he quickly stands up blushing heavily.*~%~%"
                       (name-of (player-of *game*)))
               (f:fmt t "Diapered Raccoon Bandit: You didn't see nothing." #\Newline #\Newline
                      (name-of (player-of *game*)) ": Oh really? *" (name-of (player-of *game*)) " lifts up the back of the raccoon's tunic and press hard against the back of the raccoon's diaper hard.*" #\Newline #\Newline
                      "*The raccoon yelps and quickly puts his paws on the back of his diaper struggling to unmush it.*" #\Newline #\Newline)
               (when (> (list-length (allies-of *game*)) 0)
                 (if (= (list-length (allies-of *game*)) 1)
                     (format t "*~a ~a*~%~%"
                             (name-of (nth 0 (allies-of *game*)))
                             (if (malep (nth 0 (allies-of *game*)))
                                 "snickers"
                                 "giggles"))
                     (f:fmt t "*Diapered Raccoon Bandit's team mates laugh*" #\Newline #\Newline)))
               (f:fmt t "Diapered Raccoon Bandit: You're gonna pay for that!!!!" #\Newline #\Newline)
               (if (< (random 2) 1)
                   (progn
                     (f:fmt t (name-of (player-of *game*)) ": I think it's you who are going to pay, unless you want me to blab to your friends telling them what you just did." #\Newline #\Newline
                            "*A soft hiss can be heard as the raccoon floods his diapers in a panic*" #\Newline #\Newline
                            "Diapered Raccoon Bandit: No please!!! I'll give you anything you want!!!" #\Newline #\Newline
                            (name-of (player-of *game*)) ": All your money and all your stuff" #\Newline #\Newline
                            "Diapered Raccoon Bandit: But... but... *groans* alright" #\Newline #\Newline
                            (name-of (player-of *game*)) " gets everything the raccoon is carrying except the clothes and diapers the raccoon is wearing. The raccoon then waddles off with his legs spread apart like a 5 year old who didn't make it to the toilet in time." #\Newline #\Newline)
                     (incf (bitcoins-of (player-of *game*)) (random-from-range 50000 100000))
                     (iter (for i from 0 to (random 5))
                           (push (make-instance 'yadfa-items:gold-bar) (inventory-of (player-of *game*))))
                     (iter (for i from 0 to (random 5))
                           (push (make-instance 'yadfa-items:gem) (inventory-of (player-of *game*))))
                     (iter (for i from 0 to (random 5))
                           (push (make-instance 'yadfa-items:bandit-uniform-tunic) (inventory-of (player-of *game*))))
                     (iter (for i from 0 to (random 20))
                           (push (make-instance 'yadfa-items:bandit-adjustable-diaper) (inventory-of (player-of *game*))))
                     (push (make-instance 'yadfa-items:bandit-swimsuit/closed) (inventory-of (player-of *game*))))
                   (set-new-battle '((yadfa-enemies:diapered-raccoon-bandit . (list :level (random-from-range 2 5)
                                                                                    :bowels/contents 0
                                                                                    :wear (list
                                                                                           (make-instance 'yadfa-items:bandit-uniform-tunic)
                                                                                           (make-instance 'yadfa-items:bandit-adjustable-diaper
                                                                                            :messiness 8000))))))))
              ((< (random 12) 1)
               (set-new-battle '((yadfa-enemies:female-diapered-raccoon-bandit . (list :level (random-from-range 2 5))))))
              ((< (random 12) 1)
               (set-new-battle '((yadfa-enemies:rookie-diapered-raccoon-bandit . (list :level (random-from-range 2 5))))))
              ((< (random 12) 1)
               (set-new-battle '((yadfa-enemies:diapered-raccoon-bandit . (list :level (random-from-range 2 5))))))
              ((< (random 12) 1)
               (set-new-battle '((yadfa-enemies:giant-diapered-raccoon-bandit . (list :level (random-from-range 5)))))))))
(defevent obtain-diaper-lock-1
  :finished-depends '(enter-bandits-shop-1 get-diaper-locked-1)
  :predicate (lambda (self)
               (declare (ignorable self))
               (and (lockedp (car (last (wear-of (player-of *game*))))) (>= (bitcoins-of (player-of *game*)) 10000)
                    (or (> (bladder/contents-of (player-of *game*)) (* (bladder/maximum-limit-of (player-of *game*)) 5/6))
                        (> (bowels/contents-of (player-of *game*)) (* (bowels/maximum-limit-of (player-of *game*)) 5/6)))
                    (destructuring-bind (&key (sogginess 0) (sogginess-capacity 0) (messiness 0) (messiness-capacity 0))
                        (calculate-diaper-usage (player-of *game*))
                      (declare (type real sogginess sogginess-capacity messiness messiness-capacity))
                      (and (< sogginess (/ sogginess-capacity 4))
                           (< messiness (/ messiness-capacity 4))))))
  :lambda (lambda (self)
            (declare (ignorable self))
            (pushnew '(yadfa-items:magic-diaper-key . (list :value 10000))
                     (yadfa-props:items-for-sale-of (getf (get-props-from-zone '(-3 22 0 yadfa-zones:bandits-domain)) :shop)))
            (f:fmt t "Shop owner: Seems you're stuck in a locked diaper. I can help." #\Newline #\Newline
                   (name-of (player-of *game*)) ": I'm listening" #\Newline #\Newline)
            (format t "I got one of those special artifacts that is used to lock and unlock these diapers. I can give it to you for 10000 bitcoins. Better pay up before you ~a yourself~%~%"
                    (cond ((and (> (bladder/contents-of (player-of *game*)) (* (bladder/maximum-limit-of (player-of *game*)) 5/6))
                                (> (bowels/contents-of (player-of *game*)) (* (bowels/maximum-limit-of (player-of *game*)) 5/6)))
                           "flood and mess")
                          ((> (bladder/contents-of (player-of *game*)) (* (bladder/maximum-limit-of (player-of *game*)) 5/6))
                           "flood")
                          ((> (bowels/contents-of (player-of *game*)) (* (bowels/maximum-limit-of (player-of *game*)) 5/6))
                           "mess")))
            (f:fmt t (name-of (player-of *game*)) ": Forget it!!! I don't need your help!!!" #\Newline #\Newline)
            (format t "*~a starts to waddle out the door, stops, then ~a*~%~%"
                    (name-of (player-of *game*))
                    (if (>
                         (bladder/contents-of (player-of *game*))
                         (* (bladder/maximum-limit-of (player-of *game*)) 5/6))
                        (format nil "crosses ~a legs and clenches the front of ~a diaper"
                                (if (malep (player-of *game*)) "his" "her")
                                (if (malep (player-of *game*)) "his" "her"))
                        (format nil "grabs the back of ~a diaper while clenching ~a butt cheeks"
                                (if (malep (player-of *game*)) "his" "her")
                                (if (malep (player-of *game*)) "his" "her"))))
            (f:fmt t (name-of (player-of *game*)) ": On second thought, GIVE ME THAT KEY!!!!" #\Newline #\Newline
                   "*" (name-of (player-of *game*)) " pays up*" #\Newline #\Newline
                   "Shop Owner: Thank you for your business, but before I hand you the key" #\Newline #\Newline)
            (let ((a (cons (when (> (bladder/contents-of (player-of *game*)) (bladder/potty-dance-limit-of (player-of *game*)))
                             (wet))
                           (when (> (bowels/contents-of (player-of *game*)) (bowels/potty-dance-limit-of (player-of *game*)))
                             (mess)))))
              (cond ((and (car a) (cdr a))
                     (f:fmt t "*The raccoon puts on earplugs, then turns on the speakers in the room which starts playing the brown note, then starts tickling " (name-of (player-of *game*)) "*" #\Newline #\Newline
                            (name-of (player-of *game*)) ": ACK!!! NO!!! STOP!!!" #\Newline #\Newline)
                     (format t "*~a starts squirming and giggling then wets and messes ~a diapers*~%~%"
                             (name-of (player-of *game*))
                             (if (malep (player-of *game*)) "his" "her")))
                    ((car a)
                     (f:fmt t "*The raccoon starts tickling " (name-of (player-of *game*)) "*" #\Newline #\Newline
                            (name-of (player-of *game*)) ": ACK!!! NO!!! STOP!!!" #\Newline #\Newline)
                     (format t "*~a starts squirming and giggling then wets ~a diapers*~%~%"
                             (name-of (player-of *game*))
                             (if (malep (player-of *game*)) "his" "her")))
                    ((cdr a)
                     (f:fmt t "*The raccoon puts on earplugs, then turns on the speakers in the room which starts playing the brown note" #\Newline #\Newline)
                     (format t "*~a quickly grabs the back of ~a diapers before messing ~aself*~%~%"
                             (name-of (player-of *game*))
                             (if (malep (player-of *game*)) "his" "her")
                             (if (malep (player-of *game*)) "his" "her"))))
              (multiple-value-bind (value key)
                  (pop-from-expansion (player-of *game*) a)
                (when (eq key :wet/mess)
                  (setf a value)))
              (f:fmt t "*The raccoon starts laughing*" #\Newline #\Newline
                     "Shop Owner: That never gets old. Here's the key as promised, enjoy " (if (car a) "sog" "mush") "butt" #\Newline #\Newline)
              (when (cdr a)
                (f:fmt t "*The Rookie Raccoon waddles in*" #\Newline #\Newline
                       "Rookie Raccoon: I wish you wouldn't play that. You made me mess my diapers again" #\Newline #\Newline
                       "Shop Owner: Ha, you were gonna mess yourself anyway, so it's not like it matters." #\Newline #\Newline))
              (f:fmt t "*After laughing his ass off, he suddenly stops and a soft hiss can be heard, the raccoon's face turns bright red and he quickly grabs the front of his diaper. The hissing continues for 10 seconds while everyone in the room stares at him while he floods his diapers.*" #\Newline #\Newline
                     "*" (name-of (player-of *game*)) " lifts up the raccoon's tunic and squishes the front of his diaper*" #\Newline #\Newline
                     (name-of (player-of *game*)) ": Seems you've had an accident too" #\Newline #\Newline
                     "Shop Owner: Shut up!!! *pulls his tunic back down while blushing bright red*" #\Newline #\Newline))))
(defevent enter-bandits-kennel-1
  :lambda (lambda (self)
            (declare (ignorable self))
            (let ((a nil)
                  (b nil))
              (f:fmt t "*You enter the kennel and in 2 of the cages you see 2 orange foxes in heavily used diapers.*" #\Newline #\Newline
                     "Vixen: Hey! someone has entered the kennel for once!!!" #\Newline #\Newline
                     "Fox: Could you let us out? We're the town's “pets” but they seemed to have forgotten about us." #\Newline #\Newline
                     "Fox: and while you're at it, could you give us... umm... you know.... *blushes heavily*" #\Newline #\Newline
                     (name-of (player-of *game*)) ": A diaper change?" #\Newline #\Newline
                     "*the fox nods with a blush on his face*" #\Newline #\Newline
                     (name-of (player-of *game*)) ": Fine, I'll be right back, don't go anywhere." #\Newline #\Newline
                     "*the 2 foxes sit down and waits, or in their case, crouches down and waits*" #\Newline #\Newline
                     "*as sitting in diapers in that state is out of the question" #\Newline #\Newline
                     "*" (name-of (player-of *game*)) " enters the Bandit's shop, find a whimpering Rookie Raccoon in a toddler's dress and messy diaper being bounced on shop owner's knee*" #\Newline #\Newline
                     "*At least you now know why this shop carries Toddler dresses, while they're distracted*" #\Newline #\Newline
                     "*You grab a couple of adjustable diapers and a gold collar for the fox and a toddler's dress*" #\Newline #\Newline
                     "*and a few of the much thicker diapers that the female raccoons wear for the vixen, you then head back to the kennel and dress the foxes and give them a diaper change*" #\Newline #\Newline
                     (name-of (player-of *game*)) ": I'm back, and got you some new clothes" #\Newline #\Newline
                     "Fox: Cool" #\Newline #\Newline
                     "Vixen: Yay" #\Newline #\Newline
                     "*the 2 wag their tails happily*" #\Newline #\Newline
                     (name-of (player-of *game*)) ": Mind telling me your names?" #\Newline #\Newline)
              (finish-output)
              (accept-with-effective-frame (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
                                                                  (fresh-line *query-io*)
                                                                  (setf a (make-instance 'yadfa-allies:chris
                                                                                         :name (clim:accept 'string
                                                                                                            :prompt "Fox Name"
                                                                                                            :default (second
                                                                                                                      (assoc :name (progn
                                                                                                                                     (c2mop:ensure-finalized
                                                                                                                                      (find-class 'yadfa-allies:chris))
                                                                                                                                     (c2mop:compute-default-initargs
                                                                                                                                      (find-class 'yadfa-allies:chris)))))
                                                                                                            :view clim:+text-field-view+
                                                                                                            :stream *query-io*)))
                                                                  (fresh-line *query-io*)
                                                                  (setf b (make-instance 'yadfa-allies:kristy
                                                                                         :name (clim:accept 'string
                                                                                                            :prompt "Vixen Name"
                                                                                                            :default (second
                                                                                                                      (assoc :name (progn
                                                                                                                                     (c2mop:ensure-finalized
                                                                                                                                      (find-class 'yadfa-allies:kristy))
                                                                                                                                     (c2mop:compute-default-initargs
                                                                                                                                      (find-class 'yadfa-allies:kristy)))))
                                                                                                            :view clim:+text-field-view+
                                                                                                            :stream *query-io*)))))
              (iter (for i in (list a b))
                    (do-push i (team-of *game*) (allies-of *game*)))
              (f:fmt t "Fox: I'm " (name-of a) #\Newline #\Newline
                     "Vixen: And I'm " (name-of b) #\Newline #\Newline
                     (name-of a) ": What's yours?" #\Newline #\Newline
                     (name-of (player-of *game*)) ": I'm " (name-of (player-of *game*)) ". Now lets get you dressed" #\Newline #\Newline
                     "*" (name-of a) " puts the new clothes and diapers on the foxes*" #\Newline #\Newline))))
(defevent get-warp-pipe-summoner-1
  :finished-depends '(enter-bandits-shop-1 enter-bandits-kennel-1)
  :lambda (lambda (self)
            (declare (ignorable self))
            (f:fmt t "*You enter the shop to find the whimpering Rookie still in a toddler's dress thrashing around and bouncing up and down in a baby bouncer*" #\Newline #\Newline
                   "Shop Owner: The baby decided to flood his diapers while I was giving him knee bounces, so I decided to reward him with happy bouncy fun time in the baby bouncer" #\Newline #\Newline
                   "Rookie: You wouldn't let me go and I couldn't hold it any longer!!!!" #\Newline #\Newline
                   "Shop Owner: A likely story. Anyway, before you go, I'd like to show you something. It's based on a dream the creator of this game had." #\Newline #\Newline
                   "*The shop owner brings out a warp device and summons a warp pipe.*" #\Newline #\Newline
                   "Shop Owner: This warp pipe leads to the secret underground. In there is an infinite supply of several resources, a place where ABDLs can be themselves, and more warp pipes that let you warp just about anywhere in the game, though to avoid breaking the storyline, the creator of the game limited these warp pipes to the places you've already been" #\Newline #\Newline
                   (name-of (player-of *game*)) ": Why are you giving me this?" #\Newline #\Newline
                   "Shop Owner: Easy way to get back to this shop, so you can buy more crap from us." #\Newline #\Newline
                   (name-of (player-of *game*)) ": Ok *grabs the device and puts it in " (if (malep (player-of *game*)) "his" "her") " inventory*" #\Newline #\Newline)
            (push (make-instance 'yadfa-items:warp-device) (inventory-of (player-of *game*)))
            (setf (hiddenp (get-zone '(0 31 0 yadfa-zones:bandits-domain))) nil)))
(defevent shopkeeper-floods-himself-1)
