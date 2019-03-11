(in-package :yadfa/events)
(defevent pirates-cove-1
    :lambda '(lambda (self)
                 (declare (ignorable self))
                 (let* ((a (make-instance 'yadfa/npcs:diaper-pirate))
                           (b (make-instance 'yadfa/npcs:thickly-diaper-pirate)))
                     (format t "*You see one of the Pirate ~as changing an ~a into multiple layers of padding*~%"
                         (species-of a) (species-of b))
                     (format t "*The ~a struggles to stand and waddles with the thick padding spreading ~a legs apart*~%"
                         (species-of b)
                         (if (malep b) "his" "her"))
                     (format t "Padded ~a: Aww, looks like the baby ~a is taking ~a first steps.~%"
                         (species-of a)
                         (species-of b)
                         (if (malep b) "his" "her"))
                     (format t "Thickly Padded ~a: Shut it. Hey, isn't that an intruder?~%"
                         (species-of b))
                     (format t "Padded ~a: Apparently.~%"
                         (species-of a))
                     (set-new-battle
                         `((yadfa/npcs:diaper-pirate . (list
                                                           :level (random-from-range 2 5)
                                                           :species ,(species-of a)
                                                           :male ,(malep a)))
                              (yadfa/npcs:thickly-diaper-pirate . (list
                                                                      :level (random-from-range 2 5)
                                                                      :species ,(species-of b)
                                                                      :male ,(malep b))))))))
(defevent pirates-cove-2
    :lambda '(lambda (self)
                 (declare (ignorable self))
                 (let* ((a (make-instance 'yadfa/npcs:diaper-pirate))
                           (coon (make-instance 'ally))
                           (diaper (make-instance 'yadfa/items:bandit-adjustable-diaper)))
                     (write-line "You find the diapered raccoon bandit back from Navy HQ in a pillory wearing nothing but 2 layers of thoroughly flooded and messy diapers")
                     (write-line "Diapered Raccoon Bandit: Ok! I learned my lesson! Can I please get a diaper change now?!?!? >///<")
                     (format t "*one of the ~as changes the outer layer of his diaper but still leaves him in the flooded and messy inner layer*~%" (species-of a))
                     (write-line "Diapered Raccoon Bandit: That's not what I meant!!!")
                     (format t "~a: You're still being punished for giving out location away. So you're going to stay wet and messy for the rest of your life.~%" (species-of a))
                     (format t "*The ~a decides to spank the raccoon through his messy padding for amusement. Judging from the face the raccoon is making, he doesn't like being mushed and humiliated like that*" (species-of a))
                     (format t "*You decide to take sympathy on the poor raccoon and rescue him. You knock the ~a out while ~a's distracted from abusing ~a former ally. Then release the raccoon from the stocks and hand him his tunic to help preserve whatever dignity he has left~%"
                         (species-of a)
                         (if (malep a) "he" "she")
                         (if (malep a) "his" "her"))
                     (write-line "Raccoon: umm, thanks.")
                     (format t "~a: got a name?~%" (name-of (player-of *game*)))
                     (setf (name-of coon) (first (prompt-for-values (string :prompt "Raccoon Name" :default "Slynk" :view clim:+text-field-view+))))
                     (format t "Raccoon: It's ~a~%" (name-of coon))
                     (format t "~a decides you can't be all bad since you're the first one to be nice to him (plus the Raccoon Bandits abandoned him) and decides to join your team~%"
                         (name-of coon))
                     (setf coon (make-instance 'ally
                                    :name (name-of coon)
                                    :male t
                                    :potty-training :last-minute
                                    :species "Raccoon"
                                    :description "Used to be one of the Diapered Raccoon Bandits. Was kicked out after he was forced to give the location of Pirate's Cove to the Navy. He was humiliated constantly by the Diapered Pirates until you rescued him. Is too embarrassed to admit when he as to go unless he's desperate"
                                    :level 5
                                    :bladder/contents (random (+ (bladder/potty-desperate-limit-of coon)
                                                                  (/ (- (bladder/potty-desperate-limit-of coon)
                                                                         (bladder/potty-dance-limit-of coon)))))
                                    :bowels/contents (random (+ (bowels/potty-desperate-limit-of coon)
                                                                 (/ (- (bowels/potty-desperate-limit-of coon)
                                                                        (bowels/potty-dance-limit-of coon)))))
                                    :wear (list
                                              (make-instance 'yadfa/items:bandit-uniform-tunic)
                                              (make-instance 'yadfa/items:thick-rubber-diaper)
                                              (make-instance 'yadfa/items:bandit-adjustable-diaper
                                                  :sogginess (sogginess-capacity-of diaper)
                                                  :messiness (messiness-capacity-of diaper)))))
                     (do-push coon (team-of *game*) (allies-of *game*))
                     (when (>= (bladder/contents-of coon) (bladder/need-to-potty-limit-of coon))
                         (format t "*~a grabs the front of his diaper*~%" (name-of coon))
                         (format t "~a: You don't have to go again do you?~%" (name-of (player-of *game*)))
                         (if (>= (bladder/contents-of coon) (bladder/potty-dance-limit-of coon))
                             (progn
                                 (format t "*~a starts hopping around while holding the front of ~a diaper*~%"
                                     (name-of coon)
                                     (if (malep coon) "his" "her")))
                             (progn
                                 (format t "*~a takes ~a hands off ~a diaper, fidgets a little and blushes*~%"
                                     (name-of coon)
                                     (if (malep coon) "his" "her")
                                     (if (malep coon) "his" "her"))))
                         (format t "~a: No, of course not. I'm not some baby who needs to ask to go to the bathroom all the time~%"
                                     (name-of coon))
                         (format t "*~a seems too embarrassed to admit when he has to use the toilet. He might change his mind if he gets desperate enough~%"
                             (name-of coon)))
                     (trigger-event (defevent unlock-slynk)))))
