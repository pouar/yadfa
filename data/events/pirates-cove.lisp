;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent pirates-cove-1
  :lambda (lambda (self)
            (declare (ignorable self))
            (let* ((a (make-instance 'yadfa-enemies:diaper-pirate))
                   (b (make-instance 'yadfa-enemies:thickly-diaper-pirate)))
              (out "*You see one of the Pirate " (species-of a) "s changing an " (species-of b) " into multiple layers of padding*" :% :%
                   "*The " (species-of b) " struggles to stand and waddles with the thick padding spreading " (if (malep b) "his" "her") " legs apart*" :% :%
                   "Padded " (species-of a) ": Aww, looks like the baby " (species-of b) " is taking " (if (malep b) "his" "her") " first steps." :% :%
                   "Thickly Padded " (species-of b) ": Shut it. Hey, isn't that an intruder?" :% :%
                   "Padded " (species-of a) ": Apparently." :% :%)
              (set-new-battle `((yadfa-enemies:diaper-pirate . (list :level (random-from-range 2 5)
                                                                     :species ,(species-of a)
                                                                     :male ,(malep a)))
                                (yadfa-enemies:thickly-diaper-pirate . (list :level (random-from-range 2 5)
                                                                             :species ,(species-of b)
                                                                             :male ,(malep b))))))))
(defevent pirates-cove-2
  :lambda (lambda (self)
            (declare (ignorable self))
            (let* ((a (make-instance 'yadfa-enemies:diaper-pirate))
                   (coon nil))
              (out "You find the diapered raccoon bandit back from Navy HQ in a pillory wearing nothing but 2 layers of thoroughly flooded and messy diapers" :% :%
                   "Diapered Raccoon Bandit: OK! I learned my lesson! Can I please get a diaper change now?!?!? >///<" :% :%
                   "*one of the " (species-of a)"s changes the outer layer of his diaper but still leaves him in the flooded and messy inner layer*" :% :%
                   "Diapered Raccoon Bandit: That's not what I meant!!!" :% :%
                   (species-of a) ": You're still being punished for giving out location away. So you're going to stay wet and messy for the rest of your life." :% :%
                   "*The " (species-of a) " decides to spank the raccoon through his messy padding for amusement. Judging from the face the raccoon is making, he doesn't like being mushed and humiliated like that*" :% :%
                   "*You decide to take sympathy on the poor raccoon and rescue him. You knock the " (species-of a) " out while " (if (malep a) "he" "she") "'s distracted from abusing " (if (malep a) "his" "her") " former ally. Then release the raccoon from the stocks and hand him his tunic to help preserve whatever dignity he has left" :% :%
                   "Raccoon: umm, thanks." :% :%
                   (name-of (player-of *game*)) ": got a name?" :% :%)
              (finish-output)
              (accept-with-frame-resolved
                (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
                  (setf coon (clim:accept 'string :stream *query-io* :prompt "Raccoon Name"
                                                  :default #.(second
                                                              (assoc :name
                                                                     (progn
                                                                       (c2mop:ensure-finalized
                                                                        (find-class 'yadfa-allies:slynk))
                                                                       (c2mop:compute-default-initargs
                                                                        (find-class 'yadfa-allies:slynk)))))
                                                  :view clim:+text-field-view+))))
              (out "Raccoon: It's " (name-of coon) :% :%
                   (name-of coon) " decides you can't be all bad since you're the first one to be nice to him (plus the Raccoon Bandits abandoned him) and decides to join your team" :% :%)
              (do-push coon (team-of *game*) (allies-of *game*))
              (when (>= (bladder/contents-of coon) (bladder/need-to-potty-limit-of coon))
                (out "*" (name-of coon) " grabs the front of his diaper*" :% :%
                     (name-of (player-of *game*)) ": You don't have to go again do you?" :% :%
                     "*" (name-of coon))
                (if (>= (bladder/contents-of coon) (bladder/potty-dance-limit-of coon))
                    (out " starts hopping around while holding the front of " (if (malep coon) "his" "her") " diaper*")
                    (out " takes " (if (malep coon) "his" "her") " hands off " (if (malep coon) "his" "her") " diaper, fidgets a little and blushes*"))
                (out :% :%
                     (name-of coon) ": No, of course not. I'm not some baby who needs to ask to go to the bathroom all the time" :% :%
                     "*" (name-of coon) " seems too embarrassed to admit when he has to use the toilet. He might change his mind if he gets desperate enough" :% :%)))))
