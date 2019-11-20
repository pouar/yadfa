;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 peachs-castle-wannabe)
  :name "Castle Entrance"
  :description "The entrance to some crappy version of Peach's Castle"
  :enter-text "You're at the castle entrance"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :warp-points (list 'silver-cape '(6 11 0 silver-cape)))
(ensure-zone (0 -1 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wondering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :props (list
          :princess (make-instance
                     'prop
                     :name "Princess Toadstool^[^?Daisy^[^?Peach"
                     :description "The princess of this castle"
                     :actions (list
                               :talk (make-action
                                      :documentation "Talk to the princess"
                                      :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                (declare (ignore prop keys))
                                                (cond ((finished-events '(yadfa-events:got-all-shine-stars-1))
                                                       (write-line "Peach: You got all the Shine Stars"))
                                                      ((<= (list-length (filter-items (inventory-of (player-of *game*)) 'yadfa-items:shine-star)) 0)
                                                       (write-line "Peach: The Shine Stars keep peace and harmony throughout the land, but Bowser has stolen them and is causing discord and chaos and you need to go find them and bring them back")
                                                       (format nil "~a: Seriously? Is that the best plot Pouar could possibly come up with for this quest?"
                                                               (name-of (player-of *game*))))
                                                      ((< (list-length (filter-items (inventory-of (player-of *game*)) 'yadfa-items:shine-star)) 5)
                                                       (format nil
                                                               "Peach: You still have ~d shine stars to collect, hurry before the player gets bored of this stupid quest."
                                                               (- 5 (list-length (filter-items (inventory-of (player-of *game*)) 'yadfa-items:shine-star)))))
                                                      (t
                                                       (write-line "Peach: You got all the Shine Stars, have a MacGuffin for putting up with this crappy quest")
                                                       (trigger-event 'yadfa-events:got-all-shine-stars-1)))))))))
(ensure-zone (0 -2 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :up t)
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (0 -2 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :down t))
(ensure-zone (0 -3 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (-1 -3 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (1 -3 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (-1 -4 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :direction-attributes (list :east (list :hidden t)
                              'painting (list :exit-text "BaBaBa-BaBa-Ba Letsago"))
  :warp-points (list 'painting '(0 0 0 peachs-castle-wannabe:pokemon-area)))
(ensure-zone (1 -4 1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :direction-attributes (list :west (list :hidden t)
                              'painting (list :exit-text "BaBaBa-BaBa-Ba Letsago"))
  :warp-points (list 'painting '(0 0 0 peachs-castle-wannabe:blank-area)))
(ensure-zone (-1 -1 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (1 -1 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (-1 -2 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :down t)
  :direction-attributes (list :west (list :hidden t)
                              :east (list :hidden t)))
(ensure-zone (1 -2 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :down t)
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-1 -2 -1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :up t))
(ensure-zone (1 -2 -1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :up t))
(ensure-zone (-2 -1 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (2 -1 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe)
(ensure-zone (-2 -2 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :direction-attributes (list :east (list :hidden t)
                              'painting (list :exit-text "BaBaBa-BaBa-Ba Letsago"))
  :warp-points (list 'painting '(0 0 0 peachs-castle-wannabe:race-area)))
(ensure-zone (2 -2 0 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :direction-attributes (list :west (list :hidden t)
                              'painting (list :exit-text "BaBaBa-BaBa-Ba Letsago"))
  :warp-points (list 'painting '(0 0 0 peachs-castle-wannabe:thwomp-area)))
(ensure-zone (0 -2 -1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :down t)
  :props (list
          :sign (make-instance
                     'prop
                     :name "Sign"
                     :description "Notice: Due to a koopa making a mess on the floor because we can't be fucked to install toilets, all koopas will be required to wear these leak proof shells because they're cheaper than diapers and hold a lot more. They will be allowed to the underground pipes once a month for emptying. The princess and Bowser on the other hand will be allowed to wear diapers because as royalty we deserve to live in comfort dammit.")))
(ensure-zone (0 -1 -1 peachs-castle-wannabe)
  :name "Castle Hallway"
  :description "Some crappy version of Peach's Castle"
  :enter-text "You're wandering around the castle"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :stairs (list :down t)
  :direction-attributes (list 'painting (list :exit-text "BaBaBa-BaBa-Ba Letsago"))
  :warp-points (list 'painting '(0 0 0 peachs-castle-wannabe:eggman-area)))
(ensure-zone (0 0 0 peachs-castle-wannabe:race-area)
  :name "Race Area Map"
  :description "Race Area Map"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:enter-race-area-1)
  :props (list
          :truck (make-instance 'prop
                                :name "Truck"
                                :description "It's that truck from Big Rigs: Over The Road Racing. Stop expecting it to move, it's never going to happen.")))
(ensure-zone (0 -1 0 peachs-castle-wannabe:race-area)
  :name "Race Area"
  :description "Race Area"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:win-race-area-1)
  :warp-points (list 'back-to-castle '(-2 -2 0 peachs-castle-wannabe))
  :props (list
          :truck (make-instance 'prop
                                :name "Truck"
                                :description "It's that truck from Big Rigs: Over The Road Racing. Stop expecting it to move, it's never going to happen.")))
(ensure-zone (0 0 0 peachs-castle-wannabe:thwomp-area)
  :name "Thwomp Area"
  :description "Thwomp Area"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:enter-thwomp-area-1))
(ensure-zone (0 -1 0 peachs-castle-wannabe:thwomp-area)
  :name "Thwomp Area"
  :description "Thwomp Area"
  :enter-text "You walk around the thwomp and move to the north"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :warp-points (list 'back-to-castle '(2 -2 0 peachs-castle-wannabe))
  :events '(yadfa-events:win-thwomp-area-1))
(ensure-zone (0 0 0 peachs-castle-wannabe:pokemon-area)
  :name "Pokémon Area"
  :description "Pokémon Area"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:enter-pokemon-area-1)
  :warp-points (list 'back-to-castle '(-1 -4 1 peachs-castle-wannabe)))
(ensure-zone (0 0 0 peachs-castle-wannabe:blank-area)
  :name "Pokémon Area"
  :description "Pokémon Area"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:enter-blank-area-1)
  :warp-points (list 'back-to-castle '(1 -4 1 peachs-castle-wannabe)))
(ensure-zone (0 0 0 peachs-castle-wannabe:eggman-area)
  :name "Eggman Area"
  :description "Eggman Area"
  :enter-text "You're wandering around the level"
  :can-potty 'can-potty-peachs-castle-wannabe
  :potty-trigger 'potty-trigger-peachs-castle-wannabe
  :events '(yadfa-events:enter-eggman-area-1)
  :warp-points (list 'back-to-castle '(0 -1 -1 peachs-castle-wannabe)))
