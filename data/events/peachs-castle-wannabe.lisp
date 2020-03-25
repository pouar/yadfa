;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent got-all-shine-stars-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "You got a MacGuffin in exchange for 5 Shine Stars, yay." :% :%)
            (push (make-instance 'yadfa-items:macguffin) (inventory-of (player-of *game*)))
            (removef-if (inventory-of (player-of *game*))
                        (lambda (e)
                          (typep e 'yadfa-items:shine-star))
                        :count 5)))
(defevent enter-race-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Welcome to the Race Area. To get the star in this area, you're gonna have to race against the enemy and win, but since Pouar can't code AIs of racers worth a shit, the enemy you're racing against is going to be that truck from Big Rigs: Over The Road Racing. You know, the one that doesn't move. Get ready for some brake jammin', CB talkin', convoy rolling action across America. And by that, I mean moving one zone to the north." :% :%)))
(defevent win-race-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Congrats, you won the race, also the star is just sitting here at the end, since that Truck isn't going to bring it to you." :% :%)
            (push (make-instance 'yadfa-items:shine-star) (inventory-of (player-of *game*)))))
(defevent enter-thwomp-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "You see the Shine Star in the zone to the north and a Thwomp in the middle of the zone" :% :%
                 "Thwomp: You will never get past me, because once you stand under me, you're a gonner." :% :%)))
(defevent win-thwomp-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Thwomp: HEY!!! NO FAIR!!!! YOU KNOW I CAN ONLY MOVE UP AND DOWN!!!!!!!" :% :%
                 "You collect the star" :% :%)
            (push (make-instance 'yadfa-items:shine-star) (inventory-of (player-of *game*)))))
(defevent enter-pokemon-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "You find a Pokémon trainer" :% :%
                 "Pokémon trainer: OK, Bowser has paid me to guard this star, so I'm going to fight you with all my might, with my level 2 Magikarp" :% :%)
            (set-new-battle '((yadfa-enemies:magikarp . (list :level 2)))
                            :continuable t
                            :enter-battle-text "Pokémon trainer wants to battle. Pokémon trainer sent out Magikarp"
                            :win-events '(win-pokemon-area-1))))
(defevent win-pokemon-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Pokémon trainer: What?!?! I can't believe you beat my level 2 Magikarp!!!! You must be like, Mewtwo level of strong!!!!" :% :%
                 (name-of (player-of *game*)) " got a shine star for winning" :% :%)
            (push (make-instance 'yadfa-items:shine-star) (inventory-of (player-of *game*)))))
(defevent enter-blank-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Seems Bowser decided to leave the shine star in this area in the middle of the level with no boss like an idiot" :% :%)
            (out (name-of (player-of *game*)) " gets a shine star")
            (push (make-instance 'yadfa-items:shine-star) (inventory-of (player-of *game*)))))
(defevent enter-eggman-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "Eggman: Bowser has paid me a ton of money to protect this shine star, So I'm going to use the same robot I used in Sonic Heroes since it worked so beautifully, an Egg Pawn with an Egg Spear that simply charges at you." :% :%)
            (set-new-battle '((yadfa-enemies:egg-pawn . (list :level 1 :wield (make-instance 'yadfa-items:egg-spear))))
                            :continuable t
                            :enter-battle-text "Eggman sends out a Egg Pawn wielding a L̶a̶n̶c̶e̶ \"Egg Spear\""
                            :win-events '(win-eggman-area-1))))
(defevent win-eggman-area-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "I can't believe my crappy robot got destroyed again." :% :%
                 (name-of (player-of *game*)) " gets a shine star" :% :%)
            (push (make-instance 'yadfa-items:shine-star) (inventory-of (player-of *game*)))))
