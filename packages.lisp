;;;; -*- mode: Common-Lisp; sly-buffer-package: "common-lisp-user"; coding: utf-8-unix; -*-
(in-package :cl-user)
(uiop:define-package :yadfa-util
  (:use :cl :iterate)
  (:export
   #:shl
   #:shr
   #:lambda-list
   #:do-push
   #:remove-nth
   #:insert
   #:insertf
   #:substitutef
   #:type-specifier
   #:coerced-function
   #:removef-if
   #:deletef-if
   #:append*
   #:appendf*
   #:collecting*
   #:summing*
   #:in*
   #:sum*
   #:defunassert
   #:lappendf
   #:random-from-range
   #:print-unreadable-object-with-prefix)
  (:documentation "Utility functions that aren't really part of the game's API")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands)))
(uiop:define-package :yadfa
  (:use :cl :yadfa-util :iterate)
  (:export
   ;;variables
   #:*battle*
   #:*game*
   #:*cheat-hooks*
   #:*battle-packages*
   #:*world-packages*
   #:*command-packages*
   ;;structures
   #:event
   ;;conditions
   #:onesie-too-thick
   #:onesie-locked
   #:invalid-user-input
   ;;macros
   #:defevent
   #:ensure-zone
   #:defzone
   #:ensure-zone*
   #:defzone*
   #:defonesie
   #:make-pocket-zone
   #:accept-with-effective-frame
   #:with-effective-frame
   #:present-with-effective-frame
   #:updating-present-with-effective-frame
   #:defmatch
   #:define-type
   ;;functions
   #:finished-events
   #:unfinished-events
   #:finish-events
   #:process-battle
   #:get-positions-of-type
   #:trigger-event
   #:intro-function
   #:set-player
   #:set-status-condition
   #:set-new-battle
   #:get-inventory-list
   #:get-zone
   #:get-destination
   #:get-path-end
   #:getf-direction
   #:remf-direction
   #:get-diaper-expansion
   #:pop-from-expansion
   #:getf-action-from-prop
   #:filter-items
   #:total-thickness
   #:thickest-sort
   #:thickest
   #:wet
   #:mess
   #:get-event
   #:move-to-pocket-map
   #:move-to-secret-underground
   #:get-warp-point
   #:get-props-from-zone
   #:get-items-from-prop
   #:get-bitcoins-from-prop
   #:effective-type-effectiveness
   #:calculate-diaper-usage
   #:calculate-diaper-usage*
   #:calculate-level-to-exp
   #:calculate-exp-yield
   #:calculate-wear-stats
   #:calculate-wield-stats
   #:calculate-stat-delta
   #:calculate-stat-multiplier
   #:calculate-stat
   #:wash
   #:go-to-sleep
   #:shopfun
   #:ally-join
   #:pushnewmove
   #:get-move
   #:process-potty-dance-check
   #:clear-configuration-hook
   ;;methods
   #:describe-diaper-wear-usage
   #:describe-diaper-inventory-usage
   #:get-process-potty-action-type
   #:output-process-potty-text
   #:get-babyish-padding
   #:resolve-enemy-spawn-list
   #:process-battle-accident
   #:process-potty-dance
   #:calculate-damage
   #:event-attributes
   #:battle-script
   #:condition-script
   #:attack
   #:default-attack
   #:use-script
   #:wear-script
   #:wield-script
   #:toggle-onesie
   #:type-match
   #:coerce-element-type
   #:coerce-element-types
   #:fill-bladder
   #:fill-bowels
   #:bladder/fill-rate
   #:bowels/fill-rate
   #:fart
   #:fart-result-text
   ;;constructors
   #:make-action
   ;;classes
   #:status-condition
   #:stats-view
   #:base-character
   #:bladder-character
   #:bowels-character
   #:potty-character
   #:npc
   #:team-member
   #:potty-trained-team-member
   #:element-type
   #:ally
   #:ally-rebel
   #:ally-no-potty-training
   #:ally-rebel-potty-training
   #:ally-silent-potty-training
   #:ally-last-minute-potty-training
   #:ally-feral
   #:playable-ally
   #:player
   #:zone
   #:move
   #:buff
   #:debuff
   #:clear-status-mixin
   #:health-inc-move
   #:energy-inc-move
   #:damage-move
   #:damage-item
   #:damage-wield
   #:mess-move-mixin
   #:wet-move-mixin
   #:prop
   #:placable-prop
   #:item
   #:consumable
   #:ammo
   #:weapon
   #:clothing
   #:top
   #:headpiece
   #:bottoms
   #:snap-bottoms
   #:closed-bottoms
   #:full-outfit
   #:closed-full-outfit
   #:onesie
   #:onesie/opened
   #:onesie/closed
   #:incontinence-product
   #:padding
   #:ab-clothing
   #:undies
   #:stuffer
   #:diaper
   #:pullup
   #:skirt
   #:dress
   #:shirt
   #:pants
   #:enemy
   #:bladder-enemy
   #:bowels-enemy
   #:potty-enemy
   #:battle
   #:pantsable-character
   ;;accessors
   #:name-of
   #:description-of
   #:attributes-of
   #:direction-attributes-of
   #:target-of
   #:tail-of
   #:wings-of
   #:skin-of
   #:config-of
   #:stairs-of
   #:element-types-of
   #:last-process-potty-time-of
   #:statuses-cleared-of
   #:blocks-turn-of
   #:duration-of
   #:stat-delta-of
   #:stat-multiplier-of
   #:priority-of
   #:health-of
   #:energy-of
   #:level-of
   #:malep
   #:wear-of
   #:species-of
   #:time-of
   #:accumulative-of
   #:bladder/contents-of
   #:bladder/fill-rate-of
   #:bladder/need-to-potty-limit-of
   #:bladder/potty-dance-limit-of
   #:bladder/potty-desperate-limit-of
   #:bladder/maximum-limit-of
   #:bowels/contents-of
   #:bowels/fill-rate-of
   #:bowels/need-to-potty-limit-of
   #:bowels/potty-dance-limit-of
   #:bowels/potty-desperate-limit-of
   #:bowels/maximum-limit-of
   #:fart-count-of
   #:moves-of
   #:exp-of
   #:user-of
   #:clothes-of
   #:base-stats-of
   #:team-npcs-of
   #:iv-stats-of
   #:bitcoins-of
   #:bitcoins-per-level-of
   #:inventory-of
   #:wield-of
   #:learned-moves-of
   #:position-of
   #:warp-on-death-point-of
   #:enter-text-of
   #:props-of
   #:events-of
   #:continue-battle-of
   #:underwaterp
   #:hiddenp
   #:warp-points-of
   #:lockedp
   #:sellablep
   #:tossablep
   #:placeablep
   #:can-potty-p
   #:potty-trigger-of
   #:wet-text-of
   #:mess-text-of
   #:wear-wet-text-of
   #:wear-mess-text-of
   #:must-wear-of
   #:must-wear*-of
   #:must-not-wear-of
   #:must-not-wear*-of
   #:no-wetting/messing-of
   #:enemy-spawn-list-of
   #:team-npc-spawn-list-of
   #:energy-cost-of
   #:power-of
   #:use-power-of
   #:reload-count-of
   #:ammo-power-of
   #:ammo-of
   #:ammo-type-of
   #:ammo-capacity-of
   #:cant-use-p
   #:items-of
   #:actions-of
   #:plural-name-of
   #:value-of
   #:default-attack-power-of
   #:wear-stats-of
   #:wield-stats-of
   #:special-actions-of
   #:thickness-of
   #:thickness-capacity-of
   #:thickness-capacity-threshold-of
   #:waterproofp
   #:leakproofp
   #:disposablep
   #:sogginess-of
   #:sogginess-capacity-of
   #:messiness-of
   #:messiness-capacity-of
   #:key-of
   #:onesie-thickness-capacity-of
   #:onesie-thickness-capacity-threshold-of
   #:onesie-waterproof-p
   #:watersport-limit-of
   #:mudsport-limit-of
   #:watersport-chance-of
   #:mudsport-chance-of
   #:turn-queue-of
   #:enter-battle-text-of
   #:enemies-of
   #:win-events-of
   #:player-of
   #:allies-of
   #:team-of
   #:events-of
   #:finished-events-of
   #:current-events-of
   #:seen-enemies-of
   #:action-lambda
   #:action-p
   #:fainted-of
   #:curablep
   #:deletef-status-conditions
   #:deletef-status-conditions-if
   #:status-conditions)
  (:documentation "Yet Another Diaperfur Adventure")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars) (:sc :serapeum/contrib/hooks)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:ms :marshal) (:f :fmt)))
(uiop:define-package :yadfa-bin
  (:export #:lst
           #:wear
           #:unwear
           #:get-stats
           #:toggle-onesie
           #:toss
           #:toggle-full-repl
           #:wield
           #:unwiled
           #:pokedex
           #:toggle-lock
           #:change
           #:wield
           #:unwield
           #:enable-mods
           #:disable-mods
           #:reload-files
           #:get-inventory-of-type)
  (:documentation "Commands that the player can run anytime"))
(uiop:define-package :yadfa-world
  (:export #:move
           #:interact
           #:save-game
           #:load-game
           #:go-potty
           #:tickle
           #:wash-all-in
           #:use-item
           #:add-ally-to-team
           #:remove-ally-from-team
           #:swap-team-member
           #:stats
           #:place
           #:reload
           #:place-prop
           #:take-prop
           #:fart)
  (:documentation "contains the commands when in the open world (assuming that's what it's called) (and not in something like a battle). The player probably shouldn't call these with the package prefix unless they're developing"))
(uiop:define-package :yadfa-battle
  (:export #:fight
           #:run
           #:use-item
           #:stats
           #:reload)
  (:documentation "Contains the commands used when battling. The player probably shouldn't call these with the package prefix unless they're developing"))
(uiop:define-package :yadfa-moves
  (:shadow #:pants)
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:kamehameha
   #:superglitch
   #:watersport
   #:mudsport
   #:tickle
   #:tackle
   #:mush
   #:mudbomb
   #:spray
   #:pants
   #:fire-breath
   #:roar
   #:scratch
   #:face-sit
   #:ghost-tickle
   #:ghost-squish
   #:ghost-mush
   #:bite
   #:teleporting-flood
   #:teleporting-mess
   #:boop
   #:fart
   #:spank)
  (:documentation "Contains all the moves in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-items
  (:shadow #:dress #:onesie #:diaper #:onesie/opened #:onesie/closed #:skirt)
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:bandit-swimsuit/closed
   #:revive
   #:shine-star
   #:egg-spear
   #:pacifier
   #:gold-pacifier
   #:recovering-pacifier
   #:healing-pacifier
   #:energizing-pacifier
   #:blanket
   #:plushie
   #:short-dress
   #:dress
   #:sundress
   #:fursuit
   #:watertight-fursuit
   #:koopa-shell
   #:cheerleader-outfit
   #:ballerina-dress
   #:braixen-dress
   #:skirt
   #:denim-skirt
   #:shendyt
   #:kalasiris
   #:toddler-dress
   #:knights-armor
   #:tshirt
   #:jeans
   #:snap-jeans
   #:baggy-jeans
   #:cannibal-corpse-tshirt
   #:black-leather-jacket
   #:orca-suit
   #:stretchable-orca-suit
   #:orca-suit-lite
   #:stretchable-orca-suit-lite
   #:boxers
   #:panties
   #:mutagen
   #:antimutagen
   #:bra
   #:bikini-top
   #:tunic
   #:bandit-uniform-tunic
   #:bandit-uniform-shirt
   #:bandit-uniform-sports-bikini-top
   #:bottle-of-milk
   #:monster-energy-drink
   #:spiked-bottle-of-milk
   #:potion
   #:cannibal-corp-meat
   #:maximum-tomato
   #:holy-hand-grenade
   #:generic-diapers
   #:generic-diapers-package
   #:generic-pullons
   #:generic-pullons-package
   #:incontinence-pad
   #:incontinence-pad-package
   #:cloth-incontinence-pad
   #:diaper
   #:high-capacity-diaper
   #:black-diaper
   #:cloth-diaper
   #:diaper-package-mixin
   #:ammo-box-mixin
   #:diaper-package
   #:midnight-diaper
   #:midnight-diaper-package
   #:kurikia-thick-diaper
   #:thick-cloth-diaper
   #:thick-diaper
   #:gem-diaper
   #:infinity-diaper
   #:temple-diaper
   #:cursed-diaper
   #:temple-pullups
   #:thick-diaper-package
   #:kurikia-thick-rubber-diaper
   #:kurikia-thick-cloth-diaper
   #:thick-rubber-diaper
   #:hyper-thick-diaper
   #:hyper-thick-cloth-diaper
   #:hyper-thick-rubber-diaper
   #:pullups
   #:pullups-package
   #:cloth-pullups
   #:rubber-pullups
   #:swim-diaper-cover
   #:disposable-swim-diaper
   #:disposable-swim-diaper-package
   #:rubber-diaper
   #:bandit-diaper
   #:bandit-adjustable-diaper
   #:bandit-female-diaper
   #:bandit-swim-diaper-cover
   #:lower-bandit-swim-diaper-cover
   #:female-bandit-swim-diaper-cover
   #:pink-frilly-diaper
   #:magic-diaper-key
   #:gold-bar
   #:gem
   #:gold-collar
   #:diaper-corset
   #:blackjack-uniform-diaper
   #:cloth-diaper-corset
   #:rubber-diaper-corset
   #:collar
   #:rubber-suit
   #:magic-diaper-key
   #:lil-koopalings
   #:lil-koopalings-package
   #:ak47
   #:7.62×39mm
   #:box-of-7.62×39mm
   #:exterminator
   #:exterminator-ammo
   #:pink-sword
   #:hammer-gun
   #:messing-laser
   #:wrench
   #:three-swords
   #:pocket-map-machine
   #:warp-device
   #:navy-shirt
   #:navy-pants
   #:navy-skirt
   #:navy-pullups
   #:pirate-dress
   #:pirate-shirt
   #:macguffin
   #:itemfinder
   #:enemy-catcher
   #:ghost-catcher
   #:contained-enemies-of
   #:contained-enemies-max-length-of
   #:catch-chance-multiplier-of
   #:catch-chance-delta-of
   #:contained-enemies
   #:contained-enemies-max-length
   #:catch-chance-multiplier
   #:catch-chance-delta
   #:device-health
   #:max-device-health
   #:device-health-of
   #:max-device-health-of)
  (:documentation "Contains all the items in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-battle-commands
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:catch-enemy)
  (:documentation "convenience functions for battle")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-world-commands
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:loot-caught-enemies
   #:disown-adopted-enemies)
  (:documentation "convenience functions for battle")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-element-types
  (:use :cl :yadfa :yadfa-util :iterate)
  (:export #:normal
           #:fighting
           #:flying
           #:poison
           #:ground
           #:rock
           #:bug
           #:ghost
           #:steel
           #:fire
           #:water
           #:grass
           #:electric
           #:psychic
           #:ice
           #:dragon
           #:dark
           #:fairy
           #:abdl)
  (:documentation "Element types")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-enemies
  (:use :cl :yadfa :yadfa-util :iterate)
  (:export
   #:magikarp
   #:egg-pawn
   #:diapered-raccoon-bandit
   #:catchable-raccoon-bandit
   #:rookie-diapered-raccoon-bandit
   #:female-diapered-raccoon-bandit
   #:giant-diapered-raccoon-bandit
   #:navy-officer
   #:navy-officer*
   #:diaper-pirate
   #:diapered-kobold
   #:diapered-skunk
   #:diapered-skunk*
   #:thickly-diaper-pirate
   #:padded-fursuiter-servant
   #:fursuiter-servant
   #:diapered-dragon*
   #:diapered-dragon
   #:dergy
   #:ghost
   #:werewolf
   #:domesticated-werewolf
   #:catchable-enemy
   #:catch-chance
   #:catch-chance-rate-of
   #:raptor
   #:change-class-target
   #:change-class-text
   #:adoptable-enemy
   #:skunk-boop-mixin)
  (:documentation "Contains all the enemies in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-props
  (:use :cl :yadfa :yadfa-util :iterate)
  (:export
   #:toilet
   #:washer
   #:automatic-changing-table
   #:checkpoint
   #:shop
   #:vending-machine
   #:debug-shop
   #:bed
   #:placable-bed
   #:placable-toilet
   #:placable-washer
   #:pet-bed
   #:training-potty
   #:items-for-sale-of
   #:items-for-sale)
  (:documentation "Contains all the enemies in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-status-conditions
  (:use :cl :yadfa :yadfa-util :iterate)
  (:export
   #:wetting
   #:messing
   #:mushed
   #:laughing
   #:skunked
   #:pantsed
   #:poisoned)
  (:documentation "Contains all the status conditions in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-zones
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:peachs-castle-wannabe
   #:painting
   #:back-to-castle
   #:race-area
   #:thwomp-area
   #:home
   #:debug-map
   #:bandits-domain
   #:lukurbo
   #:ironside
   #:silver-cape
   #:bandits-way
   #:cave-entrance
   #:descend
   #:bandits-entrance
   #:secret-underground
   #:pirates-cove
   #:candle-carnival
   #:sky-base
   #:star-city
   #:flying-mansion
   #:your-ship
   #:rpgmaker-dungeon
   #:haunted-house
   #:haunted-forest
   #:rocket
   #:rainbow-slide
   #:pyramid)
  (:documentation "Contains all the zone definitions in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-events
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:enter-bandits-village-1
   #:enter-bandits-shop-1
   #:enter-bandits-shop-2
   #:enter-bandits-shop-3
   #:decend-bandits-cave-1
   #:obtain-diaper-lock-1
   #:enter-bandits-kennel-1
   #:get-warp-pipe-summoner-1
   #:shopkeeper-floods-himself-1
   #:pointless-quest-1
   #:ironside-university-joke-1
   #:enter-lukurbo-1
   #:got-all-shine-stars-1
   #:enter-race-area-1
   #:win-race-area-1
   #:enter-thwomp-area-1
   #:win-thwomp-area-1
   #:enter-pokemon-area-1
   #:win-pokemon-area-1
   #:enter-blank-area-1
   #:enter-eggman-area-1
   #:win-eggman-area-1
   #:pirates-cove-1
   #:pirates-cove-2
   #:secret-underground-pipe-rpgmaker-dungeon
   #:secret-underground-pipe-lukurbo
   #:secret-underground-pipe-silver-cape
   #:secret-underground-pipe-haunted-forest
   #:secret-underground-pipe-haunted-house
   #:secret-underground-pipe-candle-carnival
   #:secret-underground-pipe-sky-base
   #:secret-underground-pipe-star-city
   #:enter-silver-cape-1
   #:obtain-pirate-ship-1
   #:get-location-to-pirate-cove-1
   #:get-diaper-locked-1
   #:pyramid-puzzle-1
   #:infinity-diaper-obtained-1)
  (:documentation "Contains all the event definitions in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-allies
  (:use :yadfa :yadfa-util :cl :iterate)
  (:export
   #:slynk
   #:chris
   #:kristy
   #:furry
   #:raptor
   #:diapered-kobold
   #:adopted-enemy
   #:diapered-raccoon-bandit
   #:found-raccoon-bandit)
  (:documentation "Contains all the allies in the game")
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-user
  (:use :cl :iterate)
  (:documentation "The package that the player typically executes commands from"))
(uiop:define-package :yadfa-clim
  (:use :iterate :yadfa-util :yadfa :clim-lisp)
  (:documentation "CLIM related stuff")
  (:export #:stat-view #:+stat-view+ #:draw-bar #:run-listener)
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-blackjack
  (:use :iterate :yadfa-util :yadfa :clim-lisp)
  (:export #:run-game)
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-pyramid
  (:use :iterate :yadfa-util :yadfa :clim-lisp)
  (:export #:run-game #:stat-view #:+stat-view+ #:process-potty)
  (:shadow #:area)
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
(uiop:define-package :yadfa-puzzle
  (:use :iterate :yadfa-util :yadfa :clim-lisp :yadfa-pyramid)
  (:export #:run-game)
  (:shadow #:run-game)
  (:local-nicknames (:s :serapeum) (:a :alexandria) (:u :ugly-tiny-infix-macro) (:g :global-vars)
                    (:c :clim) (:ce :clim-extensions) (:cc :conditional-commands) (:f :fmt)))
