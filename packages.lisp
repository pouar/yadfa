(in-package :cl-user)
(uiop:define-package #:yadfa
    (:use #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:import-from #:macro-level #:macro-level)
    (:export
        ;;variables
        #:*battle*
        #:*game*
        ;;macros
        #:defevent
        #:ensure-zone
        #:defonesie
        #:make-pocket-zone
        #:do-push
        ;;functions
        #:intro-function
        #:set-player
        #:prompt-for-values
        #:color-format
        #:set-status-condition
        #:set-new-battle
        #:get-inventory-list
        #:get-zone
        #:get-destination
        #:get-path-end
        #:getf-direction
        #:setf-direction
        #:remf-direction
        #:getf-action-from-prop
        #:wearingp
        #:thickest
        #:total-thickness
        #:thickest-sort
        #:wet
        #:mess
        #:get-event
        #:random-from-range
        #:move-to-pocket-map
        #:move-to-secret-underground
        #:get-warp-point
        #:get-props-from-zone
        #:get-items-from-prop
        #:get-bitcoins-from-prop
        #:calculate-diaper-usage
        #:calculate-level-to-exp
        #:calculate-exp-yield
        #:calculate-wear-stats
        #:calculate-wield-stats
        #:calculate-stat-delta
        #:calculate-stat-multiplier
        #:calculate-stat
        #:calculate-damage
        #:wash
        #:go-to-sleep
        #:shopfun
        #:ally-join
        #:pushnewmove
        #:get-move
        ;;constructors
        #:make-action
        ;;classes
        #:status-condition
        #:base-character
        #:team-member
        #:ally
        #:player
        #:zone
        #:stat/move
        #:prop
        #:item
        #:consumable
        #:clothing
        #:top
        #:headpiece
        #:bottoms
        #:closed-bottoms
        #:full-outfit
        #:closed-pants
        #:closed-full-outfit
        #:onesie
        #:onesie/opened
        #:onesie/closed
        #:incontinence-product
        #:padding
        #:cub-undies
        #:pullon
        #:tabbed-briefs
        #:incontinence-pad
        #:undies
        #:stuffer
        #:diaper
        #:pullup
        #:skirt
        #:dress
        #:shirt
        #:pants
        #:toilet
        #:washer
        #:automatic-changing-table
        #:checkpoint
        #:shop
        #:debug-shop
        #:bed
        #:npc
        #:potty-npc
        #:battle
        ;;accessors
        #:name-of
        #:description-of
        #:attributes-of
        #:direction-attributes-of
        #:target-of
        #:battle-script-of
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
        #:bladder/contents-of
        #:bladder/fill-rate-of
        #:bladder/need-to-potty-limit-of
        #:bladder/potty-dance-limit-of
        #:bladder/maximum-limit-of
        #:bowels/contents-of
        #:bowels/fill-rate-of
        #:bowels/need-to-potty-limit-of
        #:bowels/potty-dance-limit-of
        #:bowels/maximum-limit-of
        #:moves-of
        #:exp-of
        #:base-stats-of
        #:iv-stats-of
        #:bitcoins-of
        #:bitcoins-per-level-of
        #:inventory-of
        #:wield-of
        #:learned-moves-of
        #:potty-training-of
        #:position-of
        #:warp-on-death-point-of
        #:enter-text-of
        #:props-of
        #:events-of
        #:continue-battle-of
        #:underwaterp
        #:hiddenp
        #:warp-points-of
        #:diapers-only-p
        #:lockedp
        #:sellablep
        #:tossablep
        #:placeablep
        #:no-puddles-p
        #:enemy-spawn-list-of
        #:energy-cost-of
        #:power-of
        #:ai-flags-of
        #:attack-of
        #:items-of
        #:actions-of
        #:plural-name-of
        #:value-of
        #:default-move-of
        #:wear-stats-of
        #:wield-stats-of
        #:special-actions-of
        #:use-script-of
        #:wield-script-of
        #:wear-script-of
        #:thickness-of
        #:thickness-capacity-of
        #:waterproofp
        #:disposablep
        #:sogginess-of
        #:sogginess-capacity-of
        #:messiness-of
        #:messiness-capacity-of
        #:key-of
        #:onesie-thickness-capacity-of
        #:onesie-waterproof-p
        #:items-for-sale-of
        #:watersport-limit-of
        #:mudsport-limit-of
        #:watersport-chance-of
        #:mudsport-chance-of
        #:current-turn-list-of
        #:enter-battle-text-of
        #:enemies-of
        #:win-events-of
        #:status-conditions-of
        #:zones-of
        #:player-of
        #:allies-of
        #:team-of
        #:events-of
        #:finished-events-of
        #:seen-enemies-of
        #:action-lambda
        #:action-documentation
        #:event-lambda
        #:event-repeatable)
    (:documentation "Yet Another Diaperfur Adventure"))
(uiop:define-package #:yadfa/bin
    (:export #:lst #:wear #:unwear #:get-stats #:toggle-onesie #:toss #:toggle-full-repl #:wield #:unwiled #:pokedex #:toggle-lock #:change #:wield #:unwield #:enable-mod #:disable-mod #:reload-files)
    (:documentation "Commands that the player can run anytime"))
(uiop:define-package #:yadfa/world
    (:export #:move #:interact #:save-game #:load-game #:go-potty #:tickle #:wash-all-in #:use-item #:add-ally-to-team #:remove-ally-from-team #:swap-team-member #:stats #:place)
    (:documentation "contains the commands when in the open world (assuming that's what it's called) (and not in something like a battle). The player probably shouldn't call these with the package prefix unless they're developing"))
(uiop:define-package #:yadfa/battle
    (:export #:fight #:run #:use-item #:stats)
    (:documentation "Contains the commands used when battling. The player probably shouldn't call these with the package prefix unless they're developing"))
(uiop:define-package #:yadfa/moves
    (:import-from #:macro-level #:macro-level)
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
        #:kamehameha
        #:superglitch
        #:watersport
        #:mudsport
        #:tickle
        #:tackle
        #:default
        #:weapon-default
        #:mush)
    (:documentation "Contains all the moves in the game"))
(uiop:define-package #:yadfa/items
    (:import-from #:macro-level #:macro-level)
    (:shadow #:dress #:onesie #:diaper #:onesie/opened #:onesie/closed #:incontinence-pad)
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
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
        #:toddler-dress
        #:knights-armor
        #:tshirt
        #:jeans
        #:baggy-jeans
        #:orca-suit
        #:stretchable-orca-suit
        #:boxers
        #:panties
        #:bra
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
        #:generic-diapers
        #:generic-diapers-package
        #:generic-pullons
        #:generic-pullons-package
        #:incontinence-pad
        #:incontinence-pad-package
        #:cloth-incontinence-pad
        #:diaper
        #:cloth-diaper
        #:diaper-package
        #:kurikia-thick-diaper
        #:kurikia-thick-cloth-diaper
        #:thick-latex-diaper
        #:hyper-thick-diaper
        #:hyper-thick-cloth-diaper
        #:hyper-thick-latex-diaper
        #:pullups
        #:pullups-package
        #:cloth-pullups
        #:swim-diaper-cover
        #:bandit-diaper
        #:bandit-adjustable-diaper
        #:bandit-female-diaper
        #:bandit-swim-diaper-cover
        #:lower-bandit-swim-diaper-cover
        #:female-bandit-swim-diaper-cover
        #:magic-diaper-key
        #:gold-bar
        #:gem
        #:gold-collar
        #:collar
        #:magic-diaper-key
        #:ak47
        #:pink-sword
        #:pocket-map-machine
        #:warp-device
        #:navy-shirt
        #:navy-pants
        #:navy-skirt
        #:navy-pullups
        #:pirate-dress
        #:pirate-shirt)
    (:documentation "Contains all the items in the game"))
(uiop:define-package #:yadfa/npcs
    (:import-from #:macro-level #:macro-level)
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
        #:diapered-raccoon-bandit
        #:rookie-diapered-raccoon-bandit
        #:female-diapered-raccoon-bandit
        #:giant-diapered-raccoon-bandit
        #:navy-officer
        #:navy-officer*
        #:diaper-pirate
        #:thickly-diaper-pirate)
    (:documentation "Contains all the NPCs in the game"))
(uiop:define-package #:yadfa/status-conditions
    (:import-from #:macro-level #:macro-level)
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
        #:wetting
        #:messing
        #:mushed
        #:tickled)
    (:documentation "Contains all the status condtions in the game"))
(uiop:define-package #:yadfa/zones
    (:import-from #:macro-level #:macro-level)
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
        #:home
        #:debug-map
        #:bandits-domain
        #:|1|
        #:|2|
        #:ironside
        #:silver-cape
        #:bandits-way
        #:cave-entrance
        #:descend
        #:bandits-entrance
        #:secret-underground
        #:pirates-cove
        #:your-ship)
    (:documentation "Contains all the zone definitions in the game"))
(uiop:define-package #:yadfa/events
    (:use #:yadfa #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:export
        #:enter-bandits-village-1
        #:enter-bandits-village-2
        #:decend-bandits-cave-1
        #:get-diaper-locked-1
        #:obtain-diaper-lock-1
        #:enter-bandits-kennel-1)
    (:documentation "Contains all the event definitions in the game"))
(uiop:define-package #:yadfa-user
    (:use #:yadfa #:yadfa/bin #:c2cl #:marshal #:iterate #:ugly-tiny-infix-macro #:alexandria #:trivial-garbage)
    (:documentation "The package that the player typically executes commands from"))
