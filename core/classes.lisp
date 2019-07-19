(in-package :yadfa)
(defmethod ms:class-persistent-slots ((self standard-object))
  (mapcar #'c2mop:slot-definition-name
          (c2mop:class-slots (class-of self))))
(defclass yadfa-class ()
  ((attributes
    :initarg :attributes
    :initform '()
    :accessor attributes-of
    :documentation "Plist of attributes which are used instead of slots for stuff that aren't shared between slots"))
  (:documentation "All the classes that are part of the game's core inheit this class"))
(defclass status-condition (yadfa-class)
  ((name
    :initarg :name
    :initform nil
    :accessor name-of
    :documentation "name of status condition")
   (description
    :initarg :description
    :initform nil
    :accessor description-of
    :documentation "description of status conditions")
   (target
    :initarg :target
    :initform nil
    :accessor target-of
    :documentation "Enemy target that the battle script affects")
   (battle-script
    :initarg :battle-script
    :initform (lambda (target user self)
                (declare (ignorable target user self))
                nil)
    :accessor battle-script-of
    :documentation "function that runs at the beginning of the user's turn. USER is the user with the condition. TARGET is the enemy of said user, and SELF is the condition itself")
   (blocks-turn
    :initarg :blocks-turn
    :initform nil
    :accessor blocks-turn-of
    :documentation "If T this condition prevents the player from moving")
   (duration
    :initarg :duration
    :initform t
    :accessor duration-of
    :documentation "How many turns this condition lasts. T means it lasts indefinitely.")
   (stat-delta
    :initarg :stat-delta
    :initform '()
    :accessor stat-delta-of
    :documentation "Plist containing the status modifiers in the form of deltas")
   (stat-multiplier
    :initarg :stat-multiplier
    :initform '()
    :accessor stat-multiplier-of
    :documentation "Plist containing the status modifiers in the form of multipliers")
   (priority
    :initarg :priority
    :initform 0
    :accessor priority-of
    :documentation "Unsigned integer that specifies How important this condition is to cure. Used for the AI. Lower value means more important"))
  (:documentation "Base class for all the status conditions "))
(defclass base-character (yadfa-class)
  ((name
    :initarg :name
    :initform :missingno.
    :accessor name-of
    :documentation "Name of the character")
   (description
    :initarg :description
    :initform :?
    :accessor description-of
    :documentation "Description of the character")
   (health
    :initarg :health
    :accessor health-of
    :documentation "Health of the character.")
   (energy
    :initarg :energy
    :accessor energy-of
    :documentation "Energy of the character.")
   (default-attack-power
    :initarg :default-attack-power
    :initform 40
    :accessor default-attack-power-of
    :documentation "The default attack base stat when no attack is selected and no weapon is equipped")
   (default-attack
    :initarg :default-attack
    :accessor default-attack-of
    :initform '(lambda (target user)
                (let ((a (calculate-damage target user (default-attack-power-of user))))
                  (format t "~a attacks ~a~%" (name-of user) (name-of target))
                  (decf (health-of target) a)
                  (format t "~a received ~a damage~%" (name-of target) a)
                  a))
    :documentation "The default attack when no attack is selected and no weapon is equipped")
   (level
    :initarg :level
    :initform 2
    :accessor level-of
    :documentation "character's current level")
   (male
    :initarg :male
    :initform t
    :accessor malep
    :documentation "True if the character is male, false if female")
   (wear
    :initarg :wear
    :initform ()
    :accessor wear-of
    :documentation "List of clothes the character is wearing, outer clothes listed first")
   (species
    :initarg :species
    :initform :missingno.
    :accessor species-of
    :documentation "Character's species.")
   (last-process-potty-time
    :initarg :last-process-potty-time
    :initform 0
    :accessor last-process-potty-time-of
    :documentation "Last time process-potty was processed")
   (bladder/contents
    :initarg :bladder/contents
    :initform 0
    :accessor bladder/contents-of
    :documentation "Amount in ml that the character is holding in in ml.")
   (bladder/fill-rate
    :initarg :bladder/fill-rate
    :initform (* (/ 2000 24 60) 0)
    :accessor bladder/fill-rate-of
    :documentation "Amount in ml that the character's bladder fills each turn.")
   (bladder/need-to-potty-limit
    :initarg :bladder/need-to-potty-limit
    :initform 300
    :accessor bladder/need-to-potty-limit-of
    :documentation "How full the bladder needs to be before the character needs to go")
   (bladder/potty-dance-limit
    :initarg :bladder/potty-dance-limit
    :initform 450
    :accessor bladder/potty-dance-limit-of
    :documentation "How full the character's bladder needs to be before the character starts doing a potty dance")
   (bladder/potty-desperate-limit
    :initarg :bladder/potty-desperate-limit
    :initform 525
    :accessor bladder/potty-desperate-limit-of
    :documentation "How full the character's bladder needs to be before the character starts begging to be taken to the bathroom")
   (bladder/maximum-limit
    :initarg :bladder/maximum-limit
    :initform 600
    :accessor bladder/maximum-limit-of
    :documentation "When the character's bladder gets this full, {s,he} wets {him,her}self")
   (bowels/contents
    :initarg :bowels/contents
    :initform 0
    :accessor bowels/contents-of
    :documentation "Amount in cg that the character is holding in")
   (bowels/fill-rate
    :initarg :bowels/fill-rate
    :initform (* (/ 12000 24 60) 0)
    :accessor bowels/fill-rate-of
    :documentation "Amount in cg that the character's bowels fills each turn")
   (bowels/need-to-potty-limit
    :initarg :bowels/need-to-potty-limit
    :initform 4000
    :accessor bowels/need-to-potty-limit-of
    :documentation "How full the bowels need to be before the character needs to go")
   (bowels/potty-dance-limit
    :initarg :bowels/potty-dance-limit
    :initform 6000
    :accessor bowels/potty-dance-limit-of
    :documentation "How full the character's bowels need to be before the character starts doing a potty dance")
   (bowels/potty-desperate-limit
    :initarg :bowels/potty-desperate-limit
    :initform 7000
    :accessor bowels/potty-desperate-limit-of
    :documentation "How full the character's bowels needs to be before the character starts begging to be taken to the bathroom")
   (bowels/maximum-limit
    :initarg :bowels/maximum-limit
    :initform 8000
    :accessor bowels/maximum-limit-of
    :documentation "When the character's bowels gets this full, {he,she} messes {him,her}self")
   (moves
    :initarg :moves
    :initform ()
    :accessor moves-of
    :documentation "list of moves the character knows")
   (exp
    :initarg :exp
    :accessor exp-of
    :initform 0
    :documentation "How many experience points the character has")
   (base-stats
    :initarg :base-stats
    :initform (list :health 45 :attack 80 :defense 50 :energy 45 :speed 120)
    :accessor base-stats-of
    :documentation "the base stats of the character")
   (iv-stats
    :initarg :iv-stats
    :initform (list :health (random 16) :attack (random 16) :defense (random 16) :energy (random 16) :speed (random 16))
    :accessor iv-stats-of
    :documentation "iv stats of the character")
   (bitcoins
    :initarg :bitcoins
    :initform 0
    :accessor bitcoins-of
    :documentation "Amount of ₿itcoins the character has. Not limited to a single country.")
   (inventory
    :initarg :inventory
    :initform ()
    :accessor inventory-of
    :documentation "List of items the character has.")
   (wield
    :initarg :wield
    :initform nil
    :accessor wield-of
    :documentation "Item the character is weilding as a weapon"))
  (:documentation "Base class for the characters in the game"))
(defclass team-member (base-character) ()
  (:documentation "Either the player or an ally inherits this class"))
(defclass ally (team-member)
  ((learned-moves
    :initarg :learned-moves
    :accessor learned-moves-of
    :initform (list (cons 100 'yadfa-moves:superglitch) (cons 11 'yadfa-moves:kamehameha) (cons 7 'yadfa-moves:tickle) (cons 8 'yadfa-moves:mush))
    :documentation "Alist of moves the player learns by leveling up, first element is the level when you learn them ove, second is a symbol from the `yadfa-moves' package"))
  (:documentation "Team member that is not the player")
  (:default-initargs
   :base-stats (list :health 35 :attack 55 :defense 40 :energy 35 :speed 90)
   :name "Anon"
   :level 5
   :species "fox"
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2)
   :wear (list (make-instance 'yadfa-items:diaper))
   :moves (list (make-instance 'yadfa-moves:watersport) (make-instance 'yadfa-moves:mudsport))))
(defclass ally-no-potty-training (ally) ())
(defclass ally-rebel-potty-training (ally) ())
(defclass ally-silent-potty-training (ally) ())
(defclass ally-last-minute-potty-training (ally) ())
(defmethod print-object ((obj ally) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~w" (name-of obj))))
(defclass playable-ally (ally) ())
(defmethod initialize-instance :after
    ((c base-character) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (iter (for (a b) on initargs)
    (cond ((eq a :base-health)
           (setf (getf (base-stats-of c) :health)
                 b))
          ((eq a :base-attack)
           (setf (getf (base-stats-of c) :attack)
                 b))
          ((eq a :base-defence)
           (setf (getf (base-stats-of c) :defence)
                 b))
          ((eq a :base-speed)
           (setf (getf (base-stats-of c) :speed)
                 b))
          ((eq a :base-energy)
           (setf (getf (base-stats-of c) :energy)
                 b))))
  (unless (iter (for (a b) on initargs)
            (when (eq a :health)
              (leave t)))
    (setf (health-of c) (calculate-stat c :health)))
  (unless (iter (for (a b) on initargs)
            (when (eq a :energy) (leave t)))
    (setf (energy-of c) (calculate-stat c :energy)))
  (setf (exp-of c) (calculate-level-to-exp (level-of c))))
(defclass player (team-member)
  ((position
    :initarg :position
    :initform '(0 0 0 yadfa-zones:debug-map)
    :accessor position-of
    :documentation "Current position in the form of `(list x y z map)'.")
   (warp-on-death-point
    :initarg :warp-on-death-point
    :accessor warp-on-death-point-of
    :documentation "Where the player warps to when {s,}he dies, same format as POSITION")
   (learned-moves
    :initarg :learned-moves
    :accessor learned-moves-of
    :initform (list (cons 100 'yadfa-moves:superglitch) (cons 11 'yadfa-moves:kamehameha) (cons 7 'yadfa-moves:tickle) (cons 8 'yadfa-moves:mush))
    :documentation "Alist of moves the player learns by leveling up, first element is the level when you learn them ove, second is a symbol from the `yadfa-moves'"))
  (:documentation "The player")
  (:default-initargs
   :base-stats (list :health 45 :attack 80 :defense 50 :energy 45 :speed 120)
   :name "Anon"
   :description "This is you stupid"
   :level 5
   :species "Fox"
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2)
   :wear (list (make-instance 'yadfa-items:diaper))
   :moves (list (make-instance 'yadfa-moves:watersport)
                (make-instance 'yadfa-moves:mudsport)
                (make-instance 'yadfa-moves:mush)
                (make-instance 'yadfa-moves:tickle))))
(defmethod initialize-instance :after
    ((c player) &rest initargs)
  (declare (ignorable initargs))
  (unless (iter (for (a b) on initargs)
            (when (eq a :warp-on-death-point) (leave t)))
    (setf (warp-on-death-point-of c) (position-of c))))
(defclass zone (yadfa-class)
  ((description
    :initarg :description
    :initform "Seems Pouar didn't make the text for this room yet, get to it you lazy fuck"
    :accessor description-of
    :documentation "room description")
   (enter-text
    :initarg :enter-text
    :initform "Seems Pouar didn't make the text for this room yet, get to it you lazy fuck"
    :accessor enter-text-of
    :documentation "Text that pops up when you enter the room")
   (position
    :initarg :position
    :initform '()
    :accessor position-of
    :documentation "Position of the zone. Used when we can't figure out the position of the zone ahead of time and to avoid iterating through the hash table.")
   (name
    :initarg :name
    :initform "Mystery Zone"
    :accessor name-of
    :documentation "Name of the room")
   (props
    :initarg :props
    :initform ()
    :accessor props-of
    :documentation "Plist of props in the room, and by `props' I mean instances of the PROP class")
   (events
    :initarg :events
    :initform ()
    :accessor events-of
    :documentation "list of events that run when you enter a room")
   (continue-battle
    :initarg :continue-battle
    :initform nil
    :accessor continue-battle-of
    :documentation "A previous battle (which is an instance of the battle class) triggered by an event that you lost. Used to keep the game in a consistent state after losing.")
   (underwater
    :initarg :underwater
    :initform nil
    :accessor underwaterp
    :documentation "Whether this zone is underwater or not, better get some waterproof clothing if you don't want your diaper to swell up")
   (warp-points
    :initarg :warp-points
    :initform ()
    :accessor warp-points-of
    :documentation "Plist of warp points to different maps, values are lists in the same form as the position of the player, keys are passed to the `move` function")
   (locked
    :initarg :locked
    :initform :nil
    :accessor lockedp
    :documentation "Whether this area is locked or not. contains the type specifier of the key needed to unlock it if locked, set to nil if it isn't locked")
   (hidden
    :initarg :hidden
    :initform nil
    :accessor hiddenp
    :documentation "When true, the game pretends this room doesn't exist. This is for when certain events in the game makes certain zones disappear from the map and to avoid making them be in the exact same state as in the beginning of the game when they reappear")
   (direction-attributes
    :initarg :direction-attributes
    :initform ()
    :accessor direction-attributes-of
    :documentation "List of attributes based on the direction rather than the zone itself")
   (can-potty
    :initarg :can-potty
    :initform '(lambda (prop &key wet mess pants-down user)
                (declare (ignore prop wet mess pants-down user))
                t)
    :accessor can-potty-p
    :documentation "Whether you're allowed to go potty in this zone. PROP is the prop you're going potty on if any while USER is the one going potty. PANTS-DOWN is T when USER pulls his/her pants down and WET and MESS are the arguments")
   (potty-trigger
    :initarg :potty-trigger
    :initform '(lambda (had-accident user)
                (declare (ignore had-accident user))
                nil)
    :accessor potty-trigger-of
    :documentation "Runs whenever the user goes potty, whether on purpose or by accident, arguments are the cons called HAD-ACCIDENT that gets passed from the process-potty function, and USER which is the user who did it")
   (must-wear
    :initarg :must-wear
    :initform '(t . (lambda (user)
                      (declare (ignore user))
                      t))
    :accessor must-wear-of
    :documentation "Used to determine whether you can enter the zone based on what you're wearing. Is a cons with the type specifier of what you must be wearing and a lambda expression or function that runs to determine if you can enter the zone")
   (must-wear*
    :initarg :must-wear*
    :initform '(t . (lambda (user)
                      (declare (ignore user))
                      t))
    :accessor must-wear*-of
    :documentation "Similar to the must-wear slot but is done when you try to wear or change while still inside the zone")
   (must-not-wear
    :initarg :must-not-wear
    :initform '(nil . (lambda (user)
                        (declare (ignore user))
                        t))
    :accessor must-not-wear-of
    :documentation "Used to determine whether you can enter the zone based on what you're wearing. Is a cons with the type specifier of what you must not be wearing and a lambda expression or function that runs to determine if you can enter the zone")
   (must-not-wear*
    :initarg :must-not-wear*
    :initform '(nil . (lambda (user)
                        (declare (ignore user))
                        t))
    :accessor must-not-wear*-of
    :documentation "Similar to the must-not-wear slot but is done when you try to wear or change while still inside the zone")
   (no-wetting/messing
    :initarg no-wetting/messing
    :initform '(lambda (user)
                (declare (ignore user))
                nil)
    :accessor no-wetting/messing-of
    :documentation "lambda expression or function that tells you if you're allowed to wet or mess voluntarily")
   (enemy-spawn-list
    :initarg :enemy-spawn-list
    :initform ()
    :accessor enemy-spawn-list-of
    :documentation "list containing what enemies might show up when you enter an area. Each entry looks like this `(:random random :max-random max-random :enemies enemies)' If RANDOM is specified, then the probability of the enemy being spawn is RANDOM/MAX-RANDOM otherwise it is 1/MAX-RANDOM"))
  (:documentation "A zone on the map"))
(defmethod print-object ((obj zone) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s \"~a\"" (position-of obj) (name-of obj))))
(defclass stat/move (yadfa-class)
  ((name
    :initarg :name
    :initform :-
    :accessor name-of
    :documentation "name of move")
   (description
    :initarg :description
    :initform :-
    :accessor description-of
    :documentation "Description of move")
   (energy-cost
    :initarg :energy-cost
    :initform 0
    :accessor energy-cost-of
    :documentation "How much energy this move costs")
   (power
    :initarg :power
    :initform 40
    :accessor power-of
    :documentation "Number used to determine the damage of this attack")
   (ai-flags
    :initarg :ai-flags
    :initform ()
    :accessor ai-flags-of
    :documentation "list containing flags that affect the behavior of the AI.")
   (attack
    :initarg :attack
    :initform '(lambda (target user self)
                (let ((a (calculate-damage target user (power-of self))))
                  (format t "~a used ~a~%" (name-of user) (name-of self))
                  (decf (health-of target) a)
                  (format t "~a received ~a damage~%" (name-of target) a)
                  a))
    :accessor attack-of
    :documentation "function that performs the move. TARGET is the enemy that is being attacked and USER is the one doing the attacking, SELF is the move itself"))
  (:documentation "base class of moves used in battle"))
(defclass prop (yadfa-class)
  ((description
    :initarg :description
    :initform ""
    :accessor description-of
    :documentation "Description of a prop")
   (name
    :initarg :name
    :initform ""
    :accessor name-of
    :documentation "Name of prop")
   (placeable
    :initarg :placeable
    :initform nil
    :accessor placeablep
    :documentation "Whether you can place items here")
   (items
    :initarg :items
    :initform ()
    :accessor items-of
    :documentation "List of items this prop has")
   (bitcoins
    :initarg :bitcoins
    :initform 0
    :accessor bitcoins-of
    :documentation "Number of bitcoins this prop has")
   (actions
    :initarg :actions
    :initform ()
    :accessor actions-of
    :documentation "Plist of actions who's lambda-list is `(prop &key &allow-other-keys)' that the player sees as actions they can perform with the prop, PROP is the instance that this slot belongs to"))
  (:documentation "Tangible objects in the AREA that the player can interact with"))
(defmethod print-object ((obj prop) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~w" (name-of obj))))
(defclass item (yadfa-class)
  ((description
    :initarg :description
    :initform :?
    :accessor description-of
    :documentation "item description")
   (name
    :initarg :name
    :initform :teru-sama
    :accessor name-of
    :documentation "item description")
   (plural-name
    :initarg :plural-name
    :initform nil
    :accessor plural-name-of
    :documentation "The plural name of item")
   (ammo-type
    :initarg :ammo-type
    :initform nil
    :accessor ammo-type-of
    :documentation "A type specifier specifying the type of ammo this will hold")
   (ammo
    :initarg :ammo
    :initform ()
    :accessor ammo-of
    :documentation "List of ammo this item has")
   (ammo-power
    :initarg :ammo-power
    :initform ()
    :accessor ammo-power-of
    :documentation "Attack base when using this as ammo.")
   (reload-count
    :initarg :reload-count
    :initform nil
    :accessor reload-count-of
    :documentation "When in battle, the maximum amount of ammo the user can reload into this item per turn, if nil, then there is no limit")
   (ammo-capacity
    :initarg :ammo-capacity
    :initform 0
    :accessor ammo-capacity-of
    :documentation "How much ammo this thing can hold")
   (consumable
    :initarg :consumable
    :initform nil
    :accessor consumablep
    :documentation "Whether this item goes away when you use it")
   (tossable
    :initarg :tossable
    :initform t
    :accessor tossablep
    :documentation "Whether you can throw this item away or not")
   (sellable
    :initarg :sellable
    :initform t
    :accessor sellablep
    :documentation "Whether you can sell this item or not")
   (value
    :initarg :value
    :initform 0
    :accessor value-of
    :documentation "Value of item in bitcoins")
   (ai-flags
    :initarg :ai-flags
    :initform ()
    :accessor ai-flags-of
    :documentation "List of flags that affect the AI")
   (power
    :initarg :power
    :initform 40
    :accessor power-of
    :documentation "Attack base when used as a melee weapon")
   (cant-use-predicate
    :initarg :cant-use-predicate
    :initform '(lambda (item user &rest keys &key target action &allow-other-keys)
                (declare (ignorable item user keys target action))
                nil)
    :accessor cant-use-predicate-of
    :documentation "Function that is used to determine if the player can use this item")
   (attack-script
    :initarg :attack-script
    :initform '(lambda (target user self)
                (declare (ignorable target user self))
                (let ((a (calculate-damage target user
                                           (if (first (ammo-of self))
                                               (ammo-power-of (first (ammo-of self)))
                                               (power-of self)))))
                  (format t "~a whacks ~a with ~a ~a~%"
                          (name-of user)
                          (name-of target)
                          (if (malep user) "his" "her")
                          (name-of self))
                  (decf (health-of target) a)
                  (format t "~a received ~a damage~%" (name-of target) a)))
    :accessor attack-script-of
    :documentation "Script that runs when attacking with this weapon")
   (wear-stats
    :initarg :wear-stats
    :initform ()
    :accessor wear-stats-of
    :documentation "stat boost when wearing this item. Is a plist in the form of (list :attack attack :defense defense :health health :energy energy :speed speed)")
   (wield-stats
    :initarg :wield-stats
    :initform ()
    :accessor wield-stats-of
    :documentation "stat boost when weilding this item. Is a plist in the form of (list :attack attack :defense defense :health health :energy energy :speed speed)")
   (special-actions
    :initarg :special-actions
    :initform ()
    :accessor special-actions-of
    :documentation "Plist of actions that the player sees as actions with a lambda with the lambda-list `(item user &key &allow-other-keys)' they can perform with the item, ITEM is the instance that this slot belongs to, USER is the user using the item")
   (use-script
    :initarg :use-script
    :initform '()
    :accessor use-script-of
    :documentation "Function that runs when ITEM is used on USER. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on.")
   (wield-script
    :initarg :wield-script
    :initform '()
    :accessor wield-script-of
    :documentation "Function that runs when USER is wielding ITEM. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on.")
   (wear-script
    :initarg :wear-script
    :initform '()
    :accessor wear-script-of
    :documentation "Function that runs when USER is wearing ITEM. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on."))
  (:documentation "Something you can store in your inventory and use"))
(defclass consumable (item)
  ()
  (:documentation "Doesn't actually cause items to be consumable, but is there to make filtering easier"))
(defclass ammo (item)
  ()
  (:documentation "Ammo is typically inherited by this class, but nothing in the code actually enforces this and is meant to make filtering easier"))
(defclass weapon (item)
  ()
  (:documentation "Weapons typically inherited this class, but nothing in the code actually enforces this and is meant to make filtering easier"))
(defclass clothing (item)
  ())
(defclass top (clothing)
  ())
(defclass headpiece (clothing)
  ())
(defclass bottoms (clothing)
  ((bulge-text
    :initarg :bulge-text
    :initform ()
    :accessor bulge-text-of
    :documentation "A list of pairs containing the different text that describes the appearance that your diapers have on your pants based on the thickness, first one is the minimum thickness needed for the second text. the text for thicker padding must be listed first")
   (thickness
    :initarg :thickness
    :initform 1
    :accessor thickness-of
    :documentation "the thickness of the undies in mm")
   (thickness-capacity
    :initarg :thickness-capacity
    :initform (* (expt 6.0 1/3) 25.4)
    :accessor thickness-capacity-of
    :documentation "The maximum thickness of your diaper that this can fit over. NIL means infinite")
   (thickness-capacity-threshold
    :initarg :thickness-capacity-threshold
    :initform 50
    :accessor thickness-capacity-threshold-of
    :documentation "How much higher than the thickness capacity the clothing can handle diaper expansion in mm before popping/tearing, NIL means it won't pop/tear")
   (waterproof
    :initarg :waterproof
    :initform nil
    :accessor waterproofp
    :documentation "Whether this prevents your diapers from swelling up in water")
   (disposable
    :initarg :disposable
    :initform nil
    :accessor disposablep
    :documentation "Whether you clean this or throw it away")
   (sogginess
    :initarg :sogginess
    :initform 0
    :accessor sogginess-of
    :documentation "sogginess in ml")
   (sogginess-capacity
    :initarg :sogginess-capacity
    :initform 10
    :accessor sogginess-capacity-of
    :documentation "sogginess capacity in ml")
   (messiness
    :initarg :messiness
    :initform 0
    :accessor messiness-of
    :documentation "messiness in cg")
   (messiness-capacity
    :initarg :messiness-capacity
    :initform 10
    :accessor messiness-capacity-of
    :documentation "messiness capacity in cg")
   (mess-text
    :initarg :mess-text
    :initform '()
    :accessor mess-text-of
    :documentation "Plist that contain the text that comes up in the description when in the inventory with the minimal messiness as the key")
   (wet-text
    :initarg :wet-text
    :initform '()
    :accessor wet-text-of
    :documentation "Plist that contains that contain the text that comes up in the description when in the inventory with the minimal sogginess as the key")
   (wear-mess-text
    :initarg :wear-mess-text
    :initform ()
    :accessor wear-mess-text-of
    :documentation "Plist that contains the text that comes up in the description when wearing it with the minimal messiness as the key")
   (wear-wet-text
    :initarg :wear-wet-text
    :initform ()
    :accessor wear-wet-text-of
    :documentation "Plist that contain the text that comes up in the description when wearing it with the minimal sogginess as the key")
   (key
    :initarg :key
    :initform nil
    :accessor key-of
    :documentation "Whether this piece of clothing can be locked to prevent removal. Set this to the quoted type specifier that is needed to unlock it")
   (locked
    :initarg :locked
    :initform nil
    :accessor lockedp
    :documentation "Whether this clothing is locked to prevent removal")))
(defclass closed-bottoms (bottoms)
  ())
(defclass full-outfit (top bottoms)
  ())
(defclass closed-pants (closed-bottoms) ())
(defclass closed-full-outfit (full-outfit closed-pants)
  ())
(defclass onesie (full-outfit)
  ((onesie-thickness-capacity
    :initarg :onesie-thickness-capacity
    :initform (cons 100 nil)
    :accessor onesie-thickness-capacity-of
    :documentation "cons of values for the thickness capacity of the onesie, first value is for when it's closed, second for when it's opened")
   (onesie-thickness-capacity-threshold
    :initarg :onesie-thickness-capacity-threshold
    :initform (cons 5 nil)
    :accessor onesie-thickness-capacity-threshold-of
    :documentation "cons of values for the thickness capacity threshold of the onesie, first value is for when it's closed, second for when it's opened")
   (onesie-waterproof
    :initarg :onesie-waterproof
    :initform nil
    :accessor onesie-waterproof-p
    :documentation "Boolean that determines whether the onesie prevents your diaper from swelling up when closed.")
   (onesie-bulge-text
    :initarg :onesie-bulge-text
    :initform (cons () ())
    :accessor onesie-bulge-text-of
    :documentation "A cons containing 2 lists of pairs containing the different text that describes the appearance that your diapers have on your pants based on the thickness, first one is the minimum thickness needed for the second text. the text for thicker padding must be listed first. car is the value for when it's closed, cdr is the value when it's open")))
(defclass onesie/opened (onesie)
  ())
(defclass onesie/closed (onesie closed-full-outfit)
  ())

(defmethod update-instance-for-different-class :after ((old onesie/opened) (new onesie/closed) &key)
  (setf (thickness-capacity-of new) (car (slot-value old 'onesie-thickness-capacity)))
  (setf (thickness-capacity-threshold-of new) (car (slot-value old 'onesie-thickness-capacity-threshold)))
  (setf (waterproofp new) (onesie-waterproof-p old))
  (setf (bulge-text-of new) (car (slot-value old 'onesie-bulge-text)))
  )
(defmethod update-instance-for-different-class :after ((old onesie/closed) (new onesie/opened) &key)
  (setf (thickness-capacity-of new) (cdr (slot-value old 'onesie-thickness-capacity)))
  (setf (thickness-capacity-threshold-of new) (cdr (slot-value old 'onesie-thickness-capacity-threshold)))
  (setf (waterproofp new) nil)
  (setf (bulge-text-of new) (cdr (slot-value old 'onesie-bulge-text))))
(defmethod initialize-instance :after
    ((c onesie/opened) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (thickness-capacity-of c) (cdr (onesie-thickness-capacity-of c)))
  (setf (thickness-capacity-threshold-of c) (cdr (onesie-thickness-capacity-threshold-of c)))
  (setf (waterproofp c) nil)
  (setf (bulge-text-of c) (cdr (onesie-bulge-text-of c))))
(defmethod initialize-instance :after
    ((c onesie/closed) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (thickness-capacity-of c) (car (onesie-thickness-capacity-of c)))
  (setf (thickness-capacity-threshold-of c) (car (onesie-thickness-capacity-threshold-of c)))
  (setf (waterproofp c) (onesie-waterproof-p c))
  (setf (bulge-text-of c) (car (onesie-bulge-text-of c))))
(defclass incontinence-product (closed-bottoms) ()
  (:default-initargs
   :thickness-capacity-threshold nil))
(defclass padding (incontinence-product) ())
(defclass cub-undies (incontinence-product) ())
(defclass pullon (padding) ()
  (:default-initargs
   :thickness (* 1/2 25.4)
   :thickness-capacity 40))
(defclass tabbed-briefs (padding) ()
  (:default-initargs
   :thickness 25.4
   :thickness-capacity 80
   :key 'yadfa-items:magic-diaper-key))
(defclass incontinence-pad (incontinence-product) ()
  (:default-initargs
   :thickness (* 1/4 25.4)
   :thickness-capacity 20))
(defclass undies (closed-bottoms)
  ()
  (:default-initargs
   :thickness-capacity (* (expt 6.0 1/3) 25.4)))
(defclass stuffer (incontinence-pad cub-undies)
  ())
(defclass diaper (tabbed-briefs cub-undies)
  ())
(defclass pullup (pullon cub-undies)
  ())
(defclass skirt (bottoms)
  ()
  (:default-initargs
   :thickness-capacity 100
   :thickness-capacity-threshold nil))
(defclass dress (full-outfit)
  ()
  (:default-initargs
   :thickness-capacity 100
   :thickness-capacity-threshold nil))
(defclass shirt (top)
  ())
(defclass pants (closed-pants)
  ())
(defclass toilet (prop) ()
  (:default-initargs
   :name "Toilet"
   :description "A toilet"
   :actions (list :use (make-action
                        :documentation "Use the toilet. if WET or MESS is T, the player will empty his bladder/bowels completely. If a number is given, the player will empty his bladder by that amount, however the player will mess completely no matter what number you give it if you provide a number. If ALLY number is specified, that ALLY uses the toilet, otherwise it's the player"
                        :lambda '(lambda (prop &rest keys &key wet mess pull-pants-down ally &allow-other-keys)
                                  (declare (type prop prop) (type boolean pull-pants-down) (type (or integer null) ally) (type (or boolean number) wet mess))
                                  (check-type prop prop)
                                  (check-type pull-pants-down boolean)
                                  (check-type ally (or integer null))
                                  (check-type wet (or boolean number))
                                  (check-type mess (or boolean number))
                                  (block nil
                                    (when (and ally (>= ally (list-length (allies-of *game*))))
                                      (format t "That ally doesn't exist~%")
                                      (return))
                                    (potty-on-toilet prop
                                                     :wet wet
                                                     :mess mess
                                                     :pants-down pull-pants-down
                                                     :user (if ally
                                                               (nth ally (allies-of *game*))
                                                               (player-of *game*))))))))
  (:documentation "Class for toilets. I'm pretty sure I don't need to tell you what these are for."))
(defclass washer (prop) ()
  (:default-initargs
   :name "Washer"
   :description "A place to clean your reusable diapers and all the clothes you've ruined"
   :actions (list :use (make-action
                        :documentation "Wash your clothes in this"
                        :lambda '(lambda
                                  (prop &rest keys &key &allow-other-keys)
                                  (declare (type prop prop))
                                  (check-type prop prop)
                                  (yadfa-world:wash-all-in prop)))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))

(defclass automatic-changing-table (prop) ()
  (:default-initargs
   :name "Automatic Chainging Table"
   :description "A changing table that automatically changes you"
   :actions (list :use (make-action
                        :documentation "Turn it on"
                        :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                  (declare (type prop prop))
                                  (check-type prop prop)
                                  (iter (for j in (append (list (player-of *game*)) (allies-of *game*)))
                                    (let ((a (calculate-diaper-usage j)))
                                      (when (and
                                             (or
                                              (>=
                                               (getf a :sogginess)
                                               (/ (getf a :sogginess-capacity) 4))
                                              (>=
                                               (getf a :messiness)
                                               (/ (getf a :messiness-capacity) 4)))
                                             (filter-items (wear-of j) 'closed-bottoms))
                                        (format t "Mechanical arms come out of the changing table and strap ~a down on the table to prevent ~a from escaping and proceeeds to change ~a~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her")
                                                (if (malep j) "him" "her"))
                                        (if (filter-items (wear-of j) 'padding)
                                            (progn
                                              (format t "~a: Hey!!! Don't change me here!!! People can see me!!! Stop!!!~%~%"
                                                      (name-of j)))
                                            (progn
                                              (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%"
                                                      (name-of j))))
                                        (change-the-baby j 'yadfa-items:kurikia-thick-diaper :locked t)
                                        (format t "*The machine removes ~a's soggy clothing (and any clothing that doesn't fit over the new diaper) and puts a thick diaper on ~a, then locks it to prevent the baby from removing it.*~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her"))
                                        (format t "*The machine unstraps ~a from the table and lets ~a go. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her")
                                                (name-of j)
                                                (if (malep j) "him" "her"))
                                        (trigger-event 'yadfa-events:get-diaper-locked-1))))))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass checkpoint (prop) ()
  (:default-initargs
   :name "Checkpoint"
   :description "You can use this to set this zone as a checkpoint so when you lose a battle, you'll warp to here rather than at the beginning of the game"
   :actions (list :set-checkpoint (make-action :documentation "Set checkpoint"
                                               :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                         (declare (type prop prop) (ignore prop))
                                                         (check-type prop prop)
                                                         (setf (warp-on-death-point-of (player-of *game*)) (position-of (player-of *game*)))
                                                         (format t "You will now teleport here when you black out")))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass shop (prop)
  ((items-for-sale
    :initarg :items-for-sale
    :initform ()
    :accessor items-for-sale-of
    :documentation "Quoted list of class names for sale"))
  (:default-initargs
   :name "Shop"
   :description "A place to buy crap with your bitcoins")
  (:documentation "Class for shops, you can buy stuff from these."))
(defmethod initialize-instance :after
    ((c shop) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare (type prop prop)
                                                              (ignore keys))
                                                             (check-type prop prop)
                                                             (shopfun (items-for-sale-of prop) :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare (type prop prop) (type list items) (ignore keys))
                                                   (check-type prop prop)
                                                   (check-type items list)
                                                   (shopfun (items-for-sale-of prop)
                                                    :items-to-buy items
                                                    :user (player-of *game*))))
                 :sell-items (make-action :documentation "Sell items. ITEMS is a list of indexes where each index corrisponds to an item in your inventory"
                                          :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                    (declare (type prop prop) (type list items) (ignore keys))
                                                    (check-type prop prop)
                                                    (check-type items list)
                                                    (shopfun (items-for-sale-of prop)
                                                     :items-to-sell items
                                                     :user (player-of *game*)))))))
(defclass vending-machine (prop)
  ((items-for-sale
    :initarg :items-for-sale
    :initform ()
    :accessor items-for-sale-of
    :documentation "Quoted list of class names for sale"))
  (:default-initargs
   :name "Vending Machine"
   :description "An automated machine where you can buy items from")
  (:documentation "Class for vending machines, Functions like a shop, but only lets you buy items instead of selling them"))
(defmethod initialize-instance :after
    ((c vending-machine) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare (type prop prop))
                                                             (check-type prop prop)
                                                             (shopfun (items-for-sale-of prop) :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare (type prop prop) (type list items))
                                                   (check-type prop prop)
                                                   (check-type items list)
                                                   (shopfun (items-for-sale-of prop)
                                                    :items-to-buy items
                                                    :user (player-of *game*)))))))
(defclass debug-shop (prop) ()
  (:default-initargs
   :name "Shop"
   :description "A place to buy crap with your bitcoins")
  (:documentation "Class for shops, you can buy stuff from these."))
(defmethod initialize-instance :after
    ((c debug-shop) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare (type prop prop))
                                                             (check-type prop prop)
                                                             (shopfun (let ((a ()))
                                                                        (iter (for i in (list-all-packages))
                                                                          (unless (equal i (find-package :yadfa))
                                                                            (do-external-symbols  (s i)
                                                                              (when (and
                                                                                     (find-class s nil)
                                                                                     (subclassp
                                                                                      (find-class s)
                                                                                      (find-class 'item))
                                                                                     (tossablep (make-instance s)))
                                                                                (push (cons s nil) a)))))
                                                                        a)
                                                              :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare (type prop prop) (type list items))
                                                   (check-type prop prop)
                                                   (check-type items list)
                                                   (shopfun
                                                    (let ((a ()))
                                                      (iter (for i in (list-all-packages))
                                                        (unless
                                                            (equal i (find-package :yadfa))
                                                          (do-external-symbols (s i)
                                                            (when (and
                                                                   (find-class s nil)
                                                                   (subclassp
                                                                    (find-class s)
                                                                    (find-class 'item))
                                                                   (tossablep (make-instance s)))
                                                              (push (cons s nil) a)))))
                                                      a)
                                                    :items-to-buy items
                                                    :user (player-of *game*))))
                 :sell-items (make-action :documentation "Sell items. ITEMS is a list of indexes where each index corrisponds to an item in your inventory"
                                          :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                    (declare (type prop prop) (type list items))
                                                    (check-type prop prop)
                                                    (check-type items list)
                                                    (shopfun
                                                     (let ((a ()))
                                                       (iter
                                                         (for i in (list-all-packages))
                                                         (unless (equal i (find-package :yadfa))
                                                           (do-external-symbols (s i)
                                                             (when (and
                                                                    (find-class s nil)
                                                                    (subclassp
                                                                     (find-class s)
                                                                     (find-class 'item))
                                                                    (tossablep (make-instance s)))
                                                               (push (cons s nil) a)))))
                                                       a)
                                                     :items-to-sell items
                                                     :user (player-of *game*)))))))
(defclass bed (prop) ()
  (:default-initargs
   :name "Bed"
   :description "A place to sleep and recover. Be sure to go potty so you don't wet it."
   :actions (list :sleep (make-action :documentation "Sleep in this bed and recover your health and energy. Be sure to go potty before you go to bed so you don't wet it"
                                      :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                (declare (type prop prop) (ignore prop))
                                                (check-type prop prop)
                                                (go-to-sleep)))))
  (:documentation "Class for beds, you can sleep in these."))
(defclass config (yadfa-class)
  ())
(defclass enemy (base-character)
  ((exp-yield
    :initarg :exp-yield
    :initform 50
    :accessor exp-yield-of
    :documentation "Integer that is the base exp points that player receives when this guy is defeated")
   (bitcoins-per-level
    :initarg :bitcoins-per-level
    :initform 0
    :accessor bitcoins-per-level-of
    :documentation "Bitcoins per level that you get from this enemy per battle")
   (watersport-limit
    :initarg :watersport-limit
    :initform nil
    :accessor watersport-limit-of
    :documentation "How close to bladder/maximum-limit in ml the enemy is before voluntarily wetting his/her diapers. A value of nil means he'll/she'll never wet voluntarily")
   (mudsport-limit
    :initarg :mudsport-limit
    :initform nil
    :accessor mudsport-limit-of
    :documentation "How close to bowels/maximum-limit in cg the enemy is before voluntarily wetting his/her diapers. A value of nil means he'll/she'll never mess voluntarily")
   (watersport-chance
    :initarg :watersport-chance
    :initform 1
    :accessor watersport-chance-of
    :documentation "when WATERSPORT-LIMIT is reached, there is a 1 in WATERSPORT-CHANCE he'll voluntarily wet himself")
   (mudsport-chance
    :initarg :mudsport-chance
    :initform 1
    :accessor mudsport-chance-of
    :documentation "when MUDSPORT-LIMIT is reached, there is a 1 in MUDSPORT-CHANCE he'll voluntarily mess himself")
   (battle-script
    :initarg :battle-script
    :initform (lambda (self target)
                (let ((moves-with-health
                        (iter (for i in (moves-of self))
                          (when (and (>= (energy-of self) (energy-cost-of i)) (position :ai-health-inc (ai-flags-of i)))
                            (collect i))))
                      (moves-can-use (iter (for i in (moves-of self))
                                       (when (>= (energy-of self) (energy-cost-of i))
                                         (collect i))))
                      (move-to-use nil))
                  (cond
                    ((and (<= (health-of self) (/ (calculate-stat self :health) 4)) moves-with-health)
                     (setf move-to-use (random-elt moves-with-health))
                     (funcall (coerce (attack-of move-to-use) 'function) target self move-to-use))
                    (t
                     (when moves-can-use
                       (setf move-to-use (random-elt moves-can-use)))
                     (cond ((and moves-can-use (= (random 2) 0))
                            (funcall (coerce (attack-of move-to-use) 'function) target self move-to-use))
                           ((wield-of self)
                            (funcall (coerce (attack-script-of (wield-of self)) 'function) target self (wield-of self)))
                           (t
                            (funcall (coerce (default-attack-of self) 'function) target self)))))))
    :accessor battle-script-of
    :documentation "function that runs when it's time for the enemy to attack and what the enemy does to attack"))
  (:default-initargs :base-stats (list :health 40 :attack 45 :defense 40 :energy 40 :speed 56) :level (random-from-range 2 5) :bitcoins nil)
  (:documentation "Class for enemies"))
(defmethod print-object ((obj enemy) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "\"~a ~a\"" (if (malep obj) "Male" "Female") (species-of obj))))
(defclass potty-enemy (enemy) ()
  (:default-initargs
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2))
  (:documentation "Class for an enemy with a bladder and bowels fill rate. This enemy may {wet,mess} {him,her}self in battle."))
(defclass battle (yadfa-class)
  ((turn-queue
    :initarg :turn-queue
    :initform ()
    :accessor turn-queue-of
    :documentation "The queue of characters specifing the order of who attacks when in battle")
   (enter-battle-text
    :initarg :enter-battle-text
    :initform nil
    :accessor enter-battle-text-of
    :documentation "The text that comes up when you enter a battle")
   (enemies
    :initarg :enemies
    :initform ()
    :accessor enemies-of
    :documentation "List of enemies in battle")
   (win-events
    :initarg :win-events
    :initform ()
    :accessor win-events-of
    :documentation "List of events that trigger when you've won the battle")
   (status-conditions
    :initarg :status-conditions
    :initform ()
    :accessor status-conditions-of
    :documentation "plist of characters who's values are a plist of conditions that go away after battle")
   (fainted
    :initarg :fainted
    :initform ()
    :accessor fainted-of
    :documentation "Characters that have fainted in battle, used so the \"X has fainted\" messages don't appear repeatedly"))
  (:documentation "Class that contains the information about the battle"))
(defmethod initialize-instance :after
    ((c battle) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (unless (enter-battle-text-of c)
    (setf
     (enter-battle-text-of c)
     (with-output-to-string (s)
       (iter (for i in (enemies-of c))
         (format s "A Wild ~a Appeared!!!~%" (name-of i))))))
  (setf (turn-queue-of c) (sort (copy-tree (append (enemies-of c) (team-of *game*))) '>
                                :key #'(lambda (a)
                                         (calculate-stat a :speed))))
  (incf (time-of *game*) 5))
(defclass game (yadfa-class)
  ((zones
    :initarg :zones
    :initform (make-hash-table :test 'equal)
    :accessor zones-of
    :documentation "Hash table of zones in the game")
   (player
    :initarg :player
    :initform nil
    :accessor player-of
    :documentation "The Player, which is an instance of the player class")
   (allies
    :initarg :allies
    :initform nil
    :accessor allies-of
    :documentation "List of characters that have joined you")
   (team
    :initarg :team
    :initform nil
    :accessor team-of
    :documentation "List of characters sent out to battle")
   (config
    :initarg :config
    :initform (make-instance 'config)
    :accessor config-of
    :documentation "Configuration, instance of the config class")
   (time
    :initarg :time
    :initform 0
    :accessor time-of
    :documentation "Turns since start of game")
   (events
    :initarg :events
    :initform (make-hash-table)
    :accessor events-of
    :documentation "hash table containing all the events in the game")
   (finished-events
    :initarg :finished-events
    :initform '()
    :accessor finished-events-of
    :documentation "A list containing all the symbols of events the player has finished")
   (major-event
    :initarg :major-event
    :initform nil
    :accessor major-event-of
    :documentation "Symbol of the current major event")
   (seen-enemies
    :initarg :seen-enemies
    :initform '()
    :accessor seen-enemies-of))
  (:documentation "List of all the information in the game"))
