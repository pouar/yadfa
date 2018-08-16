(in-package :yadfa)
(defmethod ms:class-persistent-slots ((self standard-object))
    (mapcar #'slot-definition-name
        (class-slots (class-of self))))
(defclass status-condition ()
    ((name
         :initarg :name
         :initform nil
         :accessor name-of
         :type (or simple-string null)
         :documentation "name of status condition")
        (description
            :initarg :description
            :initform nil
            :accessor description-of
            :type (or simple-string null)
            :documentation "description of status conditions")
        (target
            :initarg :target
            :initform nil
            :accessor target-of
            :documentation "Enemy target that the battle script affects")
        (battle-script
            :initarg :battle-script
            :initform '(lambda (target user self)
                           (declare (ignorable target user self)))
            :accessor battle-script-of
            :type (or list function)
            :documentation "script that runs at the beginning of the user's turn. USER is the user with the condition. TARGET is the enemy of said user, and SELF is the condition itself")
        (blocks-turn
            :initarg :blocks-turn
            :initform nil
            :accessor blocks-turn-of
            :type number
            :documentation "If T this condition prevents the player from moving")
        (duration
            :initarg :duration
            :initform t
            :type (or boolean unsigned-byte)
            :accessor duration-of
            :documentation "How many turns this condition lasts. T means it lasts indefinitely.")
        (stat-delta
            :initarg :stat-delta
            :initform '()
            :type list
            :accessor stat-delta-of
            :documentation "Plist containing the status modifiers in the form of deltas")
        (stat-multiplier
            :initarg :stat-multiplier
            :initform '()
            :type list
            :accessor stat-multiplier-of
            :documentation "Plist containing the status modifiers in the form of multipliers")
        (priority
            :initarg :priority
            :initform 0
            :accessor priority-of
            :type unsigned-byte
            :documentation "How important this condition is to cure. Used for the AI. Lower value means more important"))
    (:documentation "Base class for all the status conditions "))
(defclass base-character ()
    ((name
         :initarg :name
         :initform :missingno.
         :accessor name-of
         :type (or simple-string keyword null)
         :documentation "Name of the character")
        (description
            :initarg :description
            :initform :?
            :accessor description-of
            :type (or simple-string keyword null)
            :documentation "Description of the character")
        (health
            :initarg :health
            :accessor health-of
            :type number
            :documentation "Health of the character.")
        (energy
            :initarg :energy
            :accessor energy-of
            :type number
            :documentation "Energy of the character.")
        (level
            :initarg :level
            :initform 2
            :accessor level-of
            :type number
            :documentation "character's current level")
        (male
            :initarg :male
            :initform t
            :accessor malep
            :type boolean
            :documentation "True if the character is male, false if female")
        (wear
            :initarg :wear
            :initform ()
            :accessor wear-of
            :type list
            :documentation "List of clothes the character is wearing, outer clothes listed first")
        (species
            :initarg :species
            :initform :missingno.
            :accessor species-of
            :type (or simple-string keyword null)
            :documentation "Character's species.")
        (bladder/contents
            :initarg :bladder/contents
            :initform 0
            :accessor bladder/contents-of
            :type number
            :documentation "Amount in ml that the character is holding in in ml.")
        (bladder/fill-rate
            :initarg :bladder/fill-rate
            :initform (* (/ 2000 24 60) 0)
            :accessor bladder/fill-rate-of
            :type number
            :documentation "Amount in ml that the character's bladder fills each turn.")
        (bladder/need-to-potty-limit
            :initarg :bladder/need-to-potty-limit
            :initform 300
            :accessor bladder/need-to-potty-limit-of
            :type number
            :documentation "How full the bladder needs to be before the character needs to go")
        (bladder/potty-dance-limit
            :initarg :bladder/potty-dance-limit
            :initform 450
            :accessor bladder/potty-dance-limit-of
            :type number
            :documentation "How full the character's bladder needs to be before the character starts doing a potty dance")
        (bladder/maximum-limit
            :initarg :bladder/maximum-limit
            :initform 600
            :accessor bladder/maximum-limit-of
            :type number
            :documentation "When the character's bladder gets this full, {s,he} wets {him,her}self")
        (bowels/contents
            :initarg :bowels/contents
            :initform 0
            :accessor bowels/contents-of
            :type number
            :documentation "Amount in cg that the character is holding in")
        (bowels/fill-rate
            :initarg :bowels/fill-rate
            :initform (* (/ 12000 24 60) 0)
            :type number
            :accessor bowels/fill-rate-of
            :documentation "Amount in cg that the character's bowels fills each turn")
        (bowels/need-to-potty-limit
            :initarg :bowels/need-to-potty-limit
            :initform 4000
            :accessor bowels/need-to-potty-limit-of
            :type number
            :documentation "How full the bowels need to be before the character needs to go")
        (bowels/potty-dance-limit
            :initarg :bowels/potty-dance-limit
            :initform 6000
            :accessor bowels/potty-dance-limit-of
            :type number
            :documentation "How full the character's bowels need to be before the character starts doing a potty dance")
        (bowels/maximum-limit
            :initarg :bowels/maximum-limit
            :initform 8000
            :accessor bowels/maximum-limit-of
            :type number
            :documentation "When the character's bowels gets this full, {he,she} messes {him,her}self")
        (moves
            :initarg :moves
            :initform ()
            :accessor moves-of
            :type list
            :documentation "list of moves the character knows")
        (exp
            :initarg :exp
            :accessor exp-of
            :initform 0
            :type number
            :documentation "How many experience points the character has")
        (base-stats
            :initarg :base-stats
            :initform (list :health 35 :attack 55 :defense 30 :energy 35)
            :accessor base-stats-of
            :type list
            :documentation "the base stats of the character")
        (iv-stats
            :initarg :iv-stats
            :initform (list :health (strong-random 16) :attack (strong-random 16) :defense (strong-random 16) :energy (strong-random 16))
            :accessor iv-stats-of
            :type list
            :documentation "iv stats of the character")
        (bitcoins
            :initarg :bitcoins
            :initform 0
            :accessor bitcoins-of
            :type number
            :documentation "Amount of ₿itcoins the character has. Not limited to a single country.")
        (inventory
            :initarg :inventory
            :initform ()
            :accessor inventory-of
            :type list
            :documentation "List of items the character has.")
        (wield
            :initarg :wield
            :initform nil
            :accessor wield-of
            :type (or wield null)
            :documentation "Item the character is weilding as a weapon"))
    (:documentation "Base class for the characters in the game"))
(defclass ally (base-character)
    ((learned-moves
         :initarg :learned-moves
         :accessor learned-moves-of
         :type list
         :initform '((100 . yadfa/moves:superglitch) (11 . yadfa/moves:kamehameha) (7 . yadfa/moves:tickle) (8 . yadfa/moves:mush))
         :documentation "Alist of moves the player learns by leveling up, first element is the level when you learn them ove, second is a symbol from the `yadfa/moves' package")
        (potty-training
            :initarg :potty-training
            :initform :last-minute
            :accessor potty-training-of
            :type (member :none :rebel :last-minute)
            :documentation "Whether this ally is potty trained. Valid values so far are :NONE, :REBEL, and LAST-MINUTE"))
    (:documentation "Team member that is not the player")
    (:default-initargs
        :base-stats (list :health 35 :attack 55 :defense 30 :energy 35)
        :name "anon"
        :level 5
        :species "fox"
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :wear (list (make-instance 'yadfa/items:diaper))
        :moves (list
                   (make-instance 'yadfa/moves:watersport)
                   (make-instance 'yadfa/moves:mudsport))))
(defmethod initialize-instance :after
    ((c base-character) &rest args)
    (declare (ignorable args))
    (setf (health-of c) (calculate-stat c :health))
    (setf (energy-of c) (calculate-stat c :energy))
    (setf (exp-of c) (calculate-level-to-exp (level-of c))))
(defclass player (base-character)
    ((position
         :initarg :position
         :initform '(0 0 0 yadfa/zones:home)
         :accessor position-of
         :type list
         :documentation "Current position in the form of `(list x y z map)'.")
        (warp-on-death-point
            :initarg :warp-on-death-point
            :accessor warp-on-death-point-of
            :type list
            :documentation "Where the player warps to when {s,}he dies")
        (pocket-map-point
            :initarg :pocket-map-point
            :initform nil
            :accessor pocket-map-point-of
            :type list
            :documentation "Contains the current location of the pocket map")
        (learned-moves
            :initarg :learned-moves
            :accessor learned-moves-of
            :type list
            :initform '((100 . yadfa/moves:superglitch) (11 . yadfa/moves:kamehameha) (7 . yadfa/moves:tickle) (8 . yadfa/moves:mush))
            :documentation "Alist of moves the player learns by leveling up, first element is the level when you learn them ove, second is a symbol from the `yadfa/moves'"))
    (:documentation "The player")
    (:default-initargs
        :base-stats (list :health 35 :attack 55 :defense 30 :energy 35)
        :name "Anon"
        :description "This is you stupid"
        :level 5
        :species "Fox"
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :wear (list (make-instance 'yadfa/items:diaper))
        :moves (list
                   (make-instance 'yadfa/moves:watersport)
                   (make-instance 'yadfa/moves:mudsport))))
(defmethod initialize-instance :after
    ((c player) &rest args)
    (declare (ignorable args))
    (setf (warp-on-death-point-of c) (position-of c)))
(defclass zone ()
    ((description
         :initarg :description
         :initform "Seems Pouar didn't make the text for this room yet, get to it you lazy fuck"
         :accessor description-of
         :type simple-string
         :documentation "room description")
        (enter-text
            :initarg :enter-text
            :initform "Seems Pouar didn't make the text for this room yet, get to it you lazy fuck"
            :accessor enter-text-of
            :type simple-string
            :documentation "Text that pops up when you enter the room")
        (name
            :initarg :name
            :initform "Mystery Zone"
            :accessor name-of
            :type simple-string
            :documentation "Name of the room")
        (props
            :initarg :props
            :initform ()
            :accessor props-of
            :type list
            :documentation "Plist of props in the room, and by `props' I mean instances of the PROP class")
        (events
            :initarg :events
            :initform ()
            :accessor events-of
            :type list
            :documentation "list of events that run when you enter a room")
        (continue-battle
            :initarg :continue-battle
            :initform nil
            :accessor continue-battle-of
            :type (or battle null)
            :documentation "A previous battle triggered by an event that you lost. Used to keep the game in a consistent state after losing")
        (underwater
            :initarg :underwater
            :initform nil
            :accessor underwaterp
            :type boolean
            :documentation "Whether this zone is underwater or not, better get some waterproof clothing if you don't want your diaper to swell up")
        (warp-points
            :initarg :warp-points
            :initform ()
            :accessor warp-points-of
            :type list
            :documentation "Plist of warp points to different maps, values are lists in the same form as the position of the player, keys are passed to the `move` function")
        (diapers-only
            :initarg :diapers-only
            :initform nil
            :accessor diapers-only-p
            :type boolean
            :documentation "Whether you're only allowed to wear diapers from the waist down here or not.")
        (locked
            :initarg :locked
            :initform nil
            :accessor lockedp
            :type (or class (and symbol (not keyword)) list null)
            :documentation "Whether this area is locked or not. contains the key needed to unlock it if locked, set to nil if it isn't locked")
        (hidden
            :initarg :hidden
            :initform nil
            :accessor hiddenp
            :type boolean
            :documentation "When true, the game pretends this room doesn't exist. This is for when certain events in the game makes certain zones disappear from the map and to avoid making them be in the exact same state as in the beginning of the game when they reappear")
        (no-puddles
            :initarg :no-puddles
            :initform nil
            :accessor no-puddles-p
            :type boolean
            :documentation "Whether you're allowed to go potty on the floor or not, violators will be diapered.")
        (enemy-spawn-list
            :initarg :enemy-spawn-list
            :initform ()
            :type list
            :accessor enemy-spawn-list-of
            :documentation "list containing what enemies might show up when you enter an area. Each entry looks like this `(:random random :max-random max-random :enemies enemies)' If RANDOM is specified, then the probability of the enemy being spawn is RANDOM/MAX-RANDOM otherwise it is 1/MAX-RANDOM"))
    (:documentation "A zone on the map"))
(defmethod initialize-instance :after
    ((c zone) &rest args)
    (declare (ignorable args))
    (iter (for (a b) on
              (gethash
                  (intern
                      (format nil
                          "INIT-HOOKS/~a"
                          (class-name (class-of c)))
                      (symbol-package (class-name (class-of c))))
                  *inithooks/zone*)
              by #'cddr)
        (declare (ignorable a))
        (funcall (coerce b 'function) c)))
(defclass stat/move ()
    ((name
         :initarg :name
         :initform :-
         :accessor name-of
         :type (or simple-string keyword)
         :documentation "name of move")
        (description
            :initarg :description
            :initform :-
            :accessor description-of
            :type (or simple-string keyword)
            :documentation "Description of move")
        (energy-cost
            :initarg :energy-cost
            :initform 0
            :type number
            :accessor energy-cost-of
            :documentation "How much energy this move costs")
        (power
            :initarg :power
            :initform 40
            :type number
            :accessor power-of
            :documentation "Attribute used to determine the damage of this attack")
        (ai-flags
            :initarg :ai-flags
            :initform ()
            :accessor ai-flags-of
            :type list
            :documentation "list containing flags that affect the behavior of the AI.")
        (attack
            :initarg :attack
            :initform '(lambda (target user self)
                           (let ((a (calculate-damage target user (power-of self))))
                               (format t "~a used ~a~%" (name-of user) (name-of self))
                               (decf (health-of target) a)
                               (format t "~a received ~a damage~%" (name-of target) a)))
            :accessor attack-of
            :type (or list function)
            :documentation "function that performs the move. TARGET is the enemy that is being attacked and USER is the one doing the attacking, SELF is the move itself"))
    (:documentation "base class of moves used in battle"))
(defclass prop ()
    ((description
         :initarg :description
         :initform "This text is just here to avoid triggering an UNBOUND_VARIABLE condition. Nothing to see here."
         :accessor description-of
         :type simple-string
         :documentation "Description of a prop")
        (name
            :initarg :name
            :initform "This text is just here to avoid triggering an UNBOUND_VARIABLE condition. Nothing to see here."
            :accessor name-of
            :type simple-string
            :documentation "Name of prop")
        (items
            :initarg :items
            :initform ()
            :accessor items-of
            :type list
            :documentation "List of items this prop has")
        (bitcoins
            :initarg :bitcoins
            :initform 0
            :accessor bitcoins-of
            :type number
            :documentation "Number of bitcoins this prop has")
        (actions
            :initarg :actions
            :initform ()
            :accessor actions-of
            :type list
            :documentation "Plist of actions who's lambda-list is `(prop &key &allow-other-keys)' that the player sees as actions they can perform with the prop, PROP is the instance that this slot belongs to"))
    (:documentation "Tangible objects in the AREA that the player can interact with"))
(defclass item ()
    ((description
         :initarg :description
         :initform :?
         :accessor description-of
         :type (or simple-string keyword)
         :documentation "item description")
        (name
            :initarg :name
            :initform :teru-sama
            :accessor name-of
            :type (or simple-string keyword)
            :documentation "item description")
        (plural-name
            :initarg :plural-name
            :initform nil
            :accessor plural-name-of
            :type (or simple-string null)
            :documentation "The plural name of item")
        (consumable
            :initarg :consumable
            :initform nil
            :accessor consumablep
            :type boolean
            :documentation "Whether this item goes away when you use it")
        (tossable
            :initarg :tossable
            :initform t
            :accessor tossablep
            :type boolean
            :documentation "Whether you can throw this item away or not")
        (sellable
            :initarg :sellable
            :initform t
            :accessor sellablep
            :type boolean
            :documentation "Whether you can sell this item or not")
        (value
            :initarg :value
            :initform 0
            :accessor value-of
            :type number
            :documentation "Worth in bitcoins")
        (ai-flags
            :initarg :ai-flags
            :initform ()
            :accessor ai-flags-of
            :type list
            :documentation "Flags that affect the AI")
        (default-move
            :initarg :default-move
            :initform (make-instance 'yadfa/moves:weapon-default)
            :accessor default-move-of
            :type stat/move
            :documentation "Default move this weapon uses")
        (wear-stats
            :initarg :wear-stats
            :initform ()
            :accessor wear-stats-of
            :type list
            :documentation "stat boost when wearing this item. Is a plist in the form of (list :attack attack :defense defense :health health :energy energy)")
        (wield-stats
            :initarg :wield-stats
            :initform ()
            :accessor wield-stats-of
            :type list
            :documentation "stat boost when weilding this item. Is a plist in the form of (list :attack attack :defense defense :health health :energy energy)")
        (special-actions
            :initarg :special-actions
            :initform ()
            :accessor special-actions-of
            :type list
            :documentation "Plist of actions that the player sees as actions with a lambda with the lambda-list `(item user &key &allow-other-keys)' they can perform with the item, ITEM is the instance that this slot belongs to, USER is the user using the item")
        (use-script
            :initarg :use-script
            :initform '()
            :accessor use-script-of
            :type (or list function)
            :documentation "Script that runs when ITEM is used on USER. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on.")
        (wield-script
            :initarg :wield-script
            :initform '()
            :accessor wield-script-of
            :type (or list function)
            :documentation "Script that runs when USER is wielding ITEM. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on.")
        (wear-script
            :initarg :wear-script
            :initform '()
            :accessor wear-script-of
            :type (or list function)
            :documentation "Script that runs when USER is wearing ITEM. The lambda list is `(ITEM USER)' where ITEM is the instance of the item and USER is the user you're using it on."))
    (:documentation "Something you can store in your inventory and use"))
(defclass consumable (item)
    ()
    (:documentation "Doesn't actually cause items to be consumable, but is there to make filtering easier"))
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
         :type list
         :documentation "A list of pairs containing the different text that describes the appearance that your diapers have on your pants based on the thickness, first one is the minimum thickness needed for the second text. the text for thicker padding must be listed first")
        (thickness
            :initarg :thickness
            :initform 1
            :accessor thickness-of
            :type number
            :documentation "the thickness of the undies in mm")
        (thickness-capacity
            :initarg :thickness-capacity
            :initform (* (expt 6 1/3) 25.4)
            :accessor thickness-capacity-of
            :type (or number (and boolean (not null)))
            :documentation "The maximum thickness of your diaper that this can fit over. T means infinite")
        (waterproof
            :initarg :waterproof
            :initform nil
            :accessor waterproofp
            :type boolean
            :documentation "Whether this prevents your diapers from swelling up in water")
        (disposable
            :initarg :disposable
            :initform nil
            :accessor disposablep
            :type boolean
            :documentation "Whether you clean this or throw it away")
        (sogginess
            :initarg :sogginess
            :initform 0
            :accessor sogginess-of
            :type number
            :documentation "sogginess")
        (sogginess-capacity
            :initarg :sogginess-capacity
            :initform 10
            :accessor sogginess-capacity-of
            :type number
            :documentation "sogginess capacity")
        (messiness
            :initarg :messiness
            :initform 0
            :accessor messiness-of
            :type number
            :documentation "messiness")
        (messiness-capacity
            :initarg :messiness-capacity
            :initform 10
            :accessor messiness-capacity-of
            :type number
            :documentation "messiness capacity")
        (mess-text
            :initarg :mess-text
            :initform (list nil nil nil)
            :accessor mess-text-of
            :type list
            :documentation "Text that comes up in the description when in the inventory, it based on messiness")
        (wet-text
            :initarg :wet-text
            :initform (list nil nil nil)
            :accessor wet-text-of
            :type list
            :documentation "Text that comes up in the description when in the inventory it based on sogginess")
        (wear-mess-text
            :initarg :wear-mess-text
            :initform ()
            :accessor wear-mess-text-of
            :type list
            :documentation "Text that comes up in the description when wearing it based on messiness")
        (wear-wet-text
            :initarg :wear-wet-text
            :initform ()
            :accessor wear-wet-text-of
            :type list
            :documentation "Text that comes up in the description when wearing it based on sogginess")
        (key
            :initarg :key
            :initform nil
            :type (or (and symbol (not keyword)) list class)
            :accessor key-of
            :documentation "Whether this piece of clothing can be locked to prevent removal. Set this to the quoted type specifier that is needed to unlock it")
        (locked
            :initarg :locked
            :initform nil
            :accessor lockedp
            :type boolean
            :documentation "Whether this clothing is locked to prevent removal")))
(defclass closed-bottoms (bottoms)
    ())
(defmethod initialize-instance :after
    ((c closed-bottoms) &rest args)
    (declare (ignorable args))
    (unless (wear-mess-text-of c)
        (setf (wear-mess-text-of c) (mess-text-of c)))
    (unless (wear-wet-text-of c)
        (setf (wear-wet-text-of c) (wet-text-of c))))
(defclass full-outfit (top bottoms)
    ())
(defclass closed-pants (closed-bottoms) ())
(defclass closed-full-outfit (full-outfit closed-pants)
    ())
(defclass onesie (full-outfit)
    ((onesie-thickness-capacity
         :initarg :onesie-thickness-capacity
         :initform (cons (* 16 25.4) t)
         :accessor onesie-thickness-capacity-of
         :type cons
         :documentation "cons of values for the thickness capacity of the onesie, first value is for when it's closed, second for when it's opened")
        (onesie-waterproof
            :initarg :onesie-waterproof
            :initform nil
            :accessor onesie-waterproof-p
            :type boolean
            :documentation "Determines whether the onesie prevents your diaper from swelling up when closed.")
        (onesie-bulge-text
            :initarg :onesie-bulge-text
            :initform (cons () ())
            :accessor onesie-bulge-text-of
            :type cons
            :documentation "A cons containing 2 lists of pairs containing the different text that describes the appearance that your diapers have on your pants based on the thickness, first one is the minimum thickness needed for the second text. the text for thicker padding must be listed first. car is the value for when it's closed, cdr is the value when it's open")))
(defclass onesie/opened (onesie)
    ())
(defclass onesie/closed (onesie closed-full-outfit)
    ())

(defmethod update-instance-for-different-class :after ((old onesie/opened) (new onesie/closed) &key)
    (setf (thickness-capacity-of new) (car (slot-value old 'onesie-thickness-capacity)))
    (setf (waterproofp new) (onesie-waterproof-p old))
    (setf (bulge-text-of new) (car (slot-value old 'onesie-bulge-text)))
    )
(defmethod update-instance-for-different-class :after ((old onesie/closed) (new onesie/opened) &key)
    (setf (thickness-capacity-of new) (cdr (slot-value old 'onesie-thickness-capacity)))
    (setf (waterproofp new) nil)
    (setf (bulge-text-of new) (cdr (slot-value old 'onesie-bulge-text))))
(defmethod initialize-instance :after
    ((c onesie/opened) &rest args)
    (declare (ignorable args))
    (setf (thickness-capacity-of c) (cdr (onesie-thickness-capacity-of c)))
    (setf (waterproofp c) nil)
    (setf (bulge-text-of c) (cdr (onesie-bulge-text-of c))))
(defmethod initialize-instance :after
    ((c onesie/closed) &rest args)
    (declare (ignorable args))
    (setf (thickness-capacity-of c) (car (onesie-thickness-capacity-of c)))
    (setf (waterproofp c) (onesie-waterproof-p c))
    (setf (bulge-text-of c) (car (onesie-bulge-text-of c))))
(defclass incontinence-product (closed-bottoms) ())
(defclass padding (incontinence-product) ())
(defclass cub-undies (incontinence-product) ())
(defclass pullon (padding) ()
    (:default-initargs :thickness (* 1/2 25.4) :thickness-capacity 40))
(defclass tabbed-briefs (padding) ()
    (:default-initargs :thickness 25.4 :thickness-capacity 80 :key 'yadfa/items:magic-diaper-key))
(defclass incontinence-pad (incontinence-product) ()
    (:default-initargs :thickness (* 1/4 25.4) :thickness-capacity 20))
(defclass undies (closed-bottoms)
    ()
    (:default-initargs :thickness-capacity (* (expt 6 1/3) 25.4)))
(defclass stuffer (incontinence-pad cub-undies)
    ())
(defclass diaper (tabbed-briefs cub-undies)
    ())
(defclass pullup (pullon cub-undies)
    ())
(defclass skirt (bottoms)
    ()
    (:default-initargs :thickness-capacity 100))
(defclass dress (full-outfit)
    ()
    (:default-initargs :thickness-capacity 100))
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
                                :lambda '(lambda
                                             (prop &rest keys &key wet mess pull-pants-down ally &allow-other-keys)
                                             (declare
                                                 (type prop prop)
                                                 (type boolean pull-pants-down)
                                                 (type (or integer null) ally)
                                                 (type (or boolean number) wet mess))
                                             (check-type prop prop)
                                             (check-type pull-pants-down boolean)
                                             (check-type ally (or integer null))
                                             (check-type wet (or boolean number))
                                             (check-type mess (or boolean number))
                                             (block nil
                                                 (when (and ally (>= ally (list-length (allies-of *game*))))
                                                     (format t "That ally doesn't exist~%")
                                                     (return))
                                                 (potty-on-toilet
                                                     prop
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
                                             (yadfa/world:wash-all-in prop)))))
    (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))

(defclass automatic-changing-table (prop) ()
    (:default-initargs
        :name "Automatic Chainging Table"
        :description "A changing table that automatically changes you"
        :actions
        (list :use
            (make-action
                :documentation "Turn it on"
                :lambda
                '(lambda
                     (prop &rest keys &key &allow-other-keys)
                     (declare (type prop prop))
                     (check-type prop prop)
                     (iter (for j in (append (list (player-of *game*)) (allies-of *game*)))
                         (let ((a (calculate-diaper-usage j)))
                             (when
                                 (and
                                     (or
                                         (>=
                                             (getf a :sogginess)
                                             (/ (getf a :sogginess-capacity) 4))
                                         (>=
                                             (getf a :messiness)
                                             (/ (getf a :messiness-capacity) 4)))
                                     (wearingp (wear-of j) 'closed-bottoms))
                                 (format t "Mechanical arms come out of the changing table and strap ~a down on the table to prevent ~a from escaping and proceeeds to change ~a~%~%"
                                     (name-of j)
                                     (if (malep j) "him" "her")
                                     (if (malep j) "him" "her"))
                                 (if (wearingp (wear-of j) 'padding)
                                     (progn
                                         (format t "~a: Hey!!! Don't change me here!!! People can see me!!! Stop!!!~%~%"
                                             (name-of j)))
                                     (progn
                                         (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%"
                                             (name-of j))))
                                 (change-the-baby j 'yadfa/items:kurikia-thick-diaper :locked t)
                                 (format t "*The machine removes ~a's soggy clothing (and any clothing that doesn't fit over the new diaper) and puts a thick diaper on ~a, then locks it to prevent the baby from removing it.*~%~%"
                                     (name-of j)
                                     (if (malep j) "him" "her"))
                                 (format t "*The machine unstraps ~a from the table and lets ~a go. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
                                     (name-of j)
                                     (if (malep j) "him" "her")
                                     (name-of j)
                                     (if (malep j) "him" "her"))
                                 (unless (position
                                             (gethash :get-diaper-locked-1 (events-of-game))
                                             (finished-events-of *game*))
                                     (format t "*~a tugs at the tabs trying to remove them, but they won't budge. Better find a solution before its too late*~%~%"
                                         (name-of j))
                                     (push
                                         (gethash :get-diaper-locked-1 (events-of-game))
                                         (finished-events-of *game*))))))))))
    (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass checkpoint (prop) ()
    (:default-initargs
        :name "Checkpoint"
        :description "You can use this to set this zone as a checkpoint so when you lose a battle, you'll warp to here rather than at the beginning of the game"
        :actions (list :set-checkpoint (make-action
                                           :documentation "Set checkpoint"
                                           :lambda '(lambda
                                                        (prop &rest keys &key &allow-other-keys)
                                                        (declare (type prop prop) (ignore prop))
                                                        (check-type prop prop)
                                                        (setf (warp-on-death-point-of (player-of *game*)) (position-of (player-of *game*)))))))
    (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass shop (prop)
    ((items-for-sale
         :initarg :items-for-sale
         :initform ()
         :type list
         :accessor items-for-sale-of
         :documentation "Quoted list of class names for sale"))
    (:default-initargs
        :name "Shop"
        :description "A place to buy crap with your bitcoins"
        :actions (list
                     :list-items-for-sale (make-action
                                              :documentation "List items for sale"
                                              :lambda '(lambda
                                                           (prop &rest keys &key &allow-other-keys)
                                                           (declare (type prop prop))
                                                           (check-type prop prop)
                                                           (shopfun (items-for-sale-of prop) :format-items t)))
                     :buy-items (make-action
                                    :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                    :lambda '(lambda
                                                 (prop &rest keys &key items &allow-other-keys)
                                                 (declare (type prop prop) (type list items))
                                                 (check-type prop prop)
                                                 (check-type items list)
                                                 (shopfun (items-for-sale-of prop)
                                                     :items-to-buy items
                                                     :user (player-of *game*))))
                     :sell-items (make-action
                                     :documentation "Sell items. ITEMS is a list of indexes where each index corrisponds to an item in your inventory"
                                     :lambda '(lambda
                                                  (prop &rest keys &key items &allow-other-keys)
                                                  (declare (type prop prop) (type list items))
                                                  (check-type prop prop)
                                                  (check-type items list)
                                                  (shopfun (items-for-sale-of prop)
                                                      :items-to-sell items
                                                      :user (player-of *game*))))))
    (:documentation "Class for shops, you can buy stuff from these."))
(defclass bed (prop) ()
    (:default-initargs
        :name "Bed"
        :description "A place to sleep and recover. Be sure to go potty so you don't wet it."
        :actions (list
                     :sleep (make-action
                                :documentation "Sleep in this bed and recover your health and energy. Be sure to go potty before you go to bed so you don't wet it"
                                :lambda '(lambda
                                             (prop &rest keys &key &allow-other-keys)
                                             (declare (type prop prop) (ignore prop))
                                             (check-type prop prop)
                                             (go-to-sleep)))))
    (:documentation "Class for beds, you can sleep in these."))
(defclass config ()
    ((full-repl
         :initarg :full-repl
         :initform nil
         :accessor full-repl-of
         :type boolean
         :documentation "Enables calling other functions in the LTK REPL besides what the player is supposed to call. The reader interns every symbol it sees, even if they are undefined, causing conflicts if you try to use a package with a symbol that had the same name as an undefined symbol the reader happened to read before. Disabling this option is the workaround.")))
(defclass enemy (base-character)
    ((exp-yield
         :initarg :exp-yield
         :initform 50
         :accessor exp-yield-of
         :type integer
         :documentation "Base exp points that player receives when this guy is defeated")
        (watersport-limit
            :initarg :watersport-limit
            :initform nil
            :accessor watersport-limit-of
            :type (or number null)
            :documentation "How close to bladder/maximum-limit the enemy is before voluntarily wetting his/her diapers. A value of nil means he'll/she'll never wet voluntarily")
        (mudsport-limit
            :initarg :mudsport-limit
            :initform nil
            :accessor mudsport-limit-of
            :type (or number null)
            :documentation "How close to bowels/maximum-limit the enemy is before voluntarily wetting his/her diapers. A value of nil means he'll/she'll never mess voluntarily")
        (watersport-chance
            :initarg :watersport-chance
            :initform 1
            :accessor watersport-chance-of
            :type number
            :documentation "when WATERSPORT-LIMIT is reached, there is a 1 in WATERSPORT-CHANCE he'll voluntarily wet himself")
        (mudsport-chance
            :initarg :mudsport-chance
            :initform 1
            :accessor mudsport-chance-of
            :type number
            :documentation "when MUDSPORT-LIMIT is reached, there is a 1 in MUDSPORT-CHANCE he'll voluntarily mess himself")
        (battle-script
            :initarg :battle-script
            :initform '(lambda (self target)
                           (let ((moves-with-health
                                     (iter
                                         (for i in (append
                                                       (list
                                                           (if (wield-of self)
                                                               (attack-of (default-move-of (wield-of self)))
                                                               (make-instance 'yadfa/moves:default)))
                                                       (moves-of self)))
                                         (when (and
                                                   (>= (energy-of self) (energy-cost-of i))
                                                   (position :ai-health-inc (ai-flags-of i)))
                                             (collect i))))
                                    (moves-can-use
                                        (iter
                                            (for i in (append
                                                          (list
                                                              (if (wield-of self)
                                                                  (attack-of (default-move-of (wield-of self)))
                                                                  (make-instance 'yadfa/moves:default)))
                                                          (moves-of self)))
                                            (when (>= (energy-of self) (energy-cost-of i))
                                                (collect i))))
                                    (move-to-use nil))
                               (cond
                                   ((and (<= (health-of self) (/ (calculate-stat self :health) 4))
                                        moves-with-health)
                                       (setf move-to-use
                                           (nth (strong-random (list-length moves-with-health)) moves-with-health))
                                       (funcall
                                           (coerce
                                               (attack-of move-to-use)
                                               'function)
                                           target
                                           self
                                           move-to-use))
                                   (t
                                       (setf move-to-use
                                           (nth (strong-random (list-length moves-can-use)) moves-can-use))
                                       (funcall
                                           (coerce
                                               (attack-of move-to-use)
                                               'function)
                                           target
                                           self
                                           move-to-use)))))
            :accessor battle-script-of
            :type (or list function)
            :documentation "script that runs when it's time for the enemy to attack and what the enemy does to attack"))
    (:default-initargs :base-stats (list :health 40 :attack 45 :defense 40 :energy 35) :level (random-from-range 2 5))
    (:documentation "Class for enemies"))
(defclass potty-enemy (enemy) ()
    (:default-initargs
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2))
    (:documentation "Class for an enemy with a bladder and bowels fill rate. This enemy may {wet,mess} {him,her}self in battle."))
(defclass battle ()
    ((members-finished
         :initarg :members-finished
         :initform ()
         :type list
         :accessor members-finished-of
         :documentation "People in your team that already moved this turn")
        (enter-battle-text
            :initarg :enter-battle-text
            :initform nil
            :type (or null simple-string)
            :accessor enter-battle-text-of
            :documentation "The text that comes up when you enter a battle")
        (enemies
            :initarg :enemies
            :initform ()
            :type list
            :accessor enemies-of
            :documentation "Enemies in battle")
        (win-events
            :initarg :win-events
            :initform ()
            :type list
            :accessor win-events-of
            :documentation "Events that trigger when you've won the battle")
        (status-conditions
            :initarg :status-conditions
            :initform ()
            :accessor status-conditions-of
            :type list
            :documentation "plist of characters who's values are a plist of conditions that go away after battle"))
    (:documentation "Class that contains the information about the battle"))
(defmethod initialize-instance :after
    ((c battle) &rest args)
    (declare (ignore args))
    (unless (enter-battle-text-of c)
        (setf
            (enter-battle-text-of c)
            (with-output-to-string (s)
                (loop for i in (enemies-of c) do
                    (format s "A Wild ~a Appeared!!!~%" (name-of i)))))))
(defclass game ()
    ((zones
         :initarg :zones
         :initform (make-hash-table :test 'equal)
         :accessor zones-of
         :type hash-table
         :documentation "Zones in the game")
        (player
            :initarg :player
            :initform (make-instance 'player)
            :type player
            :accessor player-of
            :documentation "The Player")
        (allies
            :initarg :allies
            :initform nil
            :type list
            :accessor allies-of
            :documentation "Characters that have joined you")
        (team
            :initarg :team
            :initform nil
            :type list
            :accessor team-of
            :documentation "Characters sent out to battle")
        (config
            :initarg :config
            :initform (make-instance 'config)
            :type config
            :accessor config-of
            :documentation "Configuration")
        (events
            :initarg :events
            :initform (make-hash-table)
            :type hash-table
            :accessor events-of
            :documentation "hash table containing all the events in the game")
        (finished-events
            :initarg :finished-events
            :initform ()
            :type list
            :accessor finished-events-of
            :documentation "A list containing all the events the player has finished")
        (seen-enemies
            :initarg :seen-enemies
            :initform '()
            :type list
            :accessor seen-enemies-of))
    (:documentation "Contains all the information in the game"))
(defmethod initialize-instance :after
    ((c game) &rest args)
    (declare (ignorable args))
    (setf (events-of c) *events-in-game*)
    (pushnew (player-of c) (team-of c))
    (ensure-zones c))
