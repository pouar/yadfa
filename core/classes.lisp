;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defmethod ms:class-persistent-slots ((self standard-object))
  (mapcar #'c2mop:slot-definition-name
          (c2mop:class-slots (class-of self))))
(defun print-slot (object slot stream)
  (if (slot-boundp object slot)
      (write (slot-value object slot) :stream stream)
      (write-string "#<unbound>" stream)))
(defclass yadfa-class ()
  ((attributes
    :initarg :attributes
    :initform '()
    :accessor attributes-of
    :type list
    :documentation "Plist of attributes which are used instead of slots for stuff that aren't shared between slots"))
  (:documentation "All the classes that are part of the game's core inherit this class"))
(defclass status-condition (yadfa-class)
  ((name
    :initarg :name
    :initform nil
    :type (or string null)
    :accessor name-of
    :documentation "name of status condition")
   (description
    :initarg :description
    :initform nil
    :type (or string null)
    :accessor description-of
    :documentation "description of status conditions")
   (target
    :initarg :target
    :initform nil
    :accessor target-of
    :documentation "Enemy target that the battle script affects")
   (accumulative
    :initarg :accumulative
    :initform 1
    :accessor accumulative-of
    :type (or unsigned-byte (eql t))
    :documentation "how many of these the user can have at a time, @code{T} if infinite")
   (battle-script
    :initarg :battle-script
    :initform (lambda (target user self)
                (declare (ignorable target user self))
                nil)
    :accessor battle-script-of
    :type coerced-function
    :documentation "function that runs at the beginning of the user's turn. @var{USER} is the user with the condition. @var{TARGET} is the enemy of said user, and @var{SELF} is the condition itself")
   (blocks-turn
    :initarg :blocks-turn
    :initform nil
    :type boolean
    :accessor blocks-turn-of
    :documentation "If @code{T} this condition prevents the player from moving")
   (duration
    :initarg :duration
    :initform t
    :accessor duration-of
    :type (or unsigned-byte (eql t))
    :documentation "How many turns this condition lasts. @code{T} means it lasts indefinitely.")
   (stat-delta
    :initarg :stat-delta
    :initform '()
    :accessor stat-delta-of
    :type list
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
    :type unsigned-byte
    :accessor priority-of
    :documentation "Unsigned integer that specifies How important this condition is to cure. Used for the AI. Lower value means more important")
   (persistent
    :initarg :persistent
    :initform nil
    :type boolean
    :accessor persistentp
    :documentation "Whether items or moves that cure statuses cure this"))
  (:documentation "Base class for all the status conditions "))
(defgeneric process-battle-accident-method (character attack item reload selected-target))
(defclass base-character (yadfa-class)
  ((name
    :initarg :name
    :initform :missingno.
    :accessor name-of
    :type (or keyword string)
    :documentation "Name of the character")
   (description
    :initarg :description
    :initform :?
    :accessor description-of
    :type (or keyword string)
    :documentation "Description of the character")
   (health
    :initarg :health
    :accessor health-of
    :type real
    :documentation "Health of the character.")
   (energy
    :initarg :energy
    :accessor energy-of
    :type real
    :documentation "Energy of the character.")
   (default-attack-power
    :initarg :default-attack-power
    :initform 40
    :type real
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
    :type coerced-function
    :documentation "The default attack when no attack is selected and no weapon is equipped")
   (level
    :initarg :level
    :initform 2
    :accessor level-of
    :type unsigned-byte
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
    :type (or keyword string)
    :documentation "Character's species.")
   (last-process-potty-time
    :initarg :last-process-potty-time
    :initform (if *game* (time-of *game*) 0)
    :accessor last-process-potty-time-of
    :type real
    :documentation "Last time process-potty was processed")
   (bladder/contents
    :initarg :bladder/contents
    :initform 0
    :type (real 0)
    :accessor bladder/contents-of
    :documentation "Amount in ml that the character is holding in in ml.")
   (bladder/fill-rate
    :initarg :bladder/fill-rate
    :initform (* (/ 2000 24 60) 0)
    :type real
    :accessor bladder/fill-rate-of
    :documentation "Amount in ml that the character's bladder fills each turn.")
   (bladder/need-to-potty-limit
    :initarg :bladder/need-to-potty-limit
    :initform 300
    :type (real 0)
    :accessor bladder/need-to-potty-limit-of
    :documentation "How full the bladder needs to be before the character needs to go")
   (bladder/potty-dance-limit
    :initarg :bladder/potty-dance-limit
    :initform 450
    :type (real 0)
    :accessor bladder/potty-dance-limit-of
    :documentation "How full the character's bladder needs to be before the character starts doing a potty dance")
   (bladder/potty-desperate-limit
    :initarg :bladder/potty-desperate-limit
    :initform 525
    :type (real 0)
    :accessor bladder/potty-desperate-limit-of
    :documentation "How full the character's bladder needs to be before the character starts begging to be taken to the bathroom")
   (bladder/maximum-limit
    :initarg :bladder/maximum-limit
    :initform 600
    :type (real 0)
    :accessor bladder/maximum-limit-of
    :documentation "When the character's bladder gets this full, @{s,he@} wets @{him,her@}self")
   (bowels/contents
    :initarg :bowels/contents
    :initform 0
    :type (real 0)
    :accessor bowels/contents-of
    :documentation "Amount in cg that the character is holding in")
   (bowels/fill-rate
    :initarg :bowels/fill-rate
    :initform (* (/ 12000 24 60) 0)
    :type (real 0)
    :accessor bowels/fill-rate-of
    :documentation "Amount in cg that the character's bowels fills each turn")
   (bowels/need-to-potty-limit
    :initarg :bowels/need-to-potty-limit
    :initform 4000
    :type (real 0)
    :accessor bowels/need-to-potty-limit-of
    :documentation "How full the bowels need to be before the character needs to go")
   (bowels/potty-dance-limit
    :initarg :bowels/potty-dance-limit
    :initform 6000
    :type (real 0)
    :accessor bowels/potty-dance-limit-of
    :documentation "How full the character's bowels need to be before the character starts doing a potty dance")
   (bowels/potty-desperate-limit
    :initarg :bowels/potty-desperate-limit
    :initform 7000
    :type (real 0)
    :accessor bowels/potty-desperate-limit-of
    :documentation "How full the character's bowels needs to be before the character starts begging to be taken to the bathroom")
   (bowels/maximum-limit
    :initarg :bowels/maximum-limit
    :initform 8000
    :type (real 0)
    :accessor bowels/maximum-limit-of
    :documentation "When the character's bowels gets this full, @{he,she@} messes @{him,her@}self")
   (moves
    :initarg :moves
    :initform ()
    :type list
    :accessor moves-of
    :documentation "list of moves the character knows")
   (exp
    :initarg :exp
    :accessor exp-of
    :initform 0
    :type (real 0)
    :documentation "How many experience points the character has")
   (base-stats
    :initarg :base-stats
    :initform (list :health 45 :attack 80 :defense 50 :energy 45 :speed 120)
    :accessor base-stats-of
    :type list
    :documentation "the base stats of the character")
   (iv-stats
    :initarg :iv-stats
    :initform (list :health (random 16) :attack (random 16) :defense (random 16) :energy (random 16) :speed (random 16))
    :accessor iv-stats-of
    :type list
    :documentation "iv stats of the character")
   (bitcoins
    :initarg :bitcoins
    :initform 0
    :type (real 0)
    :accessor bitcoins-of
    :documentation "Amount of Bitcoins the character has. Not limited to a single country.")
   (inventory
    :initarg :inventory
    :initform ()
    :type list
    :accessor inventory-of
    :documentation "List of items the character has.")
   (wield
    :initarg :wield
    :initform nil
    :accessor wield-of
    :type (or null item)
    :documentation "Item the character is wielding as a weapon")
   (process-battle-accident
    :initarg :process-battle-accident
    :initform 'process-battle-accident-method
    :type coerced-function
    :accessor process-battle-accident-of)
   (process-potty-dance
    :initarg :process-potty-dance
    :initform '(lambda (character attack item reload selected-target)
                (declare (ignore item reload selected-target))
                (when (process-potty-dance-check character attack)
                  (format t "~a is too busy doing a potty dance to fight~%" (name-of character))
                  t))
    :type coerced-function
    :accessor process-potty-dance-of))
  (:documentation "Base class for the characters in the game"))
(defmethod process-battle-accident-method ((character base-character) attack item reload selected-target)
  (declare (ignore attack item reload selected-target))
  (when (or (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
            (>= (bowels/contents-of character) (bowels/maximum-limit-of character)))
    (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
      (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
              (name-of character)
              (if (malep character) "he" "she")
              (if (malep character) "him" "her"))
      (wet :wetter character)
      (set-status-condition 'yadfa-status-conditions:wetting character))
    (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
      (format t "~a instinctively squats down as ~a accidentally messes ~aself in battle~%"
              (name-of character)
              (if (malep character) "he" "she")
              (if (malep character) "him" "her"))
      (mess :messer character)
      (set-status-condition 'yadfa-status-conditions:messing character))
    t))
(defclass team-member (base-character)
  ((skin
    :initarg :skin
    :initform '()
    :type list
    :accessor skin-of
    :documentation "attributes for the character's skin, such as whether he/she has fur or not. current supported elements are @code{:SCALES}, @code{:FUR}, and @code{:FEATHERS}")
   (tail
    :initarg :tail
    :initform nil
    :accessor tail-of
    :type list
    :documentation "attributes for the character's tail. Is @code{NIL} if the character doesn't have a tail. Takes the same syntax as the cdr of a function form with the lambda list @code{(tail-type &optional tail)}  current supported values for @var{TAIL-TYPE} are @code{:SMALL}, @code{:MEDIUM}, @code{:LARGE}, @code{:LIZARD}, @code{:BIRD-SMALL}, @code{:BIRD-LARGE}, and @code{NIL}. current supported elements for @var{TAIL} are @code{:MULTI}, @code{:SCALES}, @code{:FUR}, and @code{:FEATHERS}")
   (wings
    :initarg :wings
    :initform '()
    :type list
    :accessor wings-of
    :documentation "list of attributes for the character's wings. current supported elements are @code{:SCALES}, @code{:FUR}, and @code{:FEATHERS}"))
  (:documentation "Either the player or an ally inherits this class"))
(defclass potty-trained-team-member (team-member) ())
(defclass ally (team-member)
  ((learned-moves
    :initarg :learned-moves
    :accessor learned-moves-of
    :type list
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
(defmethod process-battle-accident-method ((character ally-no-potty-training) attack item reload selected-target)
  (declare (ignore attack item reload selected-target))
  (when (>= (bladder/contents-of character) (bladder/need-to-potty-limit-of character))
    (let ((wet-status (wet :wetter character)))
      (format t "~a wet ~aself~%" (name-of character) (if (malep character) "him" "her"))
      (when (> (getf wet-status :leak-amount) 0))
      (format t "~a leaks and leaves puddles~%" (name-of character))))
  (when (and (>= (bowels/contents-of character) (bowels/need-to-potty-limit-of character)))
    (let ((mess-status (mess :messer character)))
      (format t "~a messed ~aself~%" (name-of character) (if (malep character) "him" "her"))
      (when (> (getf mess-status :leak-amount) 0))
      (format t "~a has a blowout and leaves a mess~%" (name-of character)))))
(defclass ally-rebel-potty-training (ally) ())
(defmethod process-battle-accident-method ((character ally-rebel-potty-training) attack item reload selected-target)
  (declare (ignore item reload))
  (cond ((and (not (typep (get-move attack character)
                          'yadfa-moves:watersport))
              (>= (bladder/contents-of character) (bladder/need-to-potty-limit-of character)))
         (let ((a (make-instance 'yadfa-moves:watersport)))
           (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of character))
           (format t "*~a uses ~a instead*~%~%" (name-of character) (name-of a))
           (funcall (coerce (attack-of a) 'function) selected-target character a))
         t)
        ((and (not (typep (get-move attack character) 'yadfa-moves:mudsport))
              (>= (bowels/contents-of character) (bowels/need-to-potty-limit-of character)))
         (let ((a (make-instance 'yadfa-moves:mudsport)))
           (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of character))
           (format t "*~a uses ~a instead*~%~%" (name-of character) (name-of a))
           (funcall (coerce (attack-of a) 'function) selected-target character a))
         t)))
(defclass ally-silent-potty-training (ally potty-trained-team-member) ())
(defclass ally-last-minute-potty-training (ally potty-trained-team-member) ())
(defclass ally-feral (ally potty-trained-team-member) ())
(defmethod print-object ((obj ally) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-slot obj 'name stream)))
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
(defclass player (potty-trained-team-member pantsable-character)
  ((position
    :initarg :position
    :initform '(0 0 0 yadfa-zones:debug-map)
    :accessor position-of
    :type list
    :documentation "Current position in the form of `(list x y z map)'.")
   (warp-on-death-point
    :initarg :warp-on-death-point
    :accessor warp-on-death-point-of
    :type list
    :initform nil
    :documentation "Where the player warps to when @{s,@}he dies, same format as POSITION")
   (learned-moves
    :initarg :learned-moves
    :accessor learned-moves-of
    :type list
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
                (make-instance 'yadfa-moves:tickle))
   :tail '(:medium :fur)
   :skin '(:fur)))
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
    :type string
    :documentation "room description")
   (enter-text
    :initarg :enter-text
    :type (or string coerced-function)
    :initform "Seems Pouar didn't make the text for this room yet, get to it you lazy fuck"
    :accessor enter-text-of
    :documentation "Text that pops up when you enter the room. either a string or a function designator or lambda expression with @code{NIL} as the lambda list that returns a string.")
   (position
    :initarg :position
    :initform '()
    :type list
    :accessor position-of
    :documentation "Position of the zone. Used when we can't figure out the position of the zone ahead of time and to avoid iterating through the hash table.")
   (name
    :initarg :name
    :initform "Mystery Zone"
    :accessor name-of
    :type string
    :documentation "Name of the room")
   (props
    :initarg :props
    :initform ()
    :accessor props-of
    :type list
    :documentation #.(format nil "Plist of props in the room, and by `props' I mean instances of the @code{PROP} class

~a."
                             (xref yadfa:prop :class)))
   (events
    :initarg :events
    :initform ()
    :accessor events-of
    :type list
    :documentation "list of events that run when you enter a room")
   (continue-battle
    :initarg :continue-battle
    :initform nil
    :type list
    :accessor continue-battle-of
    :documentation "A previous battle (which is an instance of the battle class) triggered by an event that you lost. Used to keep the game in a consistent state after losing.")
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
    :documentation #.(format nil "Plist of warp points to different maps, values are lists in the same form as the position of the player, keys are passed to the @code{MOVE} function

~a."
                             (xref yadfa-world:move :function)))
   (locked
    :initarg :locked
    :initform :nil
    :accessor lockedp
    :type (or (eql :nil) type-specifier)
    :documentation "Whether this area is locked or not. contains the type specifier of the key needed to unlock it if locked, set to @code{:NIL} if it isn't locked")
   (hidden
    :initarg :hidden
    :initform nil
    :accessor hiddenp
    :type boolean
    :documentation "When true, the game pretends this room doesn't exist. This is for when certain events in the game makes certain zones disappear from the map and to avoid making them be in the exact same state as in the beginning of the game when they reappear")
   (stairs
    :initarg :stairs
    :initform '()
    :accessor stairs-of
    :type list
    :documentation "plist with the @code{:UP} and @code{:DOWN} directions as keys, is true if there are stairs there.")
   (direction-attributes
    :initarg :direction-attributes
    :initform ()
    :type list
    :accessor direction-attributes-of
    :documentation "List of attributes based on the direction rather than the zone itself")
   (can-potty
    :initarg :can-potty
    :initform '(lambda (prop &key wet mess pants-down user)
                (declare (ignore prop wet mess pants-down user))
                t)
    :type coerced-function
    :accessor can-potty-p
    :documentation "Whether you're allowed to go potty in this zone. @var{PROP} is the prop you're going potty on if any while @var{USER} is the one going potty. @var{PANTS-DOWN} is @code{T} when @var{USER} pulls his/her pants down and @var{WET} and @var{MESS} are the arguments")
   (potty-trigger
    :initarg :potty-trigger
    :initform '(lambda (had-accident user)
                (declare (ignore had-accident user))
                nil)
    :accessor potty-trigger-of
    :type coerced-function
    :documentation "Runs whenever the user goes potty, whether on purpose or by accident, arguments are the cons called @var{HAD-ACCIDENT} that gets passed from the process-potty function, and @var{USER} which is the user who did it")
   (must-wear
    :initarg :must-wear
    :initform '(t . (lambda (user)
                      (declare (ignore user))
                      t))
    :type (or cons symbol)
    :accessor must-wear-of
    :documentation #.(format nil "Used to determine whether you can enter the zone based on what you're wearing. @var{USER} is a cons with the type specifier of what you must be wearing and a lambda expression or function that runs to determine if you can enter the zone. You can also use a symbol as key for one of the values in the hash table in the @code{MUST-WEAR} slot in ~a."
                             (ref game :class)))
   (must-wear*
    :initarg :must-wear*
    :initform '(t . (lambda (user)
                      (declare (ignore user))
                      t))
    :accessor must-wear*-of
    :type (or cons symbol)
    :documentation #.(format nil "Similar to the @code{MUST-WEAR} slot but is done when you try to wear or change while still inside the zone. You can also use a symbol as key for one of the values in the hash table in the @code{MUST-WEAR*} slot in ~a."
                             (ref game :class)))
   (must-not-wear
    :initarg :must-not-wear
    :initform '(nil . (lambda (user)
                        (declare (ignore user))
                        t))
    :type (or cons symbol)
    :accessor must-not-wear-of
    :documentation #.(format nil "Used to determine whether you can enter the zone based on what you're wearing. @var{USER} is a cons with the type specifier of what you must not be wearing and a lambda expression or function that runs to determine if you can enter the zone. You can also use a symbol as key for one of the values in the hash table in the @code{MUST-NOT-WEAR} slot in ~a."
                             (ref game :class)))
   (must-not-wear*
    :initarg :must-not-wear*
    :initform '(nil . (lambda (user)
                        (declare (ignore user))
                        t))
    :type (or cons symbol)
    :accessor must-not-wear*-of
    :documentation #.(format nil "Similar to the @code{MUST-NOT-WEAR} slot but is done when you try to wear or change while still inside the zone. You can also use a symbol as key for one of the values in the hash table in the @code{MUST-NOT-WEAR*} slot in ~a."
                             (ref game :class)))
   (no-wetting/messing
    :initarg no-wetting/messing
    :initform '(lambda (user)
                (declare (ignore user))
                nil)
    :type coerced-function
    :accessor no-wetting/messing-of
    :documentation "lambda expression or function that tells you if you're allowed to wet or mess voluntarily")
   (enemy-spawn-list
    :initarg :enemy-spawn-list
    :initform ()
    :type (or symbol list)
    :accessor enemy-spawn-list-of
    :documentation "list containing what enemies might show up when you enter an area. Each entry looks like this @code{(:chance chance :enemies enemies)} If @var{RANDOM} is specified, then the probability of the enemy being spawn is @var{CHANCE} out of 1 where @var{CHANCE} is a number between 0 and 1"))
  (:documentation "A zone on the map"))
(defmethod print-object ((obj zone) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-slot obj 'position stream)
    (write-string " " stream)
    (print-slot obj 'name stream)))
(defclass stat/move (yadfa-class)
  ((name
    :initarg :name
    :initform :-
    :accessor name-of
    :type (or keyword string)
    :documentation "name of move")
   (description
    :initarg :description
    :initform :-
    :type (or keyword string)
    :accessor description-of
    :documentation "Description of move")
   (energy-cost
    :initarg :energy-cost
    :initform 0
    :type real
    :accessor energy-cost-of
    :documentation "How much energy this move costs")
   (power
    :initarg :power
    :initform 40
    :type real
    :accessor power-of
    :documentation "Number used to determine the damage of this attack")
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
                  (format t "~a received ~a damage~%" (name-of target) a)
                  a))
    :type coerced-function
    :accessor attack-of
    :documentation "function that performs the move. @var{TARGET} is the enemy that is being attacked and @var{USER} is the one doing the attacking, @var{SELF} is the move itself"))
  (:documentation "base class of moves used in battle"))
(defclass prop (yadfa-class)
  ((description
    :initarg :description
    :initform ""
    :accessor description-of
    :type string
    :documentation "Description of a prop")
   (name
    :initarg :name
    :initform ""
    :accessor name-of
    :type string
    :documentation "Name of prop")
   (placeable
    :initarg :placeable
    :initform nil
    :accessor placeablep
    :type boolean
    :documentation "Whether you can place items here")
   (items
    :initarg :items
    :initform ()
    :type list
    :accessor items-of
    :documentation "List of items this prop has")
   (bitcoins
    :initarg :bitcoins
    :initform 0
    :accessor bitcoins-of
    :type real
    :documentation "Number of bitcoins this prop has")
   (actions
    :initarg :actions
    :initform ()
    :accessor actions-of
    :type list
    :documentation "Plist of actions who's lambda-list is @code{(prop &key &allow-other-keys)} that the player sees as actions they can perform with the prop, @var{PROP} is the instance that this slot belongs to"))
  (:documentation "Tangible objects in the AREA that the player can interact with"))
(defmethod print-object ((obj prop) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (print-slot obj 'name stream)))
(defclass item (yadfa-class)
  ((description
    :initarg :description
    :initform :?
    :accessor description-of
    :type (or keyword string)
    :documentation "item description")
   (name
    :initarg :name
    :initform :teru-sama
    :accessor name-of
    :type (or keyword string)
    :documentation "item description")
   (plural-name
    :initarg :plural-name
    :initform nil
    :accessor plural-name-of
    :type (or null string)
    :documentation "The plural name of item")
   (ammo-type
    :initarg :ammo-type
    :initform nil
    :accessor ammo-type-of
    :type type-specifier
    :documentation "A type specifier specifying the type of ammo this will hold")
   (ammo
    :initarg :ammo
    :initform ()
    :accessor ammo-of
    :type list
    :documentation "List of ammo this item has")
   (ammo-power
    :initarg :ammo-power
    :initform ()
    :accessor ammo-power-of
    :type list
    :documentation "Attack base when using this as ammo.")
   (reload-count
    :initarg :reload-count
    :initform nil
    :accessor reload-count-of
    :type (or unsigned-byte null)
    :documentation "When in battle, the maximum amount of ammo the user can reload into this item per turn, if nil, then there is no limit")
   (ammo-capacity
    :initarg :ammo-capacity
    :initform 0
    :type unsigned-byte
    :accessor ammo-capacity-of
    :documentation "How much ammo this thing can hold")
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
    :type (real 0)
    :documentation "Value of item in bitcoins")
   (ai-flags
    :initarg :ai-flags
    :initform ()
    :accessor ai-flags-of
    :type list
    :documentation "List of flags that affect the AI")
   (power
    :initarg :power
    :initform 40
    :accessor power-of
    :type real
    :documentation "Attack base when used as a melee weapon")
   (cant-use-predicate
    :initarg :cant-use-predicate
    :initform '(lambda (item user &rest keys &key target action &allow-other-keys)
                (declare (ignorable item user keys target action))
                nil)
    :accessor cant-use-predicate-of
    :type coerced-function
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
    :type coerced-function
    :documentation "Script that runs when attacking with this weapon")
   (wear-stats
    :initarg :wear-stats
    :initform ()
    :accessor wear-stats-of
    :type list
    :documentation "stat boost when wearing this item. Is a plist in the form of @code{(list :attack attack :defense defense :health health :energy energy :speed speed)}")
   (wield-stats
    :initarg :wield-stats
    :initform ()
    :accessor wield-stats-of
    :type list
    :documentation "stat boost when wielding this item. Is a plist in the form of @code{(list :attack attack :defense defense :health health :energy energy :speed speed)}")
   (special-actions
    :initarg :special-actions
    :initform ()
    :accessor special-actions-of
    :type list
    :documentation "Plist of actions that the player sees as actions with a lambda with the lambda-list @code{(item user &key &allow-other-keys)} they can perform with the item, @var{ITEM} is the instance that this slot belongs to, @var{USER} is the user using the item")
   (use-script
    :initarg :use-script
    :initform '()
    :accessor use-script-of
    :type (or null coerced-function)
    :documentation "Function that runs when @var{ITEM} is used on @var{USER}. The lambda list is @code{(ITEM USER)} where @var{ITEM} is the instance of the item and @var{USER} is the user you're using it on.")
   (wield-script
    :initarg :wield-script
    :initform '()
    :type (or null coerced-function)
    :accessor wield-script-of
    :documentation "Function that runs when @var{USER} is wielding @var{ITEM}. The lambda list is @code{(ITEM USER)} where @var{ITEM} is the instance of the item and @var{USER} is the user you're using it on.")
   (wear-script
    :initarg :wear-script
    :initform '()
    :type (or null coerced-function)
    :accessor wear-script-of
    :documentation "Function that runs when @var{USER} is wearing @var{ITEM}. The lambda list is @code{(ITEM USER)} where @var{ITEM} is the instance of the item and @var{USER} is the user you're using it on."))
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
    :type list
    :accessor bulge-text-of
    :documentation "A list of pairs containing the different text that describes the appearance that your diapers have on your pants based on the thickness, first one is the minimum thickness needed for the second text. the text for thicker padding must be listed first")
   (thickness-capacity
    :initarg :thickness-capacity
    :initform (* (expt 6.0 1/3) (+ 25 2/5))
    :accessor thickness-capacity-of
    :type (or (real 0) null)
    :documentation "The maximum thickness of your diaper that this can fit over. @code{NIL} means infinite")
   (thickness-capacity-threshold
    :initarg :thickness-capacity-threshold
    :initform 50
    :type (or (real 0) null)
    :accessor thickness-capacity-threshold-of
    :documentation "How much higher than the thickness capacity the clothing can handle diaper expansion in mm before popping/tearing, @code{NIL} means it won't pop/tear")
   (key
    :initarg :key
    :initform nil
    :accessor key-of
    :type type-specifier
    :documentation "Whether this piece of clothing can be locked to prevent removal. Set this to the quoted type specifier that is needed to unlock it")
   (locked
    :initarg :locked
    :initform nil
    :accessor lockedp
    :type boolean
    :documentation "Whether this clothing is locked to prevent removal"))
  (:documentation "Clothing you wear below the waist"))
(defclass closed-bottoms (bottoms)
  ((thickness
    :initarg :thickness
    :initform 1
    :accessor thickness-of
    :type (real 0)
    :documentation "the thickness of the undies in mm")
   (waterproof
    :initarg :waterproof
    :initform nil
    :accessor waterproofp
    :type boolean
    :documentation "Whether this prevents your diapers from swelling up in water")
   (leakproof
    :initarg :leakproof
    :initform nil
    :accessor leakproofp
    :type boolean
    :documentation "Whether this diaper leaks")
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
    :type (real 0)
    :documentation "sogginess in ml")
   (sogginess-capacity
    :initarg :sogginess-capacity
    :initform 10
    :accessor sogginess-capacity-of
    :type (real 0)
    :documentation "sogginess capacity in ml")
   (messiness
    :initarg :messiness
    :initform 0
    :accessor messiness-of
    :type (real 0)
    :documentation "messiness in cg")
   (messiness-capacity
    :initarg :messiness-capacity
    :initform 10
    :accessor messiness-capacity-of
    :type (real 0)
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
    :type list
    :documentation "Plist that contains that contain the text that comes up in the description when in the inventory with the minimal sogginess as the key")
   (wear-mess-text
    :initarg :wear-mess-text
    :initform ()
    :type list
    :accessor wear-mess-text-of
    :documentation "Plist that contains the text that comes up in the description when wearing it with the minimal messiness as the key")
   (wear-wet-text
    :initarg :wear-wet-text
    :initform ()
    :type list
    :accessor wear-wet-text-of
    :documentation "Plist that contain the text that comes up in the description when wearing it with the minimal sogginess as the key"))
  (:documentation "these are stuff like pants and underwear and not skirts"))
(defclass full-outfit (top bottoms)
  ())
(defclass closed-full-outfit (full-outfit closed-bottoms)
  ())
(defclass onesie (full-outfit)
  ((onesie-thickness-capacity
    :initarg :onesie-thickness-capacity
    :initform (cons 100 nil)
    :accessor onesie-thickness-capacity-of
    :type cons
    :documentation "cons of values for the thickness capacity of the onesie, first value is for when it's closed, second for when it's opened")
   (onesie-thickness-capacity-threshold
    :initarg :onesie-thickness-capacity-threshold
    :initform (cons 5 nil)
    :type cons
    :accessor onesie-thickness-capacity-threshold-of
    :documentation "cons of values for the thickness capacity threshold of the onesie, first value is for when it's closed, second for when it's opened")
   (onesie-waterproof
    :initarg :onesie-waterproof
    :initform nil
    :type boolean
    :accessor onesie-waterproof-p
    :documentation "Boolean that determines whether the onesie prevents your diaper from swelling up when closed.")
   (onesie-bulge-text
    :initarg :onesie-bulge-text
    :initform (cons () ())
    :type cons
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
   :thickness-capacity-threshold nil
   :disposable t
   :sellable nil)
  (:documentation "these include diapers, pullups, and stuffers"))
(defclass snap-bottoms (bottoms) ()
  (:documentation "These have snaps on them so don't tear when the diaper expands but instead come apart"))
(defclass undies (clothing)
  ())
(defclass padding (incontinence-product) ()
  (:documentation "everything but stuffers"))
(defclass ab-clothing (clothing) ()
  (:documentation "clothing that is more AB than DL"))
(defclass pullup (padding) ()
  (:default-initargs
   :thickness (* 1/2 (+ 25 2/5))
   :thickness-capacity 40))
(defclass diaper (padding) ()
  (:default-initargs
   :thickness (+ 25 2/5)
   :thickness-capacity 80
   :key 'yadfa-items:magic-diaper-key))
(defclass stuffer (incontinence-product) ()
  (:default-initargs
   :thickness (* 1/4 (+ 25 2/5))
   :thickness-capacity 20))
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
(defclass pants (closed-bottoms)
  ())
(defclass enemy (base-character)
  ((exp-yield
    :initarg :exp-yield
    :initform 50
    :accessor exp-yield-of
    :type (real 0)
    :documentation "Integer that is the base exp points that player receives when this guy is defeated")
   (bitcoins-per-level
    :initarg :bitcoins-per-level
    :initform 0
    :accessor bitcoins-per-level-of
    :type (real 0)
    :documentation "Bitcoins per level that you get from this enemy per battle. Only used if the @var{BITCOINS} slot is @code{NIL}.")
   (watersport-limit
    :initarg :watersport-limit
    :initform nil
    :accessor watersport-limit-of
    :type (or null (real 0))
    :documentation "How close to @var{BLADDER/MAXIMUM-LIMIT} in ml the enemy is before voluntarily wetting his/her diapers. A value of @code{NIL} means he'll/she'll never wet voluntarily")
   (mudsport-limit
    :initarg :mudsport-limit
    :initform nil
    :accessor mudsport-limit-of
    :type (or null (real 0))
    :documentation "How close to @var{BOWELS/MAXIMUM-LIMIT} in cg the enemy is before voluntarily wetting his/her diapers. A value of @code{NIL} means he'll/she'll never mess voluntarily")
   (watersport-chance
    :initarg :watersport-chance
    :initform 1
    :accessor watersport-chance-of
    :type (real 0)
    :documentation "when @var{WATERSPORT-LIMIT} is reached, there is a 1 in @var{WATERSPORT-CHANCE} he'll voluntarily wet himself")
   (mudsport-chance
    :initarg :mudsport-chance
    :initform 1
    :type (real 0)
    :accessor mudsport-chance-of
    :documentation "when @var{MUDSPORT-LIMIT} is reached, there is a 1 in @var{MUDSPORT-CHANCE} he'll voluntarily mess himself")
   (battle-script
    :initarg :battle-script
    :type coerced-function
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
                            (funcall (coerce (attack-of move-to-use) 'function) target self move-to-use)
                            (decf (energy-of self) (energy-cost-of move-to-use)))
                           ((wield-of self)
                            (funcall (coerce (attack-script-of (wield-of self)) 'function) target self (wield-of self)))
                           (t
                            (funcall (coerce (default-attack-of self) 'function) target self)))))))
    :accessor battle-script-of
    :documentation "function that runs when it's time for the enemy to attack and what the enemy does to attack"))
  (bitcoins
   :initarg :bitcoins
   :initform nil
   :type (or (real 0) null)
   :accessor bitcoins-of
   :documentation "Amount of Bitcoins the enemy has. Not limited to a single country.")
  (:default-initargs
   :base-stats (list :health 40
                     :attack 45
                     :defense 40
                     :energy 40
                     :speed 56)
   :level (random-from-range 2 5))
  (:documentation "Class for enemies"))
(defmethod process-battle-accident-method ((character enemy) attack item reload selected-target)
  (declare (ignore attack item reload selected-target))
  (cond ((or (>= (bladder/contents-of character)
                 (bladder/maximum-limit-of character))
             (>= (bowels/contents-of character) (bowels/maximum-limit-of character)))
         (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
           (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                   (name-of character)
                   (if (malep character) "he" "she")
                   (if (malep character) "him" "her"))
           (wet :wetter character)
           (set-status-condition 'yadfa-status-conditions:wetting character))
         (when (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
           (format t "~a involuntarily squats down as ~a accidentally messes ~aself in battle~%"
                   (name-of character)
                   (if (malep character) "he" "she")
                   (if (malep character) "him" "her"))
           (mess :messer character)
           (set-status-condition 'yadfa-status-conditions:messing character))
         t)
        ((and (watersport-limit-of character)
              (<= (- (bladder/maximum-limit-of character) (bladder/contents-of character)) (watersport-limit-of character))
              (< (random (watersport-chance-of character)) 1))
         (let ((a (make-instance 'yadfa-moves:watersport)))
           (funcall (coerce (attack-of a) 'function) (player-of *game*) character a))
         t)
        ((and (mudsport-limit-of character)
              (<= (- (bowels/maximum-limit-of character) (bowels/contents-of character)) (mudsport-limit-of character))
              (< (random (mudsport-chance-of character)) 1))
         (let ((a (make-instance 'yadfa-moves:mudsport)))
           (funcall (coerce (attack-of a) 'function) (player-of *game*) character a))
         t)))
(defmethod print-object ((obj enemy) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (cond ((not (slot-boundp obj 'male))
           (print-slot obj 'male stream))
          ((slot-value obj 'male)
           (write "Male" :stream stream))
          (t (write "Female" :stream stream)))
    (write-string " " stream)
    (print-slot obj 'species stream)))
(defclass potty-enemy (enemy) ()
  (:default-initargs
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2))
  (:documentation "Class for an enemy with a bladder and bowels fill rate. This enemy may @{wet,mess@} @{him,her@}self in battle."))
(defclass pantsable-character (base-character) ())
(defclass battle (yadfa-class)
  ((turn-queue
    :initarg :turn-queue
    :initform ()
    :accessor turn-queue-of
    :type list
    :documentation "The queue of characters specifying the order of who attacks when in battle")
   (enter-battle-text
    :initarg :enter-battle-text
    :initform nil
    :accessor enter-battle-text-of
    :type (or string null)
    :documentation "The text that comes up when you enter a battle")
   (enemies
    :initarg :enemies
    :initform ()
    :accessor enemies-of
    :type list
    :documentation "List of enemies in battle")
   (win-events
    :initarg :win-events
    :initform ()
    :accessor win-events-of
    :type list
    :documentation "List of events that trigger when you've won the battle")
   (status-conditions
    :initarg :status-conditions
    :initform ()
    :accessor status-conditions-of
    :type list
    :documentation "plist of characters who's values are a plist of conditions that go away after battle")
   (fainted
    :initarg :fainted
    :initform ()
    :type list
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
  (setf (turn-queue-of c) (sort (append* (enemies-of c) (team-of *game*)) '>
                                :key (lambda (a)
                                       (calculate-stat a :speed))))
  (incf (time-of *game*)))
(defclass game (yadfa-class)
  ((zones
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Hash table of zones in the game")
   (enemy-spawn-list
    :initarg :enemy-spawn-list
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :accessor enemy-spawn-list-of
    :documentation "contains enemy spawn lists that can be reused. Use a symbol instead of a list in the enemy spawn list to use a key")
   (must-wear
    :initarg :must-wear
    :initform (make-hash-table :test #'eq)
    :accessor must-wear-of
    :type hash-table
    :documentation #.(format nil "hash table of conses that can be used with @code{MUST-WEAR} in ~a.

See @code{MUST-WEAR} in ~a."
                             (ref zone :class) (ref zone :class)))
   (must-not-wear
    :initarg :must-not-wear
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :accessor must-not-wear-of
    :documentation #.(format nil "hash table of conses that can be used with @code{MUST-NOT-WEAR} in ~a.

See @code{MUST-NOT-WEAR} in ~a."
                             (ref zone :class) (ref zone :class)))
   (must-wear*
    :initarg :must-wear*
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :accessor must-wear*-of
    :documentation #.(format nil "hash table of conses that can be used with @code{MUST-WEAR*} in ~a.

See @code{MUST-WEAR*} in ~a."
                             (ref zone :class) (ref zone :class)))
   (must-not-wear*
    :initarg :must-not-wear*
    :initform (make-hash-table :test #'eq)
    :type hash-table
    :accessor must-not-wear*-of
    :documentation #.(format nil "hash table of conses that can be used with @code{MUST-NOT-WEAR*} in ~a.

See @code{MUST-NOT-WEAR*} in ~a."
                             (ref zone :class) (ref zone :class)))
   (player%
    :initarg :player
    :initform nil
    :accessor player-of
    :type (or null player)
    :documentation "The Player, which is an instance of the player class")
   (allies
    :initarg :allies
    :initform nil
    :accessor allies-of
    :type list
    :documentation "List of characters that have joined you")
   (team
    :initarg :team
    :initform nil
    :accessor team-of
    :type list
    :documentation "List of characters sent out to battle")
   (config
    :initarg :config
    :initform (make-hash-table :test 'equal)
    :accessor config-of
    :type hash-table
    :documentation "Arbitrary Configuration")
   (time
    :initarg :time
    :initform 0
    :accessor time-of
    :type unsigned-byte
    :documentation "Turns since start of game")
   (finished-events%
    :initarg :finished-events
    :initform '()
    :type list
    :accessor finished-events-of
    :documentation "A list containing all the symbols of events the player has finished")
   (major-event
    :initarg :major-event
    :initform nil
    :accessor major-event-of
    :type symbol
    :documentation "Symbol of the current major event")
   (seen-enemies
    :initarg :seen-enemies
    :initform '()
    :type list
    :accessor seen-enemies-of)
   (event-attributes%
    :initform (make-hash-table :test 'eq)
    :type hash-table
    :documentation "Stores the event attributes of the game"))
  (:documentation "List of all the information in the game"))
