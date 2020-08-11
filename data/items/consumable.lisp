;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass bottle-of-milk (consumable) ()
  (:default-initargs
   :name "Bottle of milk"
   :description "A baby bottle filled with milk. Fills up your health and your bladder."
   :value 50
   :consumable t))
(defmethod use-script ((item bottle-of-milk) (user base-character) (target base-character))
  (incf (bladder/contents-of target) 100)
  (if (> (health-of target) 0)
      (progn (format t "~a regained health~%" (name-of target))
             (incf (health-of target) 20))
      (f:fmt t (name-of user) " makes the unconscious " (name-of target) " suckle on the " (name-of item) " like a sleeping infant" #\Newline)))
(defclass mutagen (consumable)
  ((element-types :initarg :element-types :accessor element-types-of)))
(defmethod use-script ((item mutagen) (user base-character) (target base-character))
  (let* ((types (mapcar 'coerce-element-type (element-types-of item)))
         (old (mapcar 'coerce-element-type (element-types-of target)))
         (new (union old types :key 'type-of :test 'eq))
         (difference (set-difference new old :key 'type-of :test 'eq)))
    (if difference
        (flet ((format-type (type)
                 (f:fmt t (name-of target) " gained the " (:esc
                                                           (let* ((class (class-of type))
                                                                  (name (name-of class)))
                                                             (if name (:fmt (:a name))
                                                                 (:fmt (:s (class-name class))))))
                        " type" #\Newline)))
          (setf (element-types-of target) new)
          (iter (for i in difference)
            (format-type i)))
        (f:fmt t "It had no effect on " (name-of target) #\Newline))))
(defclass antimutagen (consumable)
  ((element-types :initarg :element-types :accessor element-types-of)))
(defmethod use-script ((item antimutagen) (user base-character) (target base-character))
  (let* ((types (mapcar 'coerce-element-type (element-types-of item)))
         (old (mapcar 'coerce-element-type (element-types-of target)))
         (new (set-difference old types :key 'type-of :test 'eq))
         (difference (set-difference old new :key 'type-of :test 'eq)))
    (if difference
        (flet ((format-type (type)
                 (f:fmt t (name-of target) " lost the " (:esc
                                                         (let* ((class (class-of type))
                                                                (name (name-of class)))
                                                           (if name (:fmt (:a name))
                                                               (:fmt (:s (class-name class))))))
                        " type" #\Newline)))
          (setf (element-types-of target) new)
          (iter (for i in difference)
            (format-type i)))
        (f:fmt t "It had no effect on " (name-of target) #\Newline))))
(defclass monster-energy-drink (consumable) ()
  (:default-initargs
   :name "Monster Energy Drink"
   :description "WARNING! NOT MEANT FOR HUMAN (or furry) CONSUMPTION. Fills up your energy and your bladder."
   :value 100
   :consumable t))
(defmethod cant-use-p ((item monster-energy-drink) (user base-character) (target base-character) action &rest keys &key &allow-other-keys)
  (declare (ignorable item user keys target action))
  (when (<= (health-of target) 0)
    (format t "Does ~a look conscious enough to use that?~%" (name-of target))
    t))
(defmethod use-script ((item monster-energy-drink) (user base-character) (target base-character))
  (declare (ignore item))
  (incf (bladder/contents-of target) 175)
  (incf (energy-of target) 20))
(defclass spiked-bottle-of-milk (consumable) ()
  (:default-initargs
   :name "Spiked Bottle of milk"
   :description "A baby bottle filled with laxatives and diuretics. Fills up your bladder and bowels really quickly."
   :value 50
   :consumable t))
(defmethod use-script ((item spiked-bottle-of-milk) (user base-character) (target base-character))
  (when (<= (health-of target) 0)
    (f:fmt t (name-of user) " makes the unconscious " (name-of target) " suckle on the " (name-of item) " like a sleeping infant" #\Newline))
  (setf (bladder/contents-of target)
        (if (< (bladder/contents-of target) (bladder/need-to-potty-limit-of target))
            (- (bladder/maximum-limit-of target) (* (bladder/fill-rate-of target) 5))
            (+ (bladder/contents-of target) (bladder/potty-dance-limit-of target))))
  (setf (bowels/contents-of target)
        (if (< (bowels/contents-of target) (bowels/need-to-potty-limit-of target))
            (- (bowels/maximum-limit-of target) (* (bowels/fill-rate-of target) 5))
            (+ (bowels/contents-of target) (bowels/potty-dance-limit-of target)))))
(defclass consious-mixin (item) ())
(defmethod cant-use-p ((item consious-mixin) (user base-character) (target base-character) action &key &allow-other-keys)
  (declare (ignorable item user action))
  (when (<= (health-of target) 0)
    (format t "Does ~a look conscious enough to use that?~%" (name-of target))
    t)
  (when (>= (health-of target) (calculate-stat target :health))
    (format t "~a's health is already full~%" (name-of target))
    t))
(defclass potion (consious-mixin consumable) ()
  (:default-initargs
   :name "Potion"
   :description "Heals 20 HP"
   :value 50
   :consumable t))
(defmethod use-script ((item potion) (user base-character) (target base-character))
  (declare (ignore item))
  (incf (health-of target) 20))
(defclass revive (consumable) ()
  (:default-initargs
   :name "Revive"
   :description "Bring someone back from the dead with this"
   :value 500
   :consumable t))
(defmethod cant-use-p ((item revive) (user base-character) (target base-character) action &key &allow-other-keys)
  (declare (ignorable item user target action))
  (when (> (health-of target) 0)
    (format t "Does ~a look unconscious to you?~%" (name-of target))
    t))
(defmethod use-script ((item revive) (user base-character) (target base-character))
  (declare (ignore item))
  (incf (health-of target) 20))
(defclass cannibal-corp-meat (consious-mixin consumable) ()
  (:default-initargs
   :name "\"CANNIBAL CORP.\" Brand Meat"
   :description "Just like in the music video. Heals 50 HP."
   :value 75
   :consumable t))
(defmethod use-script ((item cannibal-corp-meat) (user base-character) (target base-character))
  (declare (ignore item))
  (incf (bowels/contents-of target) 50)
  (incf (health-of target) 50))
(defclass maximum-tomato (consious-mixin consumable) ()
  (:default-initargs
   :name "Maximum Tomato"
   :description "Restores Full HP"
   :value 50
   :consumable t))
(defmethod use-script ((item maximum-tomato) (user base-character) (target base-character))
  (declare (ignore item))
  (setf (health-of target) (calculate-stat target :health)))
(defclass holy-hand-grenade (consumable) ()
  (:default-initargs
   :name "Holy Hand Grenade of Antioch"
   :description  "And Saint Attila raised the hand grenade up on high, saying, “O Lord, bless this thy hand grenade. That with it, thou mayest blow thine enemies to tiny bits, in thy mercy” And the Lord did grin, and the people did feast upon the lambs, and sloths, and carp, and anchovies, and orangutans, and breakfast cereals, and fruit bats, and..."
   :value 200
   :consumable t))
(defmethod cant-use-p ((item holy-hand-grenade) (user base-character) (target base-character) action &key &allow-other-keys)
  (declare (ignorable item user target action))
  (unless *battle*
    (write-line "You can only use that in battle")
    t))
(defmethod use-script ((item holy-hand-grenade) (user base-character) (target base-character))
  (declare (ignore item))
  (if (or (and (typep target 'team-member) (cdr (team-of *game*)))
          (and (typep target 'enemy) (cdr (enemies-of *battle*))))
      (progn
        (format t "~a: One, Two, Five~%" (name-of target))
        (format t "~a: Three ~a~%" (name-of (if (typep target 'team-member)
                                                (or (second (member target (team-of *game*)))
                                                    (player-of *game*))
                                                (or (second (member target (enemies-of *battle*)))
                                                    (first (enemies-of *battle*)))))
                (if (malep target) "Sir" "Ma'am"))
        (format t "~a: Three!!!" (name-of target)))
      (format t "~a: One, Two, Five, I mean Three!!!" (name-of target)))
  (write-line " *throws hand grenade*")
  (write-line "*BOOM*")
  (iter (for i in (if (typep target 'team-member)
                      (enemies-of *battle*)
                      (team-of *game*)))
    (decf (health-of i) 120)))
