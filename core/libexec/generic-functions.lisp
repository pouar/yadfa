;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defgeneric cant-use-p (item user target action &rest keys &key &allow-other-keys)
  (:documentation "Function that is used to determine if the player can use this item. Should return 2 values. A boolean indicating whether it can be used, and a plist. Current keys to the plist are :FORMAT-CONTROL and :FORMAT-ARGUMENTS which are used to override the usual dialog that shows up when the player selects an unusable item with their own message.")
  (:method (item user target (action (eql nil)) &key &allow-other-keys)
    (unless (compute-applicable-methods #'use-script (list item user target))
      (values t `(:format-control "~s has no ~s method defined" :format-arguments (,item use-script)))))
  (:method (item user target (action symbol) &key &allow-other-keys)
    (unless (getf (special-actions-of item) action)
      (values t `(:format-control "~s has no special action ~s set" :format-arguments (,item ,action))))))
(defgeneric process-battle-accident (base-character attack item reload selected-target)
  (:method ((character base-character) attack item reload selected-target)
    (declare (ignore attack item reload selected-target))
    (when (or (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
              (>= (bowels/contents-of character) (bowels/maximum-limit-of character)))
      (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
        (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                (name-of character)
                (if (malep character) "he" "she")
                (if (malep character) "him" "her"))
        (let ((wet (wet :wetter character)))
          (when (> (getf wet :leak-amount) 0)
            (f:fmt t "A puddle starts to form at " (name-of character) "'s feet" #\Newline)))
        (set-status-condition 'yadfa-status-conditions:wetting character))
      (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
        (format t "~a instinctively squats down as ~a accidentally messes ~aself in battle~%"
                (name-of character)
                (if (malep character) "he" "she")
                (if (malep character) "him" "her"))
        (let ((mess (mess :messer character)))
          (when (> (getf mess :leak-amount) 0)
            (f:fmt t (name-of character) " starts to make a mess on the floor" #\Newline)))
        (set-status-condition 'yadfa-status-conditions:messing character))
      t))
  (:method ((character ally-no-potty-training) attack (item item) reload (selected-target base-character))
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
  (:method ((character enemy) attack (item item) reload (selected-target base-character))
    (declare (ignore attack item reload selected-target))
    (let* ((male (malep character))
           (heshe (if male "he" "she"))
           (himher (if male "him" "her"))
           (name (name-of character))
           (bladder/maximum-limit (bladder/maximum-limit-of character))
           (bowels/maximum-limit (bowels/maximum-limit-of character))
           (mudsport-limit (mudsport-limit-of character))
           (watersport-limit (watersport-limit-of character))
           (bladder/contents (bladder/contents-of character))
           (bowels/contents (bowels/contents-of character)))
      (cond ((or (>= bladder/contents bladder/maximum-limit)
                 (>= bowels/contents bowels/maximum-limit))
             (when (>= bladder/contents bladder/maximum-limit)
               (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                       name
                       heshe
                       himher)
               (let ((wet (wet :wetter character)))
                 (when (> (getf wet :leak-amount) 0)
                   (f:fmt t "A puddle starts to form at " (name-of character) "'s feet" #\Newline)))
               (set-status-condition 'yadfa-status-conditions:wetting character))
             (when (>= bowels/contents bowels/maximum-limit)
               (format t "~a involuntarily squats down as ~a accidentally messes ~aself in battle~%"
                       name
                       heshe
                       himher)
               (let ((mess (mess :messer character)))
                 (when (> (getf mess :leak-amount) 0)
                   (f:fmt t (name-of character) " starts to make a mess on the floor" #\Newline)))
               (set-status-condition 'yadfa-status-conditions:messing character))
             t)
            ((and watersport-limit
                  (<= (- bladder/maximum-limit (bladder/contents-of character)) watersport-limit)
                  (< (random (watersport-chance-of character)) 1))
             (let ((a (make-instance 'yadfa-moves:watersport)))
               (attack (player-of *game*) character a))
             t)
            ((and mudsport-limit
                  (<= (- bowels/maximum-limit (bowels/contents-of character)) mudsport-limit)
                  (< (random (mudsport-chance-of character)) 1))
             (let ((a (make-instance 'yadfa-moves:mudsport)))
               (attack (player-of *game*) character a))
             t))))
  (:method ((character ally-rebel-potty-training) attack (item item) reload (selected-target ally-rebel-potty-training))
    (declare (ignore item reload))
    (cond ((and (not (typep (get-move attack character)
                            'yadfa-moves:watersport))
                (>= (bladder/contents-of character) (bladder/need-to-potty-limit-of character)))
           (let ((a (make-instance 'yadfa-moves:watersport)))
             (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of character))
             (format t "*~a uses ~a instead*~%~%" (name-of character) (name-of a))
             (attack selected-target character a))
           t)
          ((and (not (typep (get-move attack character) 'yadfa-moves:mudsport))
                (>= (bowels/contents-of character) (bowels/need-to-potty-limit-of character)))
           (let ((a (make-instance 'yadfa-moves:mudsport)))
             (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of character))
             (format t "*~a uses ~a instead*~%~%" (name-of character) (name-of a))
             (attack selected-target character a))
           t))))
(defgeneric use-script (item user target)
  (:documentation "Function that runs when @var{USER} uses @var{ITEM} on @var{TARGET}. @var{ITEM} is the instance of the item and @var{USER} and @var{TARGET} are instances of base-character"))
(defgeneric wield-script (item user)
  (:documentation "Function that runs when @var{USER} is wielding @var{ITEM}. @var{ITEM} is the instance of the item and @var{USER} is the user you're using it on.")
  (:method ((item item) (user base-character))))
(defgeneric wear-script (item user)
  (:documentation "Function that runs when @var{USER} is wearing @var{ITEM}. @var{ITEM} is the instance of the item and @var{USER} is the user you're using it on.")
  (:method ((item item) (user base-character))))
(defgeneric resolve-enemy-spawn-list (element)
  (:documentation "returns the enemy-spawn-list in the hash table (enemy-spawn-list-of *game*) if a symbol or itself if a list")
  (:method ((element symbol)) (gethash element (enemy-spawn-list-of *game*)))
  (:method ((element list)) element)
  (:method ((element zone)) (resolve-enemy-spawn-list (enemy-spawn-list-of element))))
(defgeneric resolve-team-npc-spawn-list (element)
  (:documentation "returns the team-npc-spawn-list in the hash table (team-npc-spawn-list-of *game*) if a symbol or itself if a list")
  (:method ((element symbol)) (gethash element (team-npc-spawn-list-of *game*)))
  (:method ((element list)) element)
  (:method ((element zone)) (resolve-team-npc-spawn-list (team-npc-spawn-list-of element))))
(defgeneric attack (target user attack)
  (:documentation #.(f:fmt nil "Method run when attacking. @var{ATTACK} is @code{NIL} when it is the default attack without any weapons. is an instance of " (ref move :class) " when it is an attack using a move and is an instance of " (ref item :class) " when that instance is being used as a weapon"))
  (:method ((target base-character) (user base-character) (attack null))
    (declare (ignore attack))
    (let ((a (calculate-damage target user (default-attack-power-of user))))
      (format t "~a attacks ~a~%" (name-of user) (name-of target))
      (decf (health-of target) a)
      (format t "~a received ~a damage~%" (name-of target) a)
      a))
  (:method ((target base-character) (user base-character) (attack move))
    (let ((a (calculate-damage target user attack)))
      (format t "~a used ~a~%" (name-of user) (name-of attack))
      (decf (health-of target) a)
      (format t "~a received ~a damage~%" (name-of target) a)
      a))
  (:method ((target base-character) (user base-character) (item item))
    (declare (ignorable target user item))
    (let ((a (calculate-damage target user
                               (if (first (ammo-of item))
                                   (ammo-power-of (first (ammo-of item)))
                                   (power-of item)))))
      (format t "~a whacks ~a with ~a ~a~%"
              (name-of user)
              (name-of target)
              (if (malep user) "his" "her")
              (name-of item))
      (decf (health-of target) a)
      (format t "~a received ~a damage~%" (name-of target) a))))
(defgeneric battle-script (npc target)
  (:documentation #.(f:fmt nil "function that runs when it's time for @var{NPC} to attack @var{TARGET} and what @var{NPC} does to attack. Basically the \"AI\""))
  (:method ((self npc) (target base-character))
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
         (setf move-to-use (a:random-elt moves-with-health))
         (attack target self move-to-use))
        (t
         (when moves-can-use
           (setf move-to-use (a:random-elt moves-can-use)))
         (cond ((and moves-can-use (= (random 2) 0))
                (attack target self move-to-use)
                (decf (energy-of self) (energy-cost-of move-to-use)))
               ((wield-of self)
                (attack target self (wield-of self)))
               (t
                (attack target self nil))))))))
(defgeneric condition-script (user condition)
  (:documentation #.(f:fmt nil "Function that runs at the beginning of each turn @var{USER} is the character who has the @var{CONDITION}. @var{CONDITION} is a " (ref status-condition :class)))
  (:method ((user base-character) (condition status-condition))))
(defgeneric toggle-onesie% (onesie))
(defgeneric toggle-onesie (onesie clothes user))
;;; Wish the API I made for this wasn't so complex, but I wasn't sure how to make it simple and still retain the functionality
(defgeneric get-babyish-padding (user))
(defgeneric get-process-potty-action-type (user type had-accident)
  (:method ((user ally-last-minute-potty-training) (type (eql :wet)) had-accident)
    (cond ((and
            (car had-accident)
            (> (getf (car had-accident) :wet-amount) 0))
           :had-accident)
          ((>=
            (bladder/contents-of user)
            (bladder/potty-desperate-limit-of user))
           :desparate)
          ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
           :potty-dance)))
  (:method ((user ally-last-minute-potty-training) (type (eql :mess)) had-accident)
    (cond ((and
            (cdr had-accident)
            (> (getf (cdr had-accident) :mess-amount) 0))
           :had-accident)
          ((>=
            (bowels/contents-of user)
            (bowels/potty-desperate-limit-of user))
           :desparate)
          ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
           :potty-dance)))
  (:method ((user ally) (type (eql :wet)) had-accident)
    (when (and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
      :had-accident))
  (:method ((user ally) (type (eql :mess)) had-accident)
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
      :had-accident))
  (:method ((user player) (type (eql :wet)) had-accident)
    (cond ((and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
           :had-accident)
          ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
           :potty-dance)
          ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
           :need-to-potty)))
  (:method ((user player) (type (eql :mess)) had-accident)
    (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
           :had-accident)
          ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
           :potty-dance)
          ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
           :need-to-potty))))
(defgeneric output-process-potty-text (user padding type action had-accident &key stream))
(defgeneric coerce-element-type (element)
  (:method ((element-type (eql nil)))
    nil)
  (:method ((element-type symbol))
    (make-instance element-type))
  (:method ((element-type element-type))
    element-type))
(defgeneric type-match (source target)
  (:documentation "Used to determine the effectiveness of element type @var{SOURCE} against element type @var{TARGET}. Valid return values are @code{NIL}, @code{:SUPER-EFFECTIVE}, @code{:NOT-VERY-EFFECTIVE}, and @code{:NO-EFFECT}, which represent the effectiveness")
  (:method (source target) (type-match (coerce-element-type source) (coerce-element-type target)))
  (:method ((source element-type) (target element-type)) nil)
  (:method ((source (eql nil)) target)
    nil)
  (:method (source (target (eql nil)))
    nil))
(defgeneric bladder/fill-rate (user)
  (:method ((user bladder-character))
    (* (bladder/fill-rate-of user) (bladder/fill-rate/multiplier-of user)))
  (:method ((user base-character))
    0))
(defgeneric bowels/fill-rate (user)
  (:method ((user bowels-character))
    (* (bowels/fill-rate-of user) (bowels/fill-rate/multiplier-of user)))
  (:method ((user base-character))
    0))
(defgeneric fill-bladder (user &key &allow-other-keys)
  (:method ((user bladder-character) &key (times 1) &allow-other-keys)
    (iter (for i from 1 to times)
      (with multiplier = (bladder/fill-rate/multiplier-of user))
      (with fill-rate = (bladder/fill-rate-of user))
      (with cooldown = (bladder/fill-rate/cooldown-of user))
      (with bladder = (bladder/contents-of user))
      (incf bladder (* fill-rate multiplier))
      (cond ((> (- multiplier cooldown) 1) (decf multiplier cooldown))
            ((< (+ multiplier cooldown) 1) (incf multiplier cooldown))
            (t (setf multiplier 1)))
      (finally (setf (bladder/contents-of user) bladder
                     (bladder/fill-rate/multiplier-of user) multiplier))))
  (:method ((user base-character) &key &allow-other-keys)
    0))
(defgeneric fill-bowels (user &key &allow-other-keys)
  (:method ((user bowels-character) &key (times 1) &allow-other-keys)
    (iter (for i from 1 to times)
      (with multiplier = (bowels/fill-rate/multiplier-of user))
      (with fill-rate = (bowels/fill-rate-of user))
      (with cooldown = (bowels/fill-rate/cooldown-of user))
      (with bowels = (bowels/contents-of user))
      (incf bowels (* fill-rate multiplier))
      (cond ((> (- multiplier cooldown) 1) (decf multiplier cooldown))
            ((< (+ multiplier cooldown) 1) (incf multiplier cooldown))
            (t (setf multiplier 1)))
      (finally (setf (bowels/contents-of user) bowels
                     (bowels/fill-rate/multiplier-of user) multiplier))))
  (:method ((user base-character) &key &allow-other-keys)
    0))
(defgeneric fart (user)
  (:method ((user bowels-character) &aux (fart-count (fart-count-of user)) (bowels (bowels/contents-of user))
                                      (maximum-limit (bowels/maximum-limit-of user)) (rate (bowels/fill-rate-of user)))
    (cond ((< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
           :cant-go)
          ((= 0 (random (expt 2 fart-count)))
           (decf (bowels/contents-of user)
                 (cond ((>= bowels (/ maximum-limit 2))
                        (/ maximum-limit (expt 2 (+ 2 fart-count))))
                       ((> (- bowels (* (/ 100 (expt 2 fart-count)) rate)) 0)
                        (* (/ 100 (expt 2 fart-count)) rate))
                       (t bowels)))
           :success)
          (t (values :fail (mess :messer user))))))
(defgeneric fart-result-text (user result mess &key stream)
  (:method ((user bowels-character) (result (eql :cant-go)) mess &key (stream *standard-output*))
    (f:fmt stream (name-of user) " Doesn't have to go" #\Newline))
  (:method ((user bowels-character) (result (eql :success)) mess &key (stream *standard-output*))
    (f:fmt stream (name-of user) " farts to relieve the pressure" #\Newline))
  (:method ((user bowels-character) (result (eql :failure)) mess &key (stream *standard-output*))
    (f:fmt stream (name-of user) "tries to fart to relive the pressure then gets a look of horror on " (if (malep user) "his" "her") " face as "
           (if (malep user) "he" "she") " ends up messing " (if (malep user) "himself" "herself") " instead" #\Newline)))
