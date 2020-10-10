;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defunassert yadfa-battle:fight (attack &key target friendly-target)
    (target (or null unsigned-byte type-specifier)
            friendly-target (or null unsigned-byte type-specifier)
            attack (or symbol boolean))
  "Use a move on an enemy. @var{ATTACK} is either a keyword which is the indicator to select an attack that you know, or @code{T} for default. @var{TARGET} is the index or type specifier of the enemy you're attacking. @var{FRIENDLY-TARGET} is a member on your team you're using the move on instead. Only specify either a @var{FRIENDLY-TARGET} or @var{TARGET}. Setting both might make the game's code unhappy"
  (let ((selected-target (cond (target
                                (let ((a (typecase target
                                           (unsigned-byte (nth target (enemies-of *battle*)))
                                           (type-specifier (find target (enemies-of *battle*)
                                                                 :test (lambda (o e)
                                                                         (typep e o)))))))
                                  (or a
                                      (progn
                                        (write-line "That target doesn't exist")
                                        (return-from yadfa-battle:fight)))))
                               (friendly-target
                                (let ((a (typecase friendly-target
                                           (unsigned-byte (nth friendly-target (team-of *game*)))
                                           (type-specifier (find friendly-target (team-of *game*)
                                                                 :test (lambda (o e)
                                                                         (typep e o)))))))
                                  (or a
                                      (progn
                                        (write-line "That target doesn't exist")
                                        (return-from yadfa-battle:fight)))))
                               (t (iter (for i in (enemies-of *battle*))
                                    (when (>= (health-of i) 0)
                                      (leave i)))))))
    (process-battle :attack attack :selected-target selected-target)))
(defunassert yadfa-battle:stats (&key user enemy)
    (user (or unsigned-byte null)
          enemy (or unsigned-byte null))
  "Prints the current stats in battle, essentially this game's equivalent of a health and energy bar in battle. @var{USER} is the index of the member in your team, @var{ENEMY} is the index of the enemy in battle. Set both to @code{NIL} to show the stats for everyone."
  (cond (user
         (present-stats (nth user (team-of *game*))))
        (enemy
         (present-stats (nth enemy (enemies-of *battle*))))
        (t
         (format t "Your team:~%~%")
         (iter (for i in (team-of *game*))
           (present-stats i))
         (format t "Their team:~%~%")
         (iter (for i in (enemies-of *battle*))
           (present-stats i)))))
(defun yadfa-battle:run ()
  "Run away from a battle like a coward"
  (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
         (write-line "Can't run from this battle")
         (return-from yadfa-battle:run))
        ((and (>=
               (bladder/contents-of (player-of *game*))
               (bladder/need-to-potty-limit-of (player-of *game*)))
              (>=
               (bowels/contents-of (player-of *game*))
               (bowels/need-to-potty-limit-of (player-of *game*))))
         (format t
                 "~a wet and messed ~aself in fear and ran away like a coward~%"
                 (name-of (player-of *game*))
                 (if (malep (player-of *game*))
                     "him"
                     "her"))
         (wet)
         (mess))
        ((>= (bladder/contents-of (player-of *game*)) (bladder/need-to-potty-limit-of (player-of *game*)))
         (format t "~a wet ~aself in fear and ran away like a coward~%" (name-of (player-of *game*))
                 (if (malep (player-of *game*))
                     "him"
                     "her"))
         (wet))
        ((>= (bowels/contents-of (player-of *game*)) (bowels/need-to-potty-limit-of (player-of *game*)))
         (format t "~a messed ~aself in fear and ran away like a coward~%" (name-of (player-of *game*))
                 (if (malep (player-of *game*))
                     "him"
                     "her"))
         (mess))
        (t
         (format t "~a ran away like a coward~%" (name-of (player-of *game*)))))
  (s:nix *battle*)
  (switch-user-packages))
(defunassert yadfa-battle:use-item (item &key target enemy-target)
    (item (or unsigned-byte type-specifier)
          target (or null unsigned-byte type-specifier)
          enemy-target (or null unsigned-byte type-specifier))
  "Uses an item. @var{ITEM} is an index of an item in your inventory. @var{TARGET} is an index or type specifier of a character in your team. Setting this to 0 will use it on yourself. @var{ENEMY-TARGET} is an index or type specifier of an enemy in battle if you're using it on an enemy in battle. Only specify either a @var{TARGET} or @var{ENEMY-TARGET}. Setting both might make the game's code unhappy"
  (handle-query ((selected-item (typecase item
                                  (unsigned-byte
                                   (nth item (inventory-of (player-of *game*))))
                                  (type-specifier
                                   (find item (inventory-of (player-of *game*))
                                         :test #'(lambda (type-specifier obj)
                                                   (typep obj type-specifier))))))
                 (selected-target (cond ((and target enemy-target)
                                         (format t "Only specify TARGET or ENEMY-TARGET. Not both.")
                                         (return-from yadfa-battle:use-item))
                                        (enemy-target
                                         (or (typecase enemy-target
                                               (unsigned-byte (nth enemy-target (enemies-of *battle*)))
                                               (type-specifier (find enemy-target (enemies-of *battle*)
                                                                     :test (lambda (o e)
                                                                             (typep e o)))))))
                                        (target
                                         (or (typecase target
                                               (unsigned-byte (nth target (team-of *game*)))
                                               (type-specifier (find target (team-of *game*)
                                                                     :test (lambda (o e)
                                                                             (typep e o)))))))
                                        (t (iter (for i in (enemies-of *battle*))
                                             (when (>= (health-of i) 0)
                                               (leave i)))))))
      (*query-io* ((not selected-item)
                   (item)
                   :error-text (format nil "You don't have that item~%")
                   :prompt-text "Enter a different item")
                  ((and target (not selected-target))
                   (target)
                   :error-text "That target doesn't exist"
                   :prompt-text "Enter a different TARGET")
                  ((and enemy-target (not selected-target))
                   (enemy-target)
                   :error-text "That target doesn't exist"
                   :prompt-text "Enter a different ENEMY-TARGET"))
    (process-battle
     :item selected-item
     :selected-target selected-target)))
(defunassert yadfa-battle:reload (&optional ammo-type)
    (ammo-type (or null type-specifier))
  (let* ((inventory (inventory-of (player-of *game*)))
         (user (first (turn-queue-of *battle*)))
         (user-name (name-of user))
         (weapon (wield-of user))
         (weapon-name (name-of weapon))
         (ammo-capacity (ammo-capacity-of (wield-of user)))
         (weapon-ammo-type (ammo-type-of weapon)))
    (unless weapon
      (format t "~a isn't carrying a weapon~%" user-name)
      (return-from yadfa-battle:reload))
    (unless (and weapon-ammo-type (> ammo-capacity 0))
      (format t "~a's ~a doesn't take ammo~%" user-name weapon-name)
      (return-from yadfa-battle:reload))
    (when
        (list-length-<= ammo-capacity (ammo-of weapon))
      (format t "~a's ~a is already full~%" user-name weapon-name)
      (return-from yadfa-battle:reload))
    (handle-query ((selected-ammo-type (or ammo-type
                                           (iter (for i in inventory)
                                             (when (typep i weapon-ammo-type)
                                               (leave i)))
                                           (progn (format t "~a doesn't have any ammo~%" (name-of user))
                                                  (return-from yadfa-battle:reload)))))
        (*query-io*
         ((and ammo-type (not (subtypep ammo-type weapon-ammo-type)))
          (ammo-type)
          :error-text (format nil "~a ~a doesn't take that ammo"
                              user-name
                              weapon-name)
          :prompt-text "Select different ammo")
         ((and ammo-type (iter (for i in inventory)
                           (when (typep i ammo-type)
                             (leave t))))
          (ammo-type)
          :error-text (format nil "~a doesn't have that ammo" user-name)
          :prompt-text "Select different ammo"))
      (process-battle :reload selected-ammo-type))))
