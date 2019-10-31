;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defmethod catch-method ((item enemy-catcher) (target yadfa-enemies:catchable-enemy))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (out (name-of item) " can't hold anymore enemies" :% :%))
    ((not (< (random 1.0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance-of target)))))
     (out "You failed to catch the " (name-of target) :% :%)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (out "You caught the " (name-of target) :% :%)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)
     (remf (status-conditions-of *battle*) target)

     ;; these may break the save file since functions don't serialize very well
     ;; and they will never get called after the enemy is caught, so just delete these
     (setf (yadfa-enemies:catch-chance-of target) nil
           (battle-script-of target) nil
           (default-attack-of target) nil
           (process-battle-accident-of target) nil
           (process-potty-dance-of target) nil)

     (push target (contained-enemies-of item)))))
(defmethod catch-method ((item enemy-catcher) (target yadfa-enemies:ghost))
  (out "You failed to catch " (name-of target) :% :%)
  (cond ((eq (device-health-of item) t) nil)
        ((<= (device-health-of item) 1)
         (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
        (t (decf (device-health-of item)))))
(defmethod catch-method ((item ghost-catcher) (target yadfa-enemies:ghost))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (out (name-of item) " can't hold anymore enemies" :% :%))
    ((not (< (random 1.0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance-of target)))))
     (out "You failed to catch the " (name-of target) :% :%)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (out "You caught the " (name-of target) :% :%)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)
     (remf (status-conditions-of *battle*) target)

     ;; these may break the save file since functions don't serialize very well
     ;; and they will never get called after the enemy is caught, so just delete these
     (setf (yadfa-enemies:catch-chance-of target) nil
           (battle-script-of target) nil
           (default-attack-of target) nil
           (process-battle-accident-of target) nil
           (process-potty-dance-of target) nil)

     (push target (contained-enemies-of item)))))
