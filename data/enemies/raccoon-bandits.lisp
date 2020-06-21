;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass diapered-raccoon-bandit (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Diapered Raccoon Bandit"
   :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Apparently wearing (and using) diapers is extremely embarrassing for them, so they wear tunics to hide them."
   :species "Raccoon"
   :male t
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (list (make-instance 'yadfa-items:bandit-uniform-tunic)
               (make-instance 'yadfa-items:bandit-adjustable-diaper))
   :inventory (let ((a ()))
                (iter (for i from 0 to (random 5))
                  (push (make-instance 'yadfa-items:bandit-diaper) a))
                (iter (for i from 0 to (random 5))
                  (push (make-instance 'yadfa-items:bandit-adjustable-diaper) a))
                (iter (for i from 0 to (random 5))
                  (push (make-instance 'yadfa-items:bandit-female-diaper) a)))
   :bitcoins-per-level 40))
(defmethod battle-script ((self diapered-raccoon-bandit) (target base-character))
  (let ((moves-with-health (iter (for i in (moves-of self))
                             (when (and (>= (energy-of self) (energy-cost-of i)) (position :ai-health-inc (ai-flags-of i)))
                               (collect i))))
        (moves-can-use (iter (for i in (moves-of self))
                         (when (>= (energy-of self) (energy-cost-of i))
                           (collect i))))
        (move-to-use nil))
    (cond ((and (<= (health-of self) (/ (calculate-stat self :health) 4)) moves-with-health)
           (setf move-to-use (a:random-elt moves-with-health))
           (dispatch-attack (attack-of move-to-use) target self move-to-use))
          (t
           (when moves-can-use
             (setf move-to-use (a:random-elt moves-can-use)))
           (cond ((and (>= (bladder/contents-of target) (bladder/potty-dance-limit-of target)) (= (random 3) 0))
                  (format t "~a gets a grin on ~a face~%" (name-of self) (if (malep self) "his" "her"))
                  (let ((move-to-use (make-instance 'yadfa-moves:tickle)))
                    (dispatch-attack (attack-of move-to-use) target self move-to-use)))
                 ((and (> (getf (calculate-diaper-usage target) :messiness) 0) (= (random 3) 0))
                  (format t "~a gets a grin on ~a face~%" (name-of self) (if (malep self) "his" "her"))
                  (let ((move-to-use (make-instance 'yadfa-moves:mush)))
                    (dispatch-attack (attack-of move-to-use) target self move-to-use)))
                 ((and move-to-use (= (random 4) 0))
                  (dispatch-attack (attack-of move-to-use) target self move-to-use)
                  (decf (energy-of self) (energy-cost-of move-to-use)))
                 ((wield-of self)
                  (dispatch-attack (attack-script-of (wield-of self)) target self (wield-of self)))
                 (t
                  (dispatch-default-attack (default-attack-of self) target self)))))))
(defmethod process-battle-accident ((character diapered-raccoon-bandit) attack (item item) reload (selected-target base-character))
  (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
    (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
            (name-of character)
            (if (malep character) "he" "she")
            (if (malep character) "him" "her"))
    (let ((wet (wet :wetter character)))
      (when (> (getf wet :leak-amount) 0)
        (f:fmt t "A puddle starts to form at " (name-of character) "'s feet" #\Newline)))
    (set-status-condition 'yadfa-status-conditions:wetting character))
  (when (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
    (format t "~a involuntarily squats down as ~a accidentally messes ~aself in battle~%"
            (name-of character)
            (if (malep character) "he" "she")
            (if (malep character) "him" "her"))
    (let ((mess (mess :messer character)))
      (when (> (getf mess :leak-amount) 0)
        (f:fmt t (name-of character) " starts to make a mess on the floor" #\Newline)))
    (set-status-condition 'yadfa-status-conditions:messing character))
  (let ((wetting (find-if (lambda (o) (typep o 'yadfa-status-conditions:wetting))
                          (getf (status-conditions-of *battle*) character)))
        (messing (find-if (lambda (o) (typep o 'yadfa-status-conditions:messing))
                          (getf (status-conditions-of *battle*) character)))
        (teammember (find-if (lambda (o)
                               (and (typep o 'diapered-raccoon-bandit) (not (eq o character))))
                             (enemies-of *battle*))))
    (cond ((and wetting teammember (= (random 5) 0))
           (write-line "Other Raccoon: Now's not the time to go potty")
           (write-line "Flooding Raccoon Bandit: *whines*"))
          ((and messing teammember (= (random 5) 0))
           (write-line "Other Raccoon: You couldn't wait until after the battle before doing that?")
           (write-line "Messing Raccoon Bandit: *grunts*")))))
(defclass rookie-diapered-raccoon-bandit (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Rookie Diapered Raccoon Bandit"
   :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Despite how embarrassing diapers are for them, the use of toilets and pants in the gang are a privilege and not a right. The ones without these privileges have “babysitters” to keep track of them, as they're not allowed to change themselves. Despite this, they try their best to not wet and/or mess their diapers in a desperate attempt to make their situation less embarrassing."
   :species "Raccoon"
   :male t
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (list (make-instance 'yadfa-items:bandit-uniform-shirt)
               (make-instance 'yadfa-items:bandit-diaper
                              :sogginess (let ((a (random 3)))
                                           (cond ((= a 0) 0)
                                                 ((= a 1) (random-from-range 10 50))
                                                 ((= a 2) (random-from-range 300 1000))))
                              :messiness (let ((a (random 2)))
                                           (cond ((= a 0) 0)
                                                 ((= a 1) 8000)))))
   :bitcoins-per-level 20))
(defclass female-diapered-raccoon-bandit (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Female Diapered Raccoon Bandit"
   :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Apparently gender equality is non-existent in this gang, so the females have the same potty and pants privileges as the rookies, meaning none at all."
   :species "Raccoon"
   :male nil
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (list (make-instance 'yadfa-items:bandit-uniform-sports-bikini-top)
               (make-instance 'yadfa-items:bandit-female-diaper
                              :sogginess (let ((a (random 3)))
                                           (cond ((= a 0) 0)
                                                 ((= a 1) (random-from-range 10 50))
                                                 ((= a 2) (random-from-range 300 1000))))
                              :messiness (let ((a (random 2)))
                                           (cond ((= a 0) 0)
                                                 ((= a 1) 8000)))))
   :bitcoins-per-level 20))
(defclass giant-diapered-raccoon-bandit (diapered-raccoon-bandit) ()
  (:default-initargs
   :name "Giant Diapered Raccoon Bandit"
   :description "Basically we just took a Diapered Raccoon Bandit and made him bigger. Aren't we so creative at designing bosses?"
   :bitcoins-per-level 200))
(defclass catchable-raccoon-bandit (diapered-raccoon-bandit catchable-enemy) ())
(setf (get 'catchable-raccoon-bandit 'change-class-target) 'yadfa-allies:diapered-raccoon-bandit)
