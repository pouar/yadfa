;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-allies"; coding: utf-8-unix; -*-
(in-package :yadfa-allies)
(defclass slynk (playable-ally ally-last-minute-potty-training pantsable-character) ()
  (:default-initargs
   :name "Slynk"
   :male t
   :wear (let ((diaper (progn (c2mop:ensure-finalized (find-class 'yadfa-items:bandit-adjustable-diaper))
                              (c2mop:compute-default-initargs (find-class 'yadfa-items:bandit-adjustable-diaper)))))
           (list (make-instance 'yadfa-items:bandit-uniform-tunic)
                 (make-instance 'yadfa-items:thick-rubber-diaper)
                 (make-instance 'yadfa-items:bandit-adjustable-diaper
                                :sogginess (second (assoc :sogginess-capacity diaper))
                                :messiness (second (assoc :messiness-capacity diaper)))))
   :moves (loop for i in '(yadfa-moves:watersport yadfa-moves:mudsport yadfa-moves:tickle yadfa-moves:mush yadfa-moves:fart)
                collect (make-instance i))
   :species "Raccoon"
   :tail '(:medium :fur)
   :skin '(:fur)
   :description "Used to be one of the Diapered Raccoon Bandits. Was kicked out after he was forced to give the location of Pirate's Cove to the Navy. He was humiliated constantly by the Diapered Pirates until you rescued him. Is too embarrassed to admit when he as to go unless he's desperate"
   :level 5))
(defmethod fart-result-text ((user slynk) (result (eql :failure)) mess &key stream)
  (if (and (getf mess :leak-amount) (> (getf mess :leak-amount) 0))
      (f:fmt stream (name-of user) " gets a look of horror on " (if (malep user) "his" "her") " face as "
             (if (malep user) "he" "she") " ends up messing " (if (malep user) "himself" "herself")
             " and has a blowout" #\Newline)
      (f:fmt stream (name-of user) "tries to fart to relieve the pressure but ends up messing " (if (malep user) "his" "her")
             "pamps, doesn't seem to realize it wasn't a fart and just continues on in a messy diaper" #\Newline)))
(defmethod initialize-instance :after
    ((c slynk) &key (bladder/contents nil bladderp) (bowels/contents nil bowelsp) &allow-other-keys)
  (declare (ignore bladder/contents bowels/contents))
  (unless bladderp
    (setf (bladder/contents-of c)
          (random (coerce (+ (bladder/potty-desperate-limit-of c) (/ (- (bladder/potty-desperate-limit-of c) (bladder/potty-dance-limit-of c)))) 'long-float))))
  (unless bowelsp
    (setf (bowels/contents-of c)
          (random (coerce (+ (bowels/potty-desperate-limit-of c) (/ (- (bowels/potty-desperate-limit-of c) (bowels/potty-dance-limit-of c)))) 'long-float)))))
(defclass chris (playable-ally ally-rebel-potty-training) ()
  (:default-initargs
   :name "Chris"
   :male t
   :species "Fox"
   :tail '(:medium :fur)
   :skin '(:fur)
   :description "An orange fox. has gotten accustomed to being treated like a pet and will typically wear nothing but a collar, refuses to be housebroken like a good fox so he must be diapered at all times."
   :wear (loop for i in '(yadfa-items:gold-collar yadfa-items:bandit-diaper)
               collect (make-instance i))))
(defclass kristy (playable-ally ally-no-potty-training pantsable-character) ()
  (:default-initargs
   :name "Kristy"
   :male nil
   :species "Fox"
   :tail '(:medium :fur)
   :skin '(:fur)
   :description "A beautiful orange vixen who has a personality that is more like a child than an adult. Loves wearing thick diapers, can't stand pants. Has gone without diapers for so long that she has become dependent on them."
   :wear (loop for i in '(yadfa-items:toddler-dress yadfa-items:bandit-female-diaper)
               collect (make-instance i))))
(defclass furry (playable-ally ally-silent-potty-training pantsable-character) ()
  (:default-initargs
   :name "Furry"
   :male t
   :species "Fox"
   :tail '(:medium :fur)
   :skin '(:fur)
   :description "A fox that likes to wear a fursuit. Doesn't talk much. The team got him as a pet, and as a plushie."
   :wear (loop for i in '(yadfa-items:watertight-fursuit yadfa-items:kurikia-thick-cloth-diaper)
               collect (make-instance i))))
