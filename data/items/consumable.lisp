;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass bottle-of-milk (consumable) ()
  (:default-initargs
   :name "Bottle of milk"
   :description "A baby bottle filled with milk. Fills up your health and your bladder."
   :value 50
   :consumable t
   :use-script '(lambda (item user)
                 (incf (bladder/contents-of user) 100)
                 (if (> (health-of user) 0)
                     (progn (format t "~a regained health~%" (name-of user))
                            (incf (health-of user) 20))
                     (format t "You make the unconscious ~a suckle on the ~a like a sleeping infant~%" (name-of user) (name-of item))))))
(defclass monster-energy-drink (consumable) ()
  (:default-initargs
   :name "Monster Energy Drink"
   :description "WARNING! NOT MEANT FOR HUMAN (or furry) CONSUMPTION. Fills up your energy and your bladder."
   :value 100
   :consumable t
   :cant-use-predicate '(lambda (item user &rest keys &key target action &allow-other-keys)
                         (declare (ignorable item user keys target action))
                         (when (<= (health-of target) 0)
                           (format t "Does ~a look conscious enough to use that?~%" (name-of target))))
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (incf (bladder/contents-of user) 175)
                 (incf (energy-of user) 20))))
(defclass spiked-bottle-of-milk (consumable) ()
  (:default-initargs
   :name "Spiked Bottle of milk"
   :description "A baby bottle filled with laxatives and diuretics. Fills up your bladder and bowels really quickly."
   :value 50
   :consumable t
   :use-script '(lambda (item user)
                 (when (<= (health-of user) 0)
                   (format t "You make the unconscious ~a suckle on the ~a like a sleeping infant~%" (name-of user) (name-of item)))
                 (setf (bladder/contents-of user)
                  (if (< (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                      (- (bladder/maximum-limit-of user) (* (bladder/fill-rate-of user) 5))
                      (+ (bladder/contents-of user) (bladder/potty-dance-limit-of user))))
                 (setf (bowels/contents-of user)
                  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                      (- (bowels/maximum-limit-of user) (* (bowels/fill-rate-of user) 5))
                      (+ (bowels/contents-of user) (bowels/potty-dance-limit-of user)))))))
(defclass potion (consumable) ()
  (:default-initargs
   :name "Potion"
   :description "Heals 20 HP"
   :value 50
   :consumable t
   :cant-use-predicate '(lambda (item user &rest keys &key target action &allow-other-keys)
                         (declare (ignorable item user keys action))
                         (when (<= (health-of target) 0)
                           (format t "Does ~a look conscious enough to use that?~%" (name-of target)))
                         (when (>= (health-of target) (calculate-stat target :health))
                           (format t "~a's health is already full~%" (name-of target))))
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (incf (health-of user) 20))))
(defclass revive (consumable) ()
  (:default-initargs
   :name "Revive"
   :description "Bring someone back from the dead with this"
   :value 500
   :consumable t
   :cant-use-predicate '(lambda (item user &rest keys &key target action &allow-other-keys)
                         (declare (ignorable item user keys target action))
                         (when (> (health-of target) 0)
                           (format t "Does ~a look unconscious to you?~%" (name-of target))))
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (incf (health-of user) 20))))
(defclass cannibal-corp-meat (consumable) ()
  (:default-initargs
   :name "\"CANNIBAL CORP.\" Brand Meat"
   :description "Just like in the music video. Heals 50 HP."
   :value 75
   :consumable t
   :cant-use-predicate '(lambda (item user &rest keys &key target action &allow-other-keys)
                         (declare (ignorable item user keys target action))
                         (when (<= (health-of target) 0)
                           (format t "Does ~a look conscious enough to use that?~%" (name-of target)))
                         (when (>= (health-of target) (calculate-stat target :health))
                           (format t "~a's health is already full~%" (name-of target))))
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (incf (bowels/contents-of user) 500)
                 (incf (health-of user) 50))))
(defclass maximum-tomato (consumable) ()
  (:default-initargs
   :name "Maximum Tomato"
   :description "Restores Full HP"
   :value 50
   :consumable t
   :cant-use-predicate '(lambda (item user &rest keys &key target action &allow-other-keys)
                         (declare (ignorable item user keys target action))
                         (when (<= (health-of target) 0)
                           (format t "Does ~a look conscious enough to use that?~%" (name-of target)))
                         (when (>= (health-of target) (calculate-stat target :health))
                           (format t "~a's health is already full~%" (name-of target))))
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (setf (health-of user) (calculate-stat user :health)))))
