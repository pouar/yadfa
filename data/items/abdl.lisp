;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass pacifier (headpiece ab-clothing) ()
  (:default-initargs
   :name "Pacifier"
   :description "A pacifier you can suck on."
   :value 250))
(defclass gold-pacifier (headpiece ab-clothing) ()
  (:default-initargs
   :name "Gold-Pacifier"
   :description "A pacifier you can suck on. Has a golden mouth shield and a ruby for a handle"
   :value 10000))
(defclass recovering-pacifier (headpiece ab-clothing) ()
  (:default-initargs
   :name "Recovering Pacifier"
   :description "A pacifier you can suck on. Recovers your health and energy when you suck on it"
   :value 5000))
(defmethod wear-script ((item recovering-pacifier) (user base-character))
  (declare (ignorable item))
  (incf (health-of user) (/ (calculate-stat user :health) 16))
  (incf (health-of user) (/ (calculate-stat user :energy) 16))
  (when (> (health-of user) (calculate-stat user :health))
    (setf (health-of user) (calculate-stat user :health)))
  (when (> (energy-of user) (calculate-stat user :energy))
    (setf (energy-of user) (calculate-stat user :energy))))
(defclass healing-pacifier (headpiece ab-clothing) ()
  (:default-initargs
   :name "Healing Pacifier"
   :description "A pacifier you can suck on. Recovers your health when you suck on it"
   :value 2500))
(defmethod wear-script ((item healing-pacifier) (user base-character))
  (declare (ignorable item))
  (incf (health-of user) (/ (calculate-stat user :health) 16))
  (when (> (health-of user) (calculate-stat user :health))
    (setf (health-of user) (calculate-stat user :health))))
(defclass energizing-pacifier (headpiece ab-clothing) ()
  (:default-initargs
   :name "Energizing Pacifier"
   :description "A pacifier you can suck on. Recovers your energy when you suck on it"
   :value 2500))
(defmethod wear-script ((item energizing-pacifier) (user base-character))
  (declare (ignorable item))
  (incf (health-of user) (/ (calculate-stat user :energy) 16))
  (when (> (energy-of user) (calculate-stat user :energy))
    (setf (energy-of user) (calculate-stat user :energy))))
(defclass blanket (item) ()
  (:default-initargs
   :name "Blanket"
   :description "A Blanket you can carry around like a toddler"
   :value 200))
(defclass plushie (item) ()
  (:default-initargs
   :name "Plushie"
   :description "A Plushie you can carry around like a toddler"
   :value 200))
