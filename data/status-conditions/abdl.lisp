;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-status-conditions"; coding: utf-8-unix; -*-
(in-package :yadfa-status-conditions)
(defclass wetting (status-condition)
  ()
  (:default-initargs
   :name "Wetting"
   :description "User is currently wetting himself/herself"
   :duration 1
   :blocks-turn t))
(defmethod condition-script ((user base-character) (self wetting) (battle (eql t)))
  (declare (ignore self))
  (format t "~a is too busy wetting ~aself to fight~%" (name-of user) (if (malep user) "his" "her"))
  (setf (bladder/contents-of user) 0))
(defclass messing (status-condition)
  ()
  (:default-initargs
   :name "Messing"
   :description "User is currently messing himself/herself"
   :duration 1
   :blocks-turn t))
(defmethod condition-script ((user base-character) (self messing) (battle (eql t)))
  (declare (ignore self))
  (format t "~a is too busy messing ~aself to fight~%" (name-of user) (if (malep user) "his" "her"))
  (setf (bowels/contents-of user) 0))
(defclass mushed (status-condition)
  ()
  (:default-initargs
   :name "Mushed"
   :description "User's messy diaper has been mushed"
   :duration t
   :stat-multiplier (list :speed 1/2)
   :blocks-turn t))
(defmethod condition-script ((user base-character) (self mushed) (battle (eql t)))
  (cond ((<= (getf (calculate-diaper-usage user) :messiness) 0)
         (setf (status-conditions-of user) (remove self (status-conditions-of user))))
        ((< (random 4) 1)
         (format t "~a is too busy grabbing the back of ~a diaper trying to unmush it to fight~%" (name-of user) (if (malep user) "his" "her"))
         (setf (blocks-turn-of self) t))
        (t (setf (blocks-turn-of self) nil))))
(defclass pantsed (status-condition)
  ()
  (:default-initargs
   :name "Pantsed"
   :description "The user has been pantsed revealing his padding to the world"
   :curable t))
(defclass laughing (status-condition)
  ()
  (:default-initargs
   :name "Laughing"
   :description "User is laughing"
   :duration 1
   :blocks-turn t))
(defmethod condition-script ((user base-character) (self laughing) (battle (eql t)))
  (declare (ignore self))
  (format t "~a is too busy laughing to fight~%" (name-of user))
  (when (or (>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
            (and (>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user)) (= 0 (random 5))))
    (format t "~a~%"
            (cond ((filter-items (wear-of user) 'diaper)
                   (format nil "~a starts wetting ~a diapers~%" (name-of user) (if (malep user) "his" "her")))
                  ((filter-items (wear-of user) '(and pullup ab-clothing-mixin))
                   (format nil "The little pictures on the front of ~a's pullups start fading~%" (name-of user)))
                  ((filter-items (wear-of user) 'pullup)
                   (format nil "~a starts wetting ~a pullons~%" (name-of user) (if (malep user) "his" "her")))
                  (t
                   (format nil "~a starts wetting ~aself~%" (name-of user) (if (malep user) "him" "her")))))
    (set-status-condition 'wetting user)
    (wet :wetter user)))
