;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass superglitch (move) ()
  (:default-initargs
   :name "Superglitch"
   :description "Classic glitch move from the Pokémon games, but without the undefined behavior and unwanted side effects."))
(defmethod attack ((target base-character) (user base-character) (attack superglitch))
  (declare (ignore attack))
  (format t
          "TMTRAINER ~a is frozen solid~%TMTRAINER ~a is hurt by the burn~%"
          (name-of user)
          (name-of user))
  (setf (health-of target) 0))
(defclass watersport (move) ()
  (:default-initargs
   :name "Watersport"
   :description "Soak your diapers"))
(defmethod attack ((target base-character) (user base-character) (attack watersport))
  (declare (ignore target))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (wet :wetter user)
             (format t "~a wet ~a~%" (name-of user) (if (malep user) "himself" "herself")))))
(defclass mudsport (move) ()
  (:default-initargs
   :name "Mudsport"
   :description "mess your diapers"))
(defmethod attack ((target base-character) (user base-character) (attack mudsport))
  (declare (ignore target))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (mess :messer user)
             (format t "~a messed ~a~%"
                     (name-of user)
                     (if (malep user) "himself" "herself")))))
(defclass mudbomb (move) ()
  (:default-initargs
   :name "Mud Bomb"
   :description "massively mess your diapers, never fails"
   :energy-cost 5))
(defmethod attack ((target base-character) (user base-character) (attack mudbomb))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (write-line "But it failed."))
(defmethod attack ((target base-character) (user potty-character) (attack mudbomb))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (mess :force-fill-amount (if (< (* 30 24 (bowels/fill-rate-of user)) (bowels/maximum-limit-of user))
                               (bowels/maximum-limit-of user)
                               (* 30 24 (bowels/fill-rate-of user))) :messer user)
  (format t "~a messed ~a massively~%"
          (name-of user)
          (if (malep user) "himself" "herself"))
  (iter (for i in (if (typep user 'team-member)
                      (enemies-of *battle*)
                      (team-of *game*)))
    (set-status-condition 'yadfa-status-conditions:skunked i)
    (format t "~a is grossed out by the smell~%" (name-of i))))
(defclass tickle (move) ()
  (:default-initargs
   :name "Tickle"
   :description "Tickle the enemy"))
(defmethod attack ((target base-character) (user base-character) (attack tickle))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (if (getf (attributes-of target) :not-ticklish)
      (write-line "It has no effect")
      (progn (format t "~a starts laughing helplessly~%" (name-of target))
             (set-status-condition 'yadfa-status-conditions:laughing target))))
(defclass tackle (move) ()
  (:default-initargs
   :name "Tackle"
   :description "Tackles the enemy"))
(defclass roar (move) ()
  (:default-initargs
   :name "Roar"
   :energy-cost 2
   :description "Scares enemies into flooding themselves"))
(defmethod attack ((target base-character) (user base-character) (attack roar))
  (declare (ignore target))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (unless (iter (for i in (if (typep user 'team-member)
                              (enemies-of *battle*)
                              (team-of *game*)))
            (with j = nil)
            (when (>= (bladder/contents-of i) (bladder/need-to-potty-limit-of i))
              (format t "~a jumps and wets ~aself~%" (name-of i) (if (malep i) "him" "her"))
              (wet :wetter i)
              (set-status-condition 'yadfa-status-conditions:wetting i)
              (setf j t))
            (finally (return j)))
    (write-line "it had no effect")))
(defclass bite (move) ()
  (:default-initargs
   :name "Bite"
   :description "Bites the enemy"
   :power 80))
