;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass superglitch (damage-move) ()
  (:default-initargs
   :name "Superglitch"
   :description "Classic glitch move from the Pokémon games, but without the undefined behavior and unwanted side effects."))
(defmethod attack :around ((target base-character) (user base-character) (attack superglitch)
                           &aux (name (name-of user)))
  (declare (ignore attack))
  (format t
          "TMTRAINER ~a is frozen solid~%TMTRAINER ~a is hurt by the burn~%"
          name
          name)
  (setf (health-of target) 0))
(progn
  (defclass watersport (wet-move-mixin) ()
    (:default-initargs
     :name "Watersport"
     :description "Soak your diapers"
     :element-types '(#1=#.(make-instance 'yadfa-element-types:abdl))))
  (defclass mudsport (mess-move-mixin) ()
    (:default-initargs
     :name "Mudsport"
     :description "mess your diapers"
     :element-types '(#1#)))
  (defclass mudbomb (mess-move-mixin debuff) ()
    (:default-initargs
     :name "Mud Bomb"
     :description "massively mess your diapers, never fails"
     :energy-cost 5
     :element-types '(#1# #.(make-instance 'yadfa-element-types:poison)))))
(defmethod attack ((target base-character) (user base-character) (attack watersport)
                   &aux (name (name-of user)))
  (declare (ignore target))
  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (wet :wetter user)
             (format t "~a wet ~a~%" name (if (malep user) "himself" "herself")))))
(defmethod attack ((target base-character) (user base-character) (attack mudsport)
                   &aux (name (name-of user)))
  (declare (ignore target))
  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (mess :messer user)
             (format t "~a messed ~a~%"
                     name
                     (if (malep user) "himself" "herself")))))
(defmethod attack ((target base-character) (user base-character) (attack mudbomb))
  (write-line "But it failed."))
(defmethod attack ((target base-character) (user bowels-character) (attack mudbomb)
                   &aux (bowels/fill-rate (* 30 24 (bowels/fill-rate-of user))) (bowels/maximum-limit (bowels/maximum-limit-of user))
                     (name (name-of user)))
  (mess :force-fill-amount (if (< bowels/fill-rate bowels/maximum-limit) bowels/maximum-limit bowels/fill-rate) :messer user)
  (format t "~a messed ~a massively~%"
          name
          (if (malep user) "himself" "herself"))
  (iter (for i in (if (typep user 'team-member)
                      (enemies-of *battle*)
                      (team-of *game*)))
    (set-status-condition 'yadfa-status-conditions:skunked i)
    (format t "~a is grossed out by the smell~%" (name-of i))))
(defclass tickle (move debuff) ()
  (:default-initargs
   :name "Tickle"
   :description "Tickle the enemy"))
(defmethod attack ((target base-character) (user base-character) (attack tickle))
  (if (getf (attributes-of target) :not-ticklish)
      (write-line "It has no effect")
      (progn (format t "~a starts laughing helplessly~%" (name-of target))
             (set-status-condition 'yadfa-status-conditions:laughing target))))
(defclass tackle (damage-move) ()
  (:default-initargs
   :name "Tackle"
   :description "Tackles the enemy"
   :element-types '(#.(make-instance 'yadfa-element-types:normal))))
(defclass roar (move debuff) ()
  (:default-initargs
   :name "Roar"
   :energy-cost 2
   :description "Scares enemies into flooding themselves"))
(defmethod attack ((target base-character) (user base-character) (attack roar))
  (declare (ignore target))
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
(defclass bite (damage-move) ()
  (:default-initargs
   :name "Bite"
   :description "Bites the enemy"
   :power 80
   :element-types '(#.(make-instance 'yadfa-element-types:dark))))
(defclass scratch (damage-move) ()
  (:default-initargs
   :name "Scratch"
   :description "Scratches the enemy"
   :power 40))
