;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass superglitch (stat/move) ()
  (:default-initargs
   :name "Superglitch"
   :description "Classic glitch move from the pokemon games, but without the undefined behavior and unwanted side effects."
   :attack '(lambda (target user self)
             (declare (ignore self))
             (format t
              "TMTRAINER ~a is frozen solid~%TMTRAINER ~a is hurt by the burn~%"
              (name-of user)
              (name-of user))
             (setf (health-of target) 0))))
(defclass watersport (stat/move) ()
  (:default-initargs
   :name "Watersport"
   :description "Soak your diapers"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                 (format t "But it failed~%")
                 (progn (wet :wetter user)
                        (format t "~a wet ~a~%" (name-of user) (if (malep user) "himself" "herself")))))))
(defclass mudsport (stat/move) ()
  (:default-initargs
   :name "Mudsport"
   :description "mess your diapers"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                 (format t "But it failed~%")
                 (progn (mess :messer user)
                        (format t "~a messed ~a~%"
                                (name-of user)
                                (if (malep user) "himself" "herself")))))))
(defclass mudbomb (stat/move) ()
  (:default-initargs
   :name "Mud Bomb"
   :description "massively mess your diapers, never fails"
   :energy-cost 5
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
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
               (format t "~a is grossed out by the smell~%" (name-of i))))))
(defclass tickle (stat/move) ()
  (:default-initargs
   :name "Tickle"
   :description "Tickle the enemy"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (getf (attributes-of target) :not-ticklish)
                 (write-line "It has no effect")
                 (progn (format t "~a starts laughing helplessly~%" (name-of target))
                        (set-status-condition 'yadfa-status-conditions:tickled target))))))
(defclass tackle (stat/move) ()
  (:default-initargs
   :name "Tackle"
   :description "Tackles the enemy"))
