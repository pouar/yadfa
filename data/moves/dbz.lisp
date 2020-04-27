;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass kamehameha (stat/move) ()
  (:default-initargs
   :name "Kamehameha Wave"
   :description "Say “Kamehameha!!!” and fire a huge burst of energy"
   :energy-cost 12
   :power 200))
(defmethod attack ((target base-character) (user base-character) (attack kamehameha))
  (let ((a (calculate-damage target user (power-of attack))))
    (format t "~a used ~a~%" (name-of user) (name-of attack))
    (format t "KAM-E-HAM-E-HA!!! *loud energy blast noise*~%OK, not as dramatic in a text based game~%")
    (decf (health-of target) a)
    a))
