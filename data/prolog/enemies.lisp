;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass catchable-enemy (enemy)
  ((catch-chance
    :initarg :catch-chance
    :accessor catch-chance-of
    :initform (lambda (enemy)
                (let ((rate 1)) ; number between 0 and 1
                  (/ (* (- (* 3 (calculate-stat enemy :health)) (* 2 (health-of enemy))) rate)
                     (* 3 (calculate-stat enemy :health)))))
    :documentation "Chance of @var{CATCH-CHANCE} in 1 that this enemy can be caught where @var{CATCH-CHANCE} is a number between 0 and 1. If it is an object that can be coerced into a function, it is a function that accepts this enemy as an argument that returns a number.")))
(defclass adoptable-enemy (enemy) ())
(defclass skunk-boop-mixin (base-character) ())
(defmethod change-class-text ((class adoptable-enemy))
  (format nil "~a was adopted" (name-of class)))
