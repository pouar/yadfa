;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defmacro make-instances (&rest symbols)
  `(list ,@(iter (for symbol in symbols)
                 (collect `(make-instance ',symbol)))))
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
(defclass ghost (enemy) ()
  (:default-initargs
   :name "Ghost"
   :description "Woooo, A Ghost"
   :species "Ghost"
   :male t
   :attributes (list :not-ticklish t)
   ;; the game can't tell the difference between ghosts and nonghosts when calculating the damage
   ;; Unlike Pokemon, this game's engine doesn't hardcode special treatment like `(if (ghostp) (do-ghost-stuff) (do-normal-stuff))'
   ;; so just give him infinity defense and health
   :base-stats (list :health most-positive-fixnum
                     :attack 0
                     :defense float-features:long-float-positive-infinity
                     :energy most-positive-fixnum
                     :speed 120)
   :element-types (make-instances yadfa-element-types:ghost)
   :moves (make-instances yadfa-moves:ghost-tickle yadfa-moves:ghost-mush yadfa-moves:ghost-squish)))
