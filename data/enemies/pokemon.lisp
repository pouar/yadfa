;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass magikarp (enemy) ()
  (:default-initargs
   :name "Magikarp"
   :description "The world's weakest Pokémon until it evolves, but when it does evolve, HOLY SHIT!!!!!!"
   :species "Magikarp"
   :male (a:random-elt '(t nil))
   :bitcoins-per-level 10))
(defmethod default-attack ((target team-member) (user magikarp))
  (declare (ignore target))
  (format t "~a uses Splash, obviously it had no effect. What did you think was going to happen?" (name-of user)))
(defmethod battle-script ((self magikarp) (target base-character))
  (dispatch-default-attack (default-attack-of self) target self))
