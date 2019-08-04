;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass magikarp (enemy) ()
  (:default-initargs
   :name "Magikarp"
   :description "The world's weakest pokemon until it evolves, but when it does evolve, HOLY SHIT!!!!!!"
   :species "Magikarp"
   :male (random-elt (list t nil))
   :default-attack (lambda (target user)
                     (declare (ignore target))
                     (format t "~a uses Splash, obviously it had no effect. What did you think was going to happen?" (name-of user)))
   :battle-script-of (lambda (self target)
                       (funcall (coerce (default-attack-of self) 'function) target self))
   :bitcoins-per-level 10))
