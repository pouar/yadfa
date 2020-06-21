;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-status-conditions"; coding: utf-8-unix; -*-
(in-package :yadfa-status-conditions)
(defclass poisoned (status-condition)
  ()
  (:default-initargs
   :name "Poisoned"
   :description "User is currently poisoned"
   :duration t))
(defmethod battle-script ((user base-character) (condition poisoned))
  (if (= 0 (random 5))
      (progn (format t "~a is hurt by the poison~%" (name-of user))
             (decf (health-of user) (/ (calculate-stat user :health))))
      (a:deletef (getf (status-conditions-of *battle*) user) condition)))
