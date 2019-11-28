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
(defclass adoptable-enemy (standard-class)
  ((change-class-target
   :initform nil
   :initarg :change-class-target
   :reader change-class-target)))
(defmethod c2mop:validate-superclass ((class adoptable-enemy) (super standard-class))
  t)
(defmethod c2mop:validate-superclass ((class standard-class) (super adoptable-enemy))
  t)
(defclass adoptable-enemy-mixin (catchable-enemy) ()
  (:metaclass adoptable-enemy))
(defmethod change-class-text ((class adoptable-enemy-mixin))
  (format nil "~a was adopted" (name-of class)))
(defmacro define-adoptable-enemy (class superclasses slot-options &body body)
  (unless (alexandria:assoc-value body :change-class-target)
    (error "change-class-target isn't defined"))
  `(defclass ,class (adoptable-enemy-mixin ,@superclasses)
     ,slot-options
     (:metaclass adoptable-enemy)
     ,@body))
(defmethod make-instance ((class adoptable-enemy) &rest initargs &key &allow-other-keys)
  (if (eq (class-name class) 'adoptable-enemy)
      (error (format nil "~s isn't supposed to be initialized directly" (class-name class)))
      (call-next-method)))
