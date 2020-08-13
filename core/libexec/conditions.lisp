;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(define-condition invalid-user-input (simple-condition) ()
  (:documentation "Condition that signals when the player enters an invalid value. Their fault."))
(define-condition onesie-too-thick (simple-condition)
  ((clothes :type list :initarg :clothes)
   (user :type base-character :initarg :user))
  (:documentation "Condition signaled by @code{TOGGLE-ONESIE} when the onesie @code{(CAR CLOTHES)} can't fit over the user @var{USER}'s @code{(CDR CLOTHES)}")
  (:report (lambda (c s)
             (format s "~s doesn't fit over pamps" (car (clothes-of c))))))
(define-condition onesie-locked (simple-condition)
  ((clothes :type list :initarg :clothes :accessor clothes-of)
   (user :type base-character :initarg :user :accessor user-of))
  (:documentation "Condition signaled by @code{TOGGLE-ONESIE} when @code{(CAR CLOTHES)} is locked")
  (:report (lambda (c s)
             (format s "~s is locked" (car (clothes-of c))))))
(define-condition unusable-item ()
  ((item :initarg :item
         :initform nil
         :reader unusable-item-item)))
(define-condition item-action-missing (unusable-item)
  ((action :initarg :action
           :initform nil
           :reader unusable-item-action))
  (:report (lambda (condition stream)
             (format stream "Action ~s for ~s doesn't exist" (unusable-item-action condition) (unusable-item-item condition)))))
(define-condition item-use-script-missing (unusable-item)
  ()
  (:report (lambda (condition stream)
             (format stream "~s has no ~s method defined" (unusable-item-item condition) 'use-script))))
