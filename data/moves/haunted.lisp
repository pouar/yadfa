;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass ghost-tickle (tickle) ()
  (:default-initargs
   :name "Ghost Tickle"))
(defmethod attack ((target base-character) (user base-character) (attack ghost-tickle))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (if (getf (attributes-of target) :not-ticklish)
      (write-line "It has no effect")
      (progn (f:fmt t "A bunch of ghost hands come out and start tickling " (name-of target) #\Newline)
             (set-status-condition 'yadfa-status-conditions:laughing target))))
(defclass ghost-squish (move) ()
  (:default-initargs
   :name "Ghost Tickle"
   :description "Tickle the enemy"))
(defmethod attack ((target base-character) (user base-character) (attack ghost-squish))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (if (filter-items (wear-of user) 'incontinence-product)
      (progn
        (format t "a ghost hand comes out and squishes ~a's diaper!~%" (name-of target))
        (if (<= (getf (calculate-diaper-usage target) :sogginess) 0)
            (format t "But it had no effect!~%")))
      (f:fmt t "it has no effect on " (name-of target) #\Newline)))
(defclass ghost-mush (mush) ()
  (:default-initargs
   :name "Ghost Mush"))
(defmethod attack ((target base-character) (user base-character) (attack ghost-mush))
  (declare (ignore attack))
  (if (filter-items (wear-of user) 'incontinence-product)
      (progn
        (format t "a ghost hand comes out and mushes the back of ~a's diaper!~%" (name-of target))
        (if (<= (getf (calculate-diaper-usage target) :messiness) 0)
            (format t "But it had no effect!~%")
            (progn (format t "~a's diaper has been mushed~%" (name-of target))
                   (set-status-condition 'yadfa-status-conditions:mushed target))))
      (f:fmt t "it has no effect on " (name-of target) #\Newline)))
