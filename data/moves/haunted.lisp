;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass ghost-tickle (tickle) ()
  (:default-initargs
   :name "Ghost Tickle"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (getf (attributes-of target) :not-ticklish)
              (write-line "It has no effect")
              (progn (out "A bunch of ghost hands come out and start tickling " (name-of target) :%)
                     (set-status-condition 'yadfa-status-conditions:laughing target))))))
(defclass ghost-squish (stat/move) ()
  (:default-initargs
   :name "Ghost Tickle"
   :description "Tickle the enemy"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (filter-items (wear-of user) 'incontinence-product)
              (progn
                (format t "a ghost hand comes out and squishes ~a's diaper!~%" (name-of target))
                (if (<= (getf (calculate-diaper-usage target) :sogginess) 0)
                    (format t "But it had no effect!~%")))
              (out "it has no effect on " (name-of target) :%)))))
(defclass ghost-mush (mush) ()
  (:default-initargs
   :name "Ghost Mush"
   :attack '(lambda (target user self)
             (declare (ignore self))
             (if (filter-items (wear-of user) 'incontinence-product)
              (progn
                (format t "a ghost hand comes out and mushes the back of ~a's diaper!~%" (name-of target))
                (if (<= (getf (calculate-diaper-usage target) :messiness) 0)
                    (format t "But it had no effect!~%")
                    (progn (format t "~a's diaper has been mushed~%" (name-of target))
                           (set-status-condition 'yadfa-status-conditions:mushed target))))
              (out "it has no effect on " (name-of target) :%)))))
