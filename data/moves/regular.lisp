;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass mush (stat/move) ()
  (:default-initargs
   :name "Mush"
   :description "Mush the target's diaper"
   :attack '(lambda (target user self)
             (declare (ignore self))
             (format t "~a mushes the back of ~a's diaper!~%" (name-of user) (name-of target))
             (if (<= (getf (calculate-diaper-usage target) :messiness) 0)
                 (format t "But it had no effect!~%")
                 (progn (format t "~a's diaper has been mushed~%" (name-of target))
                        (set-status-condition 'yadfa-status-conditions:mushed target))))))
(defclass pants (stat/move) ()
  (:default-initargs
   :name "Pants"
   :description "Pants the enemy"
   :attack 'pants%))
(defclass spray (stat/move) ()
  (:default-initargs
   :name "Spray"
   :description "Spray the target with skunk spray. Also fills your pamps with skunk spray while you're at it."
   :energy-cost 5
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (let ((amount 50)
                   (wet (when (random 5))))
               (iter (while (> amount 0))
                 (for i in (reverse (wear-of user)))
                 (when (typep i 'closed-bottoms)
                   (cond ((> amount (- (sogginess-capacity-of i) (sogginess-of i)))
                          (decf amount (- (sogginess-capacity-of i) (sogginess-of i)))
                          (setf (sogginess-of i) (sogginess-capacity-of i)))
                         ((> amount 0)
                          (incf (sogginess-of i) amount)
                          (setf amount 0))))))
             (let ((clothing (filter-items (wear-of user) 'closed-bottoms)))
               (cond
                 ((filter-items clothing 'incontinence-product)
                  (format t "~a tries to spray the enemy, but ends up spraying in ~a pamps instead~%"
                          (name-of user)
                          (if (malep user) "his" "her")))
                 (clothing
                  (format t "~a tries to spray the enemy, but ends up spraying in ~a pants instead~%"
                          (name-of user)
                          (if (malep user) "his" "her")))
                 (t
                  (format t "~a sprays the enemy~%"
                          (name-of user)))))
             (format t "~a is grossed out by the smell~%" (name-of target))
             (set-status-condition 'yadfa-status-conditions:skunked target))))
(defclass fire-breath (stat/move) ()
  (:default-initargs
   :name "Fire Breath"
   :energy-cost 5
   :power 60
   :description "Breathes fire at the enemy"
   :attack '(lambda (target user self)
             (let ((a (calculate-damage target user (power-of self))))
               (format t "~a used ~a~%" (name-of user) (name-of self))
               (decf (health-of target) a)
               a))))
(defclass face-sit (stat/move) ()
  (:default-initargs
   :name "Face Sit"
   :energy-cost 3
   :power 40
   :description "Sits on the enemy's face and messes"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (let* ((m (mess :messer user))
                    (c (calculate-diaper-usage user))
                    (a (calculate-damage target user (power-of self))))
               (if (> (getf m :mess-amount) 0)
                 (format t "~a sits on ~a's face and messes~%" (name-of user) (name-of target))
                 (format t "~a sits on ~a's face~%" (name-of user) (name-of target)))
               (when (>= (getf c :messiness) 2000)
                 (format t "~a is grossed out by the smell~%" (name-of target))
                 (set-status-condition 'yadfa-status-conditions:skunked target))
               (format t "~a is damaged by the impact~%" (name-of target))
               (decf (health-of target) a)
               a))))
