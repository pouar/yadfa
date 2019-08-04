;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass navy-officer (potty-enemy) ()
  (:default-initargs
   :name "Navy Officer"
   :description "The Navy is mainly made up of aquatic creatures. They're all toilet trained but may use their pullups if they don't want to hold it any longer."
   :species (random-elt '("Dolphin" "Orca" "Shark"))
   :male (random-elt (list t nil))
   :watersport-chance 3
   :mudsport-chance 3
   :bladder/contents (random 500)
   :bowels/contents (random 7000)
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2)
   :inventory (iter (for i from 0 to (random 5)) (collect (make-instance 'yadfa-items:navy-pullups)))
   :bitcoins-per-level 60))
(defmethod initialize-instance :after
    ((c navy-officer) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (make-instance 'yadfa-items:navy-pullups) (wear-of c))
    (when (and (not (malep c)) (= (random 5) 0))
      (push (make-instance 'yadfa-items:navy-skirt) (wear-of c)))
    (unless (malep c)
      (push (make-instance 'yadfa-items:bra) (wear-of c)))
    (push (make-instance 'yadfa-items:navy-shirt) (wear-of c)))
  (unless (iter (for (a b) on args)
            (when (eq a :watersport-limit)
              (leave t)))
    (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c) (bladder/potty-desperate-limit-of c))))
  (unless (iter (for (a b) on args)
            (when (eq a :mudsport-limit) (leave t)))
    (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c) (bowels/potty-desperate-limit-of c)))))
(defclass navy-officer* (navy-officer) ()
  (:default-initargs
   :description "A variant of the Navy Officer. This variant still wears the standard pullups, but supplements them with stuffers to avoid changing the pullups out and is a bit less likely to try and hold it"
   :watersport-chance (random-from-range 1 3)
   :mudsport-chance (random-from-range 1 3)
   :inventory (append (iter (for i from 0 to (random 5))
                        (collect (make-instance 'yadfa-items:navy-pullups)))
                      (iter (for i from 0 to (random 15))
                        (collect (make-instance 'yadfa-items:cloth-incontinence-pad))))))
(defmethod initialize-instance :after
    ((c navy-officer*) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (make-instance 'yadfa-items:cloth-incontinence-pad) (wear-of c))
    (push (make-instance 'yadfa-items:navy-pullups) (wear-of c))
    (when (and (not (malep c)) (= (random 5) 0))
      (push (make-instance 'yadfa-items:navy-skirt) (wear-of c)))
    (unless (malep c)
      (push (make-instance 'yadfa-items:bra) (wear-of c)))
    (push (make-instance 'yadfa-items:navy-shirt) (wear-of c)))
  (unless (iter (for (a b) on args)
            (when (eq a :watersport-limit)
              (leave t)))
    (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c) (random-elt (list (bladder/potty-dance-limit-of c) (bladder/need-to-potty-limit-of c))))))
  (unless (iter (for (a b) on args)
            (when (eq a :mudsport-limit)
              (leave t)))
    (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c) (random-elt (list (bowels/potty-dance-limit-of c) (bowels/need-to-potty-limit-of c)))))))
