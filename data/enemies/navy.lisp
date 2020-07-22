;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass navy-officer (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Navy Officer"
   :description "The Navy is mainly made up of aquatic creatures. They're all toilet trained but may use their pullups if they don't want to hold it any longer."
   :species (a:random-elt '("Dolphin" "Orca" "Shark"))
   :male (a:random-elt '(t nil))
   :watersport-chance 3
   :mudsport-chance 3
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :element-types (list (make-instance 'yadfa-element-types:water))
   :inventory (iter (for i from 0 to (random 5)) (collect (make-instance 'yadfa-items:navy-pullups)))
   :bitcoins-per-level 60))
(defmethod process-battle-accident ((character navy-officer) attack (item item) reload (selected-target base-character))
  (declare (ignore attack item reload selected-target))
  (let* ((male (malep character))
         (pamps (iter (for i in (wear-of character))
                  (let ((i (typecase i
                             (diaper 'diaper)
                             (pullup 'pullup)
                             (closed-bottoms 'closed-bottoms))))
                    (when i
                      (leave i)))))
         (pampspronoun (if male
                           (if pamps
                               "his "
                               "him")
                           (if pamps
                               "her "
                               "her")))
         (pampsname (case pamps
                      (diaper "diapers")
                      (pullup "pullups")
                      (closed-bottoms "pants")
                      (t "self"))))
    (cond ((or (>= (bladder/contents-of character)
                   (bladder/maximum-limit-of character))
               (>= (bowels/contents-of character) (bowels/maximum-limit-of character)))
           (let ((heshe (if male "he" "she"))
                 (himher (if male "him" "her")))
             (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
               (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                       (name-of character)
                       heshe
                       himher)
               (wet :wetter character)
               (set-status-condition 'yadfa-status-conditions:wetting character))
             (when (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
               (format t "~a involuntarily squats down as ~a accidentally messes ~aself in battle~%"
                       (name-of character)
                       heshe
                       himher)
               (mess :messer character)
               (set-status-condition 'yadfa-status-conditions:messing character))
             t))
          ((and (watersport-limit-of character)
                (<= (- (bladder/maximum-limit-of character) (bladder/contents-of character)) (watersport-limit-of character))
                (< (random (watersport-chance-of character)) 1))
           (format t "~a slightly blushes and lets go from the front of ~a~a and spreads ~a legs apart and floods them~%"
                   (name-of character)
                   pampspronoun
                   pampsname
                   (if male
                       "his"
                       "her"))
           (wet :wetter character))
          ((and (mudsport-limit-of character)
                (<= (- (bowels/maximum-limit-of character) (bowels/contents-of character)) (mudsport-limit-of character))
                (< (random (mudsport-chance-of character)) 1))
           (format t "~a slightly blushes and squats down and messes ~a~a~%"
                   (name-of character)
                   pampspronoun
                   pampsname)
           (mess :messer character)))))
(defmethod initialize-instance :after
    ((c navy-officer) &rest args &key &allow-other-keys)
  (destructuring-bind (&key (watersport-limit nil watersportp) (mudsport-limit nil mudsportp) (wear nil wearp) &allow-other-keys)
      args
    (declare (ignore watersport-limit mudsport-limit wear))
    (unless wearp
      (push (make-instance 'yadfa-items:navy-pullups) (wear-of c))
      (when (and (not (malep c)) (= (random 5) 0))
        (push (make-instance 'yadfa-items:navy-skirt) (wear-of c)))
      (unless (malep c)
        (push (make-instance 'yadfa-items:bra) (wear-of c)))
      (push (make-instance 'yadfa-items:navy-shirt) (wear-of c)))
    (unless watersportp
      (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c) (bladder/potty-desperate-limit-of c))))
    (unless mudsportp
      (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c) (bowels/potty-desperate-limit-of c))))))
(defclass navy-officer* (navy-officer) ()
  (:default-initargs
   :description "A variant of the Navy Officer. This variant still wears the standard pullups, but supplements them with stuffers to avoid changing the pullups out and is a bit less likely to try and hold it"
   :watersport-chance (random-from-range 1 3)
   :mudsport-chance (random-from-range 1 3)
   :inventory (nconc (iter (for i from 0 to (random 5))
                       (collect (make-instance 'yadfa-items:navy-pullups)))
                     (iter (for i from 0 to (random 15))
                       (collect (make-instance 'yadfa-items:cloth-incontinence-pad))))))
(defmethod initialize-instance :after
    ((c navy-officer*) &rest args &key &allow-other-keys)
  (destructuring-bind (&key (watersport-limit nil watersportp) (mudsport-limit nil mudsportp) (wear nil wearp) &allow-other-keys)
      args
      (declare (ignore watersport-limit mudsport-limit wear))
    (unless wearp
      (push (make-instance 'yadfa-items:cloth-incontinence-pad) (wear-of c))
      (push (make-instance 'yadfa-items:navy-pullups) (wear-of c))
      (when (and (not (malep c)) (= (random 5) 0))
        (push (make-instance 'yadfa-items:navy-skirt) (wear-of c)))
      (unless (malep c)
        (push (make-instance 'yadfa-items:bra) (wear-of c)))
      (push (make-instance 'yadfa-items:navy-shirt) (wear-of c)))
    (unless watersportp
      (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c)
                                       (a:random-elt (list (bladder/potty-dance-limit-of c) (bladder/need-to-potty-limit-of c))))))
    (unless mudsportp
      (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c)
                                     (a:random-elt (list (bowels/potty-dance-limit-of c) (bowels/need-to-potty-limit-of c))))))))
