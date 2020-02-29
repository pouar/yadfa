;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass diapered-kobold (potty-enemy pantsable-character adoptable-enemy) ()
  (:default-initargs
   :name "Diapered Kobold"
   :description "They're apparently from a tribe of kobolds in the area. Their outfits are similar to the ancient Egyptians."
   :species "Kobold"
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:cloth-diaper)))))
(setf (get 'diapered-kobold 'change-class-target) 'yadfa-allies:diapered-kobold)
(defmethod initialize-instance :after
    ((c diapered-kobold) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (let ((a (make-instance 'yadfa-items:thick-cloth-diaper)))
            (setf (sogginess-of a) (random (sogginess-capacity-of a)))
            (setf (messiness-of a) (random (messiness-capacity-of a)))
            a)
          (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:shendyt 'yadfa-items:kalasiris)) (wear-of c))))
(defclass diapered-skunk (potty-enemy) ()
  (:default-initargs
   :name "Diapered Skunk"
   :description "They spray their diapers when attacking. Their diapers reek of a smell of urine, feces, and skunk spray."
   :species "Skunk"
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :watersport-chance 3
   :mudsport-chance 3
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:high-capacity-diaper)))
   :moves (list (make-instance 'yadfa-moves:spray) (make-instance 'yadfa-moves:face-sit))))
(defmethod initialize-instance :after
    ((c diapered-skunk) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (let ((a (make-instance 'yadfa-items:high-capacity-diaper)))
            (setf (sogginess-of a) (+ (bladder/potty-desperate-limit-of c) (random (+ (bladder/maximum-limit-of c) (- (bladder/maximum-limit-of c) (bladder/potty-desperate-limit-of c))))))
            (setf (messiness-of a) (+ (bowels/potty-desperate-limit-of c) (random (- (bowels/maximum-limit-of c) (bowels/potty-desperate-limit-of c)))))
            a)
          (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:tshirt 'yadfa-items:bikini-top)) (wear-of c))
    (push (make-instance 'yadfa-items:black-leather-jacket) (wear-of c))
    (unless (iter (for (a b) on args)
              (when (eq a :watersport-limit)
                (leave t)))
      (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c) (bladder/potty-desperate-limit-of c))))
    (unless (iter (for (a b) on args)
              (when (eq a :mudsport-limit)
                (leave t)))
      (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c) (bowels/potty-desperate-limit-of c))))))
(defmethod process-battle-accident-method ((character diapered-skunk) attack item reload selected-target)
  (declare (ignore attack item reload selected-target))
  (let ((watersport-chance (random (watersport-chance-of character)))
        (mudsport-chance (random (mudsport-chance-of character))))
    (cond ((or (>= (bladder/contents-of character)
                   (bladder/maximum-limit-of character))
               (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
               (and (watersport-limit-of character)
                    (<= (- (bladder/maximum-limit-of character) (bladder/contents-of character)) (watersport-limit-of character))
                    (< watersport-chance 1))
               (and (mudsport-limit-of character)
                    (<= (- (bowels/maximum-limit-of character) (bowels/contents-of character)) (mudsport-limit-of character))
                    (< mudsport-chance 1)))
           (when (or (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
                     (and (watersport-limit-of character)
                          (<= (- (bladder/maximum-limit-of character) (bladder/contents-of character)) (watersport-limit-of character))
                          (< watersport-chance 1)))
             (format t "~a gets a look of relief on ~a face as ~a floods ~a pamps~%"
                     (name-of character)
                     (if (malep character) "his" "her")
                     (if (malep character) "he" "she")
                     (if (malep character) "his" "her"))
             (wet :wetter character)
             (set-status-condition 'yadfa-status-conditions:wetting character))
           (when (or (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
                     (and (mudsport-limit-of character)
                          (<= (- (bowels/maximum-limit-of character) (bowels/contents-of character)) (mudsport-limit-of character))
                          (< mudsport-chance 1)))
             (format t "~a squats down and with a heavy grunt pushes a huge load into ~a diapers~%"
                     (name-of character)
                     (if (malep character) "his" "her"))
             (mess :messer character)
             (set-status-condition 'yadfa-status-conditions:messing character))
           t))))
(defclass diapered-skunk* (potty-enemy) ()
  (:default-initargs
   :name "Diapered Skunk"
   :species "Skunk"
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :watersport-chance 3
   :mudsport-chance 3
   :bitcoins-per-level 100
   :moves (list (make-instance 'yadfa-moves:spray) (make-instance 'yadfa-moves:face-sit))))
(defmethod initialize-instance :after
    ((c diapered-skunk*) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (let ((a (make-instance 'yadfa-items:high-capacity-diaper)))
            (setf (sogginess-of a) (sogginess-capacity-of a))
            (setf (messiness-of a) (messiness-capacity-of a))
            a)
          (wear-of c))
    (unless (iter (for (a b) on args)
              (when (eq a :watersport-limit)
                (leave t)))
      (setf (watersport-limit-of c) (- (bladder/maximum-limit-of c) (bladder/potty-desperate-limit-of c))))
    (unless (iter (for (a b) on args)
              (when (eq a :mudsport-limit)
                (leave t)))
      (setf (mudsport-limit-of c) (- (bowels/maximum-limit-of c) (bowels/potty-desperate-limit-of c))))
    (unless (iter (for (a b) on args)
              (when (eq a :description)
                (leave t)))
      (let* ((male (malep c))
             (hisher (if male "his" "her")))
        (setf (description-of c) (format nil "If you thought the other skunk was stinky, that's nothing compared to this one. Apparently this skunk never changes ~a pamps at all and just continues to flood, mess, and spray ~a current one. ~a doesn't wear anything else because it just gets covered in too much of ~a own stinky juices." hisher hisher (if male "He" "She") hisher))))))
(defmethod process-battle-accident-method ((character diapered-skunk*) attack item reload selected-target)
  (declare (ignore attack item reload selected-target))
  (let* ((watersport-chance (random (watersport-chance-of character)))
         (mudsport-chance (random (mudsport-chance-of character)))
         (male (malep character))
         (hisher (if male "his" "her"))
         (name (name-of character))
         (bladder/maximum-limit (bladder/maximum-limit-of character))
         (bowels/maximum-limit (bowels/maximum-limit-of character))
         (mudsport-limit (mudsport-limit-of character))
         (watersport-limit (watersport-limit-of character)))
    (cond ((or (>= (bladder/contents-of character)
                   (bladder/maximum-limit-of character))
               (>= (bowels/contents-of character) bowels/maximum-limit)
               (and watersport-limit
                    (<= (- bladder/maximum-limit (bladder/contents-of character)) watersport-limit)
                    (< watersport-chance 1))
               (and mudsport-limit
                    (<= (- bowels/maximum-limit (bowels/contents-of character)) mudsport-limit)
                    (< mudsport-chance 1)))
           (when (or (>= (bladder/contents-of character) bladder/maximum-limit)
                     (and watersport-limit
                          (<= (- bladder/maximum-limit (bladder/contents-of character)) watersport-limit)
                          (< watersport-chance 1)))
             (format t "~a gets a look of relief on ~a face as ~a floods ~a already leaky and waterlogged pamps~%"
                     name
                     hisher
                     (if male "he" "she")
                     hisher)
             (wet :wetter character)
             (set-status-condition 'yadfa-status-conditions:wetting character))
           (when (or (>= (bowels/contents-of character) bowels/maximum-limit)
                     (and mudsport-limit
                          (<= (- bowels/maximum-limit (bowels/contents-of character)) mudsport-limit)
                          (< mudsport-chance 1)))
             (format t "~a squats down and with a heavy grunt pushes a huge load into ~a already overly full diapers~%"
                     name
                     hisher)
             (mess :messer character)
             (set-status-condition 'yadfa-status-conditions:messing character))
           t))))
(defclass diapered-dragon (potty-enemy) ()
  (:default-initargs
   :name "Diapered Dragon"
   :description "Keeps kobolds as pets. Waits until the last minute because \"he's not some hatchling that has to use the potty all the time\""
   :species "Dragon"
   :male t
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :bitcoins-per-level 100
   :wear (list (make-instance 'yadfa-items:black-leather-jacket)
               (make-instance 'yadfa-items:high-capacity-diaper))
   :inventory (nconc (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:high-capacity-diaper)))
                     (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:kurikia-thick-diaper))))
   :moves (list (make-instance 'yadfa-moves:tickle)
                (make-instance 'yadfa-moves:roar)
                (make-instance 'yadfa-moves:mush)
                (make-instance 'yadfa-moves:fire-breath))))
(defclass diapered-dragon* (diapered-dragon pantsable-character) ()
  (:default-initargs
   :description "Keeps kobolds as pets. Wears pants to hide his padding. Waits until the last minute because \"he's not some hatchling that has to use the potty all the time\""
   :wear (list (make-instance 'yadfa-items:black-leather-jacket)
               (make-instance 'yadfa-items:baggy-jeans)
               (make-instance 'yadfa-items:high-capacity-diaper))))

;;; Raptors would most likely not have bladders irl, but I already threw
;;; scientific accuracy out the window when I gave them scales instead of feathers.
(defclass raptor (potty-enemy adoptable-enemy) ()
    (:default-initargs
     :name "Raptor"
     :malep (random-elt '(t nil))
     :description "Biologically inaccurate velociraptor. The kind you see in Jurassic Park that looks more like a lizard than a prehistoric bird."
     :moves (list (make-instance 'yadfa-moves:roar)
                  (make-instance 'yadfa-moves:bite))
     :species "Raptor"))
(setf (get 'diapered-kobold 'change-class-target) 'yadfa-allies:raptor)
(defmethod change-class-text ((class raptor))
  (format nil "~a was adopted and diapered" (name-of class)))
