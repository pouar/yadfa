;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass diapered-kobold (potty-enemy pantsable-character adoptable-enemy) ()
  (:default-initargs
   :name "Diapered Kobold"
   :description "They're apparently from a tribe of kobolds in the area. Their outfits are similar to the ancient Egyptians."
   :species "Kobold"
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:cloth-diaper)))))
(setf (get 'diapered-kobold 'change-class-target) 'yadfa-allies:diapered-kobold)
(defmethod initialize-instance :after
    ((c diapered-kobold) &key (wear nil wearp) &allow-other-keys)
  (declare (ignore wear))
  (unless wearp
    (push (let ((a (make-instance 'yadfa-items:thick-cloth-diaper)))
            (setf (sogginess-of a) (random (sogginess-capacity-of a)))
            (setf (messiness-of a) (random (messiness-capacity-of a)))
            a)
          (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:shendyt 'yadfa-items:kalasiris)) (wear-of c))))
(defclass diapered-skunk (potty-enemy skunk-boop-mixin) ()
  (:default-initargs
   :name "Diapered Skunk"
   :description "They spray their diapers when attacking. Their diapers reek of a smell of urine, feces, and skunk spray."
   :species "Skunk"
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :watersport-chance 3
   :mudsport-chance 3
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:high-capacity-diaper)))
   :element-types '#.(coerce-element-types 'yadfa-element-types:poison)
   :moves (make-instances yadfa-moves:spray yadfa-moves:face-sit)))
(s:defmethods diapered-skunk (character)
  (:method initialize-instance :after
      (character &key (watersport-limit nil watersportp) (mudsport-limit nil mudsportp)
                      (wear nil wearp) &allow-other-keys)
    (declare (ignore watersport-limit mudsport-limit wear))
    (unless wearp
      (push (let ((a (make-instance 'yadfa-items:high-capacity-diaper)))
              (setf (sogginess-of a) (+ (bladder/potty-desperate-limit-of character)
                                        (random (+ (bladder/maximum-limit-of character)
                                                   (- (bladder/maximum-limit-of character) (bladder/potty-desperate-limit-of character))))))
              (setf (messiness-of a) (+ (bowels/potty-desperate-limit-of character)
                                        (random (- (bowels/maximum-limit-of character) (bowels/potty-desperate-limit-of character)))))
              a)
            (wear-of character))
      (push (make-instance (if (malep character) 'yadfa-items:tshirt 'yadfa-items:bikini-top)) (wear-of character))
      (push (make-instance 'yadfa-items:black-leather-jacket) (wear-of character)))
    (unless watersportp
      (setf (watersport-limit-of character) (- (bladder/maximum-limit-of character) (bladder/potty-desperate-limit-of character))))
    (unless mudsportp
      (setf (mudsport-limit-of character) (- (bowels/maximum-limit-of character) (bowels/potty-desperate-limit-of character)))))
  (:method process-battle-accident (character attack (item item) reload (selected-target base-character))
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
               (format t "~a gets a look of relief on ~a face as ~a floods ~a pamps~%"
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
               (format t "~a squats down and with a heavy grunt pushes a huge load into ~a diapers~%"
                       name
                       hisher)
               (mess :messer character)
               (set-status-condition 'yadfa-status-conditions:messing character))
             t)))))
(defclass diapered-skunk* (potty-enemy skunk-boop-mixin) ()
  (:default-initargs
   :name "Diapered Skunk"
   :species "Skunk"
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :watersport-chance 3
   :mudsport-chance 3
   :bitcoins-per-level 100
   :element-types '#.(coerce-element-types 'yadfa-element-types:poison)
   :moves (make-instances yadfa-moves:spray yadfa-moves:face-sit)))
(s:defmethods diapered-skunk* (character)
  (:method initialize-instance :after
      (character &key (watersport-limit nil watersportp) (mudsport-limit nil mudsportp)
                      (wear nil wearp) (description nil descriptionp) &allow-other-keys)
    (declare (ignore watersport-limit mudsport-limit wear description))
    (unless wearp
      (push (let ((a (make-instance 'yadfa-items:high-capacity-diaper)))
              (setf (sogginess-of a) (sogginess-capacity-of a))
              (setf (messiness-of a) (messiness-capacity-of a))
              a)
            (wear-of character)))
    (unless watersportp
      (setf (watersport-limit-of character) (- (bladder/maximum-limit-of character) (bladder/potty-desperate-limit-of character))))
    (unless mudsportp
      (setf (mudsport-limit-of character) (- (bowels/maximum-limit-of character) (bowels/potty-desperate-limit-of character))))
    (unless descriptionp
      (let* ((male (malep character))
             (hisher (if male "his" "her")))
        (setf (description-of character) (format nil "If you thought the other skunk was stinky, that's nothing compared to this one. Apparently this skunk never changes ~a pamps at all and just continues to flood, mess, and spray ~a current one. ~a doesn't wear anything else because it just gets covered in too much of ~a own stinky juices." hisher hisher (if male "He" "She") hisher)))))
  (:method process-battle-accident (character attack (item item) reload (selected-target base-character))
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
             t)))))
(defclass diapered-dragon (potty-enemy) ()
  (:default-initargs
   :name "Diapered Dragon"
   :description "Keeps kobolds as pets. Waits until the last minute because \{,s}he's not some hatchling that has to use the potty all the time\""
   :species "Dragon"
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :bitcoins-per-level 100
   :wear (list (make-instance 'yadfa-items:black-leather-jacket)
               (make-instance 'yadfa-items:high-capacity-diaper))
   :inventory (nconc (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:high-capacity-diaper)))
                     (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:kurikia-thick-diaper))))
   :element-types '#.(coerce-element-types '(yadfa-element-types:dragon yadfa-element-types:fire yadfa-element-types:flying))
   :moves (make-instances yadfa-moves:tickle yadfa-moves:roar yadfa-moves:mush yadfa-moves:fire-breath)))
(defclass diapered-dragon* (diapered-dragon pantsable-character) ()
  (:default-initargs
   :description "Keeps kobolds as pets. Wears pants to hide {his,her} padding. Waits until the last minute because \"{,s}he's not some hatchling that has to use the potty all the time\""
   :wear (make-instances yadfa-items:black-leather-jacket yadfa-items:baggy-jeans yadfa-items:high-capacity-diaper)))

;;; Raptors would most likely not have bladders irl, but I already threw
;;; scientific accuracy out the window when I gave them scales instead of feathers.
(defclass raptor (potty-enemy adoptable-enemy) ()
  (:default-initargs
   :name "Raptor"
   :malep (a:random-elt '(t nil))
   :description "Biologically inaccurate velociraptor. The kind you see in Jurassic Park that looks more like a lizard than a prehistoric bird."
   :moves (make-instances yadfa-moves:roar yadfa-moves:bite)
   :species "Raptor"))
(setf (get 'diapered-kobold 'change-class-target) 'yadfa-allies:raptor)
(defmethod change-class-text ((class raptor))
  (format nil "~a was adopted and diapered" (name-of class)))
(defclass dergy (bladder-enemy) ()
  (:default-initargs
   :name "Dergy"
   :description "An alien dragon like species that liquefies its food, so he lacks bowels as everything goes through its bladder. But since all that mass is forced through its bladder now, it fills up much quicker, so they have to go more often and can't hold it in for as long."
   :species "Dergy"
   :malep (a:random-elt '(t nil))
   :bitcoins-per-level 100
   :bladder/fill-rate (* (/ 14000 24 60) 2)
   :wear (list (make-instance 'yadfa-items:kurikia-thick-rubber-diaper))
   :inventory (iter (for i from 0 to (random 20))
                (collect (make-instance 'yadfa-items:kurikia-thick-rubber-diaper)))
   :element-types '#.(coerce-element-types 'yadfa-element-types:dragon)
   :moves (make-instances yadfa-moves:tickle yadfa-moves:roar yadfa-moves:mush yadfa-moves:fire-breath)))
