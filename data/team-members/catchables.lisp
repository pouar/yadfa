;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-allies"; coding: utf-8-unix; -*-
(in-package :yadfa-allies)
(defclass raptor (adopted-enemy ally-feral) ()
  (:default-initargs
   :name "Raptor"
   :malep (a:random-elt '(t nil))
   :description "Biologically inaccurate velociraptor. The kind you see in Jurassic Park that looks more like a lizard than a prehistoric bird."
   :moves (list (make-instance 'yadfa-moves:roar)
                (make-instance 'yadfa-moves:bite)
                (make-instance 'yadfa-moves:watersport)
                (make-instance 'yadfa-moves:mudsport))
   :wear (list (make-instance 'yadfa-items:diaper))))
(defclass diapered-kobold (adopted-enemy ally-silent-potty-training pantsable-character) ()
  (:default-initargs
   :name "Diapered Kobold"
   :description "They're apparently from a tribe of kobolds in the area. Their outfits are similar to the ancient Egyptians."
   :species "Kobold"
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:cloth-diaper)))
   :moves (list (make-instance 'yadfa-moves:watersport)
                (make-instance 'yadfa-moves:mudsport))))
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
(defclass diapered-raccoon-bandit (adopted-enemy ally-last-minute-potty-training pantsable-character) ()
  (:default-initargs
   :name "Diapered Raccoon Bandit"
   :malep t
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Apparently wearing (and using) diapers is extremely embarrassing for them, so they wear tunics to hide them."
   :moves (list (make-instance 'yadfa-moves:mush)
                (make-instance 'yadfa-moves:pants)
                (make-instance 'yadfa-moves:tickle)
                (make-instance 'yadfa-moves:watersport)
                (make-instance 'yadfa-moves:mudsport))
   :wear (list (make-instance 'yadfa-items:bandit-uniform-tunic)
               (make-instance 'yadfa-items:bandit-adjustable-diaper))))
(defclass found-raccoon-bandit (diapered-raccoon-bandit) ()
  (:default-initargs
   :name "Diapered Raccoon Bandit"
   :description "You found this Raccoon at that haunted house. Be sure to return him to the Raccoon Bandits when you're done having fun with him."
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (list (make-instance 'yadfa-items:bandit-diaper))))
