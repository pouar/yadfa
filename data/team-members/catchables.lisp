;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-allies"; coding: utf-8-unix; -*-
(in-package :yadfa-allies)
(defclass raptor (adopted-enemy ally-feral) ()
  (:default-initargs
   :name "Raptor"
   :malep (random-elt '(t nil))
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
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 7000)
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
