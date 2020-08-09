;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass diaper-pirate (potty-enemy) ()
  (:default-initargs
   :name "Diaper Pirate"
   :description "A generic pirate that has forgone toilets and will never try to hold it."
   :species (a:random-elt '("Dolphin" "Orca" "Shark"))
   :male (a:random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :element-types (make-instances yadfa-element-types:water)
   :watersport-limit 300
   :mudsport-limit 400
   :inventory (iter (for i from 0 to (random 20)) (collect (make-instance 'yadfa-items:diaper)))))
(defmethod initialize-instance :after ((c diaper-pirate) &rest args &key &allow-other-keys)
  (destructuring-bind (&key (wear nil wearp) &allow-other-keys)
      args
    (declare (ignore wear))
    (unless wearp
      (setf (wear-of c) nil)
      (push (make-instance 'yadfa-items:diaper) (wear-of c))
      (unless (malep c)
        (push (make-instance 'yadfa-items:bra) (wear-of c)))
      (push (make-instance (if (and (not (malep c)) (= (random 2) 0))
                               'yadfa-items:pirate-dress
                               'yadfa-items:pirate-shirt))
            (wear-of c)))))
(defclass thickly-diaper-pirate (diaper-pirate) ()
  (:default-initargs
   :description "A variant of the Diaper Pirate that wears 3 layers of padding. A stuffer, a normal diaper, and a super thick diaper."
   :inventory (nconc (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:incontinence-pad)))
                     (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:cloth-diaper)))
                     (iter (for i from 0 to (random 20))
                       (collect (make-instance 'yadfa-items:thick-rubber-diaper))))))
(defmethod initialize-instance :after ((c thickly-diaper-pirate) &rest args &key &allow-other-keys)
  (destructuring-bind (&key (wear nil wearp) &allow-other-keys)
      args
    (declare (ignore wear))
    (unless wearp
      (setf (wear-of c) nil)
      (a:appendf (wear-of c)
                 (iter (for i in '(yadfa-items:thick-rubber-diaper yadfa-items:cloth-diaper yadfa-items:incontinence-pad))
                   (collect (make-instance i))))
      (unless (malep c)
        (push (make-instance 'yadfa-items:bra) (wear-of c)))
      (push (make-instance 'yadfa-items:pirate-shirt) (wear-of c)))))
