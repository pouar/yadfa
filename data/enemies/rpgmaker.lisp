(in-package :yadfa-enemies)
(defclass diapered-kobold (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Diapered Kobold"
   :description "They're apparently from a tribe of kobolds in the area. Their outfits are similar to the ancient Egyptians."
   :species "Kobold"
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 7000)
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2)
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:cloth-diaper)))))
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
(defclass diapered-skunk (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Diapered Skunk"
   :description "Careful, these guys will try to spray you. Their diapers reek of a smell of urine, feces, and skunk spray."
   :species "Skunk"
   :male (random-elt '(t nil))
   :bladder/contents (random 500)
   :bowels/contents (random 7000)
   :bladder/fill-rate (* (/ 2000 24 60) 2)
   :bowels/fill-rate (* (/ 12000 24 60) 2)
   :bitcoins-per-level 100
   :inventory (iter (for i from 0 to (random 10))
                (collect (make-instance 'yadfa-items:high-capacity-diaper)))))
(defmethod initialize-instance :after
    ((c diapered-kobold) &rest args &key &allow-other-keys)
  (unless (iter (for (a b) on args)
            (when (eq a :wear)
              (leave t)))
    (push (let ((a (make-instance 'yadfa-items:high-capacity-diaper)))
            (setf (sogginess-of a) (random 1000))
            (setf (messiness-of a) (random 8000))
            a)
          (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:baggy-jeans 'yadfa-items:denim-skirt)) (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:tshirt 'yadfa-items:bikini-top)) (wear-of c))))
