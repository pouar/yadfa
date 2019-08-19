(in-package :yadfa-enemies)
(defclass diapered-kobold (potty-enemy pantsable-character) ()
  (:default-initargs
   :name "Diapered Kobold"
   :description "They're apparently from a tribe of kobolds in the area."
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
    (push (make-instance 'yadfa-items:cloth-diaper) (wear-of c))
    (push (make-instance (if (malep c) 'yadfa-items:shendyt 'yadfa-items:kalasiris)) (wear-of c))))
