(in-package :yadfa/npcs)
(defclass navy-officer (potty-npc) ()
    (:default-initargs
        :name "Navy Officer"
        :description "The Navy is mainly made up of aquatic creatures. They're all toilet trained but may use their pullups if they don't want to hold it any longer."
        :species (random-elt '("Dolphin" "Orca" "Shark"))
        :male (random-elt (list t nil))
        :watersport-limit 100
        :mudsport-limit 1000
        :watersport-chance 3
        :mudsport-chance 3
        :bladder/contents (random 500)
        :bowels/contents (random 7000)
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :inventory (iter (for i from 0 to (random 5)) (collect (make-instance 'yadfa/items:navy-pullups)))
        :bitcoins-per-level 60))
(defmethod initialize-instance :after
    ((c navy-officer) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :wear) (leave t)))
        (push (make-instance 'yadfa/items:navy-pullups) (wear-of c))
        (when (and (not (malep c)) (= (random 5) 0))
            (push (make-instance 'yadfa/items:navy-skirt)
                (wear-of c)))
        (unless (malep c)
            (push (make-instance 'yadfa/items:bra) (wear-of c)))
        (push (make-instance 'yadfa/items:navy-shirt) (wear-of c))))
(defclass navy-officer* (navy-officer) ()
    (:default-initargs
        :description "A variant of the Navy Officer. This variant still wears the standard pullups, but supplements them with stuffers to avoid changing the pullups out and is a bit less likely to try and hold it"
        :watersport-limit (random-elt '(300 150))
        :mudsport-limit (random-elt '(4000 2000))
        :inventory (append
                       (iter (for i from 0 to (random 5))
                           (collect (make-instance 'yadfa/items:navy-pullups)))
                       (iter (for i from 0 to (random 15))
                           (collect (make-instance 'yadfa/items:cloth-incontinence-pad))))))
(defmethod initialize-instance :after
    ((c navy-officer*) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :wear) (leave t)))
        (push (make-instance 'yadfa/items:cloth-incontinence-pad) (wear-of c))
        (push (make-instance 'yadfa/items:navy-pullups) (wear-of c))
        (when (and (not (malep c)) (= (random 5) 0))
            (push (make-instance 'yadfa/items:navy-skirt)
                (wear-of c)))
        (unless (malep c)
            (push (make-instance 'yadfa/items:bra) (wear-of c)))
        (push (make-instance 'yadfa/items:navy-shirt) (wear-of c)))
    (unless (iter (for (a b) on args)
                (when (eq a :watersport-chance) (leave t)))
        (setf
            (watersport-chance-of c)
            (if (eql (watersport-limit-of c) 300)
                1
                3)))
    (unless (iter (for (a b) on args)
                (when (eq a :mudsport-chance) (leave t)))
        (setf
            (mudsport-chance-of c)
            (if (eql (mudsport-limit-of c) 4000)
                1
                3))))

