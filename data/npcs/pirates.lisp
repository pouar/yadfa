(in-package :yadfa/npcs)
(defclass diaper-pirate (potty-npc) ()
    (:default-initargs
        :name "Diaper Pirate"
        :description "A generic pirate that has forgoed toilets and will never try to hold it."
        :species (random-elt '("Dolphin" "Orca" "Shark"))
        :male (random-elt (list t nil))
        :bladder/contents (random 500)
        :bowels/contents (random 7000)
        :watersport-limit 300
        :mudsport-limit 4000
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :inventory (iter (for i from 0 to (random 20)) (collect (make-instance 'yadfa/items:diaper)))))
(defmethod initialize-instance :after
    ((c diaper-pirate) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :wear) (leave t)))
        (push (make-instance 'yadfa/items:diaper)
            (wear-of c))
        (unless (malep c)
            (push (make-instance 'yadfa/items:bra) (wear-of c)))
        (push (make-instance
                  (if (and
                          (not (malep c))
                          (= (random 2) 0))
                      'yadfa/items:pirate-dress
                      'yadfa/items:pirate-shirt))
            (wear-of c))))
(defclass thickly-diaper-pirate (diaper-pirate) ()
    (:default-initargs
        :description "A variant of the Diaper Pirate that wears 3 layers of padding. A stuffer, a cloth diaper that they treat as normal underwear, and a super thick latex diaper that they treat as pants."
        :inventory (append
                       (iter (for i from 0 to (random 20))
                           (collect (make-instance 'yadfa/items:incontinence-pad)))
                       (iter (for i from 0 to (random 20))
                           (collect (make-instance 'yadfa/items:cloth-diaper)))
                       (iter (for i from 0 to (random 20))
                           (collect (make-instance 'yadfa/items:thick-latex-diaper))))))
(defmethod initialize-instance :after
    ((c thickly-diaper-pirate) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :wear) (leave t)))
        (appendf (wear-of c)
            (iter (for i in '(yadfa/items:incontinence-pad
                                 yadfa/items:cloth-diaper
                                 yadfa/items:thick-latex-diaper))
                (collect (make-instance i))))
        (unless (malep c)
            (push (make-instance 'yadfa/items:bra)
                (wear-of c)))
        (push (make-instance 'yadfa/items:pirate-shirt) (wear-of c))))