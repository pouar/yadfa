(in-package :yadfa/npcs)
(defclass navy-officer (yadfa:potty-npc) ()
    (:default-initargs
        :name "Navy Officer"
        :description "The Navy is mainly made up of aquatic creatures. They're all toilet trained and consider using their pullups too infantile and embarrassing and the ones that use them end up a laughingstock, so they try their best to make it to a toilet in time. Doesn't always happen though."
        :species (random-elt '("Dolphin" "Orca" "Shark"))
        :male (random-elt (list t nil))
        :bladder/contents (random 500)
        :bowels/contents (random 7000)
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :inventory (let ((a ()))
                       (loop for i from 0 to (random 5) do (push (make-instance 'yadfa/items:navy-pullups) a)))
        :bitcoins-per-level 40))
(defmethod initialize-instance :after
    ((c navy-officer) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :wear) (leave t)))
        (push (make-instance 'yadfa/items:navy-pullups) (wear-of c))
        (when (= (random 5) 0)
            (push (make-instance (if (malep c)
                                     'yadfa/items:navy-pants
                                     'yadfa/items:navy-skirt))
                (wear-of c)))
        (push (make-instance 'yadfa/items:navy-shirt) (wear-of c))))
