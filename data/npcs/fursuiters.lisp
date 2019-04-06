(in-package :yadfa-npcs)
(defclass padded-fursuiter-servant (potty-npc) ()
    (:default-initargs
        :name "Padded Fursuiter Servant"
        :description "These are basically generic \"servants\" that you can also use as a plushie. Since they're not allowed to take bathroom breaks, they're thickly padded and have special fursuits that keep all the fluids and smells in. Some are too embarrassed to use their diapers for their intended purposes and try so hard to hold it in only to uncontrollably flood and mess themselves. Other's have given up and just use their diapers whenever they have to go."
        :male (random-elt (list t nil))
        :species "Fox"
        :bladder/contents (random 500)
        :bowels/contents (random 7000)
        :wear (list
                  (make-instance 'yadfa-items:watertight-fursuit)
                  (make-instance 'yadfa-items:kurikia-thick-diaper))))
(defmethod initialize-instance :after
    ((c padded-fursuiter-servant) &rest args &key &allow-other-keys)
    (let ((potty-keys
              (iter (for (a b) on args)
                  (when
                      (member a '(:watersport-limit :mudsport-limit))
                      (collect a)))))
        (cond
            ((member '(:watersport-limit :mudsport-limit) potty-keys
                 :test (lambda (o ei)
                           (member ei o)))
                (let ((limits (random-elt (list
                                              (cons (bladder/need-to-potty-limit-of c) (bowels/need-to-potty-limit-of c))
                                              '(nil)))))
                    (setf (watersport-limit-of c) (car limits)
                        (mudsport-limit-of c) (cdr limits))))
            ((member :watersport-limit potty-keys)
                (setf (mudsport-limit-of c) (random-elt (list (bowels/need-to-potty-limit-of c) nil))))
            ((member :mudsport-limit potty-keys)
                (setf (watersport-limit-of c) (random-elt (list (bladder/need-to-potty-limit-of c) nil)))))))
