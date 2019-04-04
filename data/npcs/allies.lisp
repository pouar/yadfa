(in-package :yadfa-allies)
(defclass slynk (ally) ()
    (:default-initargs
        :name "Slynk"
        :male t
        :potty-training :last-minute
        :wear (let ((diaper
                        (progn (ensure-finalized (find-class 'yadfa-items:bandit-adjustable-diaper))
                            (compute-default-initargs (find-class 'yadfa-items:bandit-adjustable-diaper)))))
                  (list
                      (make-instance 'yadfa-items:bandit-uniform-tunic)
                      (make-instance 'yadfa-items:thick-rubber-diaper)
                      (make-instance 'yadfa-items:bandit-adjustable-diaper
                          :sogginess (second (assoc :sogginess-capacity diaper))
                          :messiness (second (assoc :messiness-capacity diaper)))))
        :species "Raccoon"
        :description "Used to be one of the Diapered Raccoon Bandits. Was kicked out after he was forced to give the location of Pirate's Cove to the Navy. He was humiliated constantly by the Diapered Pirates until you rescued him. Is too embarrassed to admit when he as to go unless he's desperate"
        :level 5))
(defmethod initialize-instance :after
    ((c slynk) &rest args &key &allow-other-keys)
    (unless (iter (for (a b) on args)
                (when (eq a :bladder/contents) (leave t)))
        (setf (bladder/contents-of c)
            (random (float (+ (bladder/potty-desperate-limit-of c)
                               (/ (- (bladder/potty-desperate-limit-of c)
                                      (bladder/potty-dance-limit-of c))))))))
    (unless (iter (for (a b) on args)
                (when (eq a :bowels/contents) (leave t)))
        (setf (bowels/contents-of c)
            (random (float (+ (bowels/potty-desperate-limit-of c)
                               (/ (- (bowels/potty-desperate-limit-of c)
                                      (bowels/potty-dance-limit-of c)))))))))
(defclass chris (ally) ()
    (:default-initargs
        :name "Chris"
        :male t
        :species "Fox"
        :potty-training :rebel
        :description "An orange fox. has gotten accustomed to being treated like a pet and will typically wear nothing but a collar, refuses to be housebroken like a good fox so he must be diapered at all times."
        :wear (list
                  (make-instance 'yadfa-items:gold-collar)
                  (make-instance 'yadfa-items:bandit-diaper))))
(defclass kristy (ally) ()
    (:default-initargs
        :name "Kristy"
        :male nil
        :potty-training :none
        :species "Fox"
        :description "A beautiful orange vixen who has a personality that is more like a child than an adult. Loves wearing thick diapers, can't stand pants. Has gone without diapers for so long that she has become dependent on them."
        :wear (list
                  (make-instance 'yadfa-items:toddler-dress)
                  (make-instance 'yadfa-items:bandit-female-diaper))))
(defclass furry (ally) ()
    (:default-initargs
        :name "Furry"
        :male t
        :potty-training :silent
        :species "Fox"
        :description "A fox that likes to wear a fursuit. Doesn't talk much. The team got him as a pet, and as a plushie."
        :wear (list
                  (make-instance 'yadfa-items:watertight-fursuit)
                  (make-instance 'yadfa-items:kurikia-thick-cloth-diaper))))
