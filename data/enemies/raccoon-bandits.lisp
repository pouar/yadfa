(in-package :yadfa/enemies)
(defclass diapered-raccoon-bandit (potty-enemy) ()
    (:default-initargs
        :name "Diapered Raccoon Bandit"
        :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Apparently wearing (and using) diapers is extremly embarrassing for them, so they wear tunics to hide them."
        :species "Raccoon"
        :male t
        :bladder/contents (strong-random 500)
        :bowels/contents (strong-random 7000)
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :wear (list
                  (make-instance 'yadfa/items:bandit-uniform-tunic)
                  (make-instance 'yadfa/items:bandit-adjustable-diaper))
        :inventory (let ((a ()))
                       (loop for i from 0 to (strong-random 5) do (push (make-instance 'yadfa/items:bandit-diaper) a))
                       (loop for i from 0 to (strong-random 5) do (push (make-instance 'yadfa/items:bandit-adjustable-diaper) a))
                       (loop for i from 0 to (strong-random 5) do (push (make-instance 'yadfa/items:bandit-female-diaper) a)))
        :bitcoins-per-level 40
        :battle-script '(lambda (self target)
                            (let ((moves-with-health
                                      (iter
                                          (for i in (append
                                                        (list
                                                            (if (wield-of self)
                                                                (default-move-of (wield-of self))
                                                                (make-instance 'yadfa/moves:default)))
                                                        (moves-of self)))
                                          (when (and
                                                    (>= (energy-of self) (energy-cost-of i))
                                                    (position :ai-health-inc (ai-flags-of i)))
                                              (collect i))))
                                     (moves-can-use
                                         (iter
                                             (for i in (append
                                                           (list
                                                               (if (wield-of self)
                                                                   (default-move-of (wield-of self))
                                                                   (make-instance 'yadfa/moves:default)))
                                                           (moves-of self)))
                                             (when (>= (energy-of self) (energy-cost-of i))
                                                 (collect i))))
                                     (move-to-use nil))
                                (cond
                                    ((and (<= (health-of self) (/ (calculate-stat self :health) 4))
                                         moves-with-health)
                                        (setf move-to-use
                                            (nth (strong-random (list-length moves-with-health)) moves-with-health))
                                        (funcall
                                            (coerce
                                                (attack-of move-to-use)
                                                'function)
                                            target
                                            self
                                            move-to-use))
                                    (t
                                        (setf move-to-use
                                            (let ((j (strong-random 5)))
                                                (cond
                                                    ((and
                                                         (>=
                                                             (bladder/contents-of target)
                                                             (bladder/potty-dance-limit-of target))
                                                         (< j 1))
                                                        (format t "~a gets a grin on ~a face~%"
                                                            (name-of self)
                                                            (if (malep self) "his" "her"))
                                                        (make-instance 'yadfa/moves:tickle))
                                                    ((and
                                                         (> (getf (calculate-diaper-usage target) :messiness) 0)
                                                         (< j 1))
                                                        (format t "~a gets a grin on ~a face~%"
                                                            (name-of self)
                                                            (if (malep self) "his" "her"))
                                                        (make-instance 'yadfa/moves:mush))
                                                    (t (nth
                                                           (strong-random (list-length moves-can-use))
                                                           moves-can-use)))))
                                        (funcall
                                            (coerce
                                                (attack-of move-to-use)
                                                'function)
                                            target
                                            self
                                            move-to-use)))))))
(defclass rookie-diapered-raccoon-bandit (potty-enemy) ()
    (:default-initargs
        :name "Rookie Diapered Raccoon Bandit"
        :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Despite how embarrasing diapers are for them, the use of toilets and pants in the gang are a privilege and not a right. The ones without these privilages have `babysitters' to keep track of them, as they're not allowed to change themselves. Despite this, they try their best to not wet and/or mess their diapers in a desperate attempt to make their situation less embarrassing."
        :species "Raccoon"
        :male t
        :bladder/contents (strong-random 500)
        :bowels/contents (strong-random 7000)
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :wear (list
                  (make-instance 'yadfa/items:bandit-uniform-shirt)
                  (make-instance 'yadfa/items:bandit-diaper
                      :sogginess (let ((a (strong-random 3)))
                                     (cond
                                         ((= a 0) 0)
                                         ((= a 1) (random-from-range 10 50))
                                         ((= a 2) (random-from-range 300 1000))))
                      :messiness (let ((a (strong-random 2)))
                                     (cond
                                         ((= a 0) 0)
                                         ((= a 1) 8000)))))
        :bitcoins-per-level 20))
(defclass female-diapered-raccoon-bandit (potty-enemy) ()
    (:default-initargs
        :name "Female Diapered Raccoon Bandit"
        :description "The Diapered Raccoon Bandits are a local AB/DL gang here in this world. Apparently gender equality is non-existent in this gang, so the females have the same potty and pants privileges as the rookies, meaning none at all."
        :species "Raccoon"
        :male nil
        :bladder/contents (strong-random 500)
        :bowels/contents (strong-random 7000)
        :bladder/fill-rate (* (/ 2000 24 60) 2)
        :bowels/fill-rate (* (/ 12000 24 60) 2)
        :wear (list
                  (make-instance 'yadfa/items:bandit-uniform-sports-bikini-top)
                  (make-instance 'yadfa/items:bandit-female-diaper
                      :sogginess (let ((a (strong-random 3)))
                                     (cond
                                         ((= a 0) 0)
                                         ((= a 1) (random-from-range 10 50))
                                         ((= a 2) (random-from-range 300 1000))))
                      :messiness (let ((a (strong-random 2)))
                                     (cond
                                         ((= a 0) 0)
                                         ((= a 1) 8000)))))
        :bitcoins-per-level 20))
(defclass giant-diapered-raccoon-bandit (diapered-raccoon-bandit) ()
    (:default-initargs
        :name "Giant Diapered Raccoon Bandit"
        :description "Basically we just took a Diapered Raccoon Bandit and made him bigger. Aren't we so creative at designing bosses?"
        :bitcoins-per-level 200))
