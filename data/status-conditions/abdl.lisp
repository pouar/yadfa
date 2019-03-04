(in-package :yadfa/status-conditions)
(defclass wetting (status-condition)
    ()
    (:default-initargs
        :name "Wetting"
        :description "User is currently wetting himself/herself"
        :battle-script '(lambda (target user self)
                            (declare (ignore target self))
                            (format t "~a is too busy wetting ~aself to fight~%"
                                (name-of user)
                                (if (malep user) "his" "her"))
                            (setf (bladder/contents-of user) 0))
        :duration 1
        :blocks-turn t))
(defclass messing (status-condition)
    ()
    (:default-initargs
        :name "Messing"
        :description "User is currently messing himself/herself"
        :battle-script '(lambda (target user self)
                            (declare (ignore target self))
                            (format t "~a is too busy messing ~aself to fight~%"
                                (name-of user)
                                (if (malep user) "his" "her"))
                            (setf (bowels/contents-of user) 0))
        :duration 1
        :blocks-turn t))
(defclass mushed (status-condition)
    ()
    (:default-initargs
        :name "Mushed"
        :description "User's messy diaper has been mushed"
        :battle-script '(lambda (target user self)
                            (declare (ignore target))
                            (cond ((<= (getf (calculate-diaper-usage user) :messiness) 0)
                                      (setf
                                          (getf (status-conditions-of *battle*) user)
                                          (remove self (getf (status-conditions-of *battle*) user))))
                                ((< (random 4) 1)
                                    (format t "~a is too busy grabbing the back of ~a diaper trying to unmush it to fight~%"
                                        (name-of user)
                                        (if (malep user) "his" "her"))
                                    (setf (blocks-turn-of self) t))
                                (t (setf (blocks-turn-of self) nil))))
        :duration t
        :stat-multiplier (list :speed 0.5)
        :blocks-turn t))
(defclass tickled (status-condition)
    ()
    (:default-initargs
        :name "Tickled"
        :description "User is being tickled"
        :battle-script '(lambda (target user self)
                            (declare (ignore self target))
                            (format t "~a is laughing helplessly~%" (name-of user))
                            (when
                                (or
                                    (>=
                                        (bladder/contents-of user)
                                        (bladder/potty-dance-limit-of user))
                                    (and
                                        (>=
                                            (bladder/contents-of user)
                                            (bladder/need-to-potty-limit-of user))
                                        (= 0 (random 5))))
                                (format t "~a~%"
                                    (cond
                                        ((wearingp (wear-of user) 'tabbed-briefs)
                                            (format nil "~a starts wetting ~a diapers~%"
                                                (name-of user)
                                                (if (malep user) "his" "her")))
                                        ((wearingp (wear-of user) 'pullup)
                                            (format nil "The little pictures on the front of ~a's pullups start fading~%"
                                                (name-of user)))
                                        ((wearingp (wear-of user) 'pullon)
                                            (format nil "~a starts wetting ~a pullons~%"
                                                (name-of user)
                                                (if (malep user) "his" "her")))
                                        (t
                                            (format nil "~a starts wetting ~aself~%"
                                                (name-of user)
                                                (if (malep user) "him" "her")))))
                                (set-status-condition 'wetting user)
                                (wet :wetter user)))
        :duration 1
        :blocks-turn t))
