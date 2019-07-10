(in-package :yadfa-moves)
(defclass superglitch (stat/move) ()
  (:default-initargs
   :name "Superglitch"
   :description "Classic glitch move from the pokemon games, but without the undefined behavior and unwanted side effects."
   :attack '(lambda (target user self)
             (declare (ignore self))
             (format t
              "TMTRAINER ~a is frozen solid~%TMTRAINER ~a is hurt by the burn~%"
              (name-of user)
              (name-of user))
             (setf (health-of target) 0))))
(defclass watersport (stat/move) ()
  (:default-initargs
   :name "Watersport"
   :description "Soak your diapers"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (format t "~a wet ~a~%" (name-of user) (if (malep user) "himself" "herself"))
             (wet :force-fill-amount (if (< (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                                         (bladder/need-to-potty-limit-of user)
                                         (bladder/contents-of user))
                  :wetter user))))
(defclass mudsport (stat/move) ()
  (:default-initargs
   :name "Mudsport"
   :description "mess your diapers"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (format t "~a messed ~a~%"
              (name-of user)
              (if (malep user) "himself" "herself"))
             (mess :force-fill-amount (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                                          (bowels/need-to-potty-limit-of user)
                                          (bowels/contents-of user))
                   :messer user))))
(defclass tickle (stat/move) ()
  (:default-initargs
   :name "Tickle"
   :description "Tickle the enemy"
   :attack '(lambda (target user self)
             (format t "~a used ~a~%" (name-of user) (name-of self))
             (if (getf (attributes-of target) :not-ticklish)
                 (write-line "It has no effect")
                 (progn (format t "~a starts laughing helplessly~%" (name-of target))
                        (set-status-condition 'yadfa-status-conditions:tickled target))))))
(defclass tackle (stat/move) ()
  (:default-initargs
   :name "Tackle"
   :description "Tackles the enemey"))
