;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defmethod documentation ((x symbol) (doc-type (eql 'event)))
  (slot-value (get-event x) 'documentation))
(defmethod print-object ((o element-type) s)
  (let ((class (slot-value (class-of o) 'name)))
    (if class
        (print-unreadable-object-with-prefix (o s :type t :identity t)
          (write class :stream s))
        (call-next-method))))
(defmethod print-object ((o element-type-class) s)
  (let ((class (slot-value o 'name)))
    (if class
        (print-unreadable-object-with-prefix (o s :type t :identity nil)
          (f:fmt s (:s class) " " (:s (class-name o))))
        (call-next-method))))
(defmethod print-object ((obj ally) stream)
  (print-unreadable-object-with-prefix (obj stream :type t :identity t)
    (print-slot obj 'name stream)))
(defmethod print-object ((obj zone) stream)
  (print-unreadable-object-with-prefix (obj stream :type t :identity t)
    (print-slot obj 'position stream)
    (write-string " " stream)
    (print-slot obj 'name stream)))
(defmethod print-object ((obj prop) stream)
  (print-unreadable-object-with-prefix (obj stream :type t :identity t)
    (print-slot obj 'name stream)))
(defmethod print-object ((obj enemy) stream)
  (print-unreadable-object-with-prefix (obj stream :type t :identity t)
    (cond ((not (slot-boundp obj 'male))
           (print-slot obj 'male stream))
          ((slot-value obj 'male)
           (write "Male" :stream stream))
          (t (write "Female" :stream stream)))
    (write-string " " stream)
    (print-slot obj 'species stream)))
(defmethod (setf health-of) (new-value (character base-character))
  (let* ((max-health (calculate-stat character :health)))
    (setf (slot-value character 'health) (cond ((< new-value 0)
                                                0)
                                               ((> new-value max-health)
                                                max-health)
                                               (t new-value)))))
(defmethod (setf health-of) (new-value (character team-member))
  (let* ((max-health (calculate-stat character :health)))
    (setf (slot-value character 'health) (cond ((< new-value 0)
                                                0)
                                               ((> new-value max-health)
                                                max-health)
                                               (t new-value)))))
(defmethod (setf energy-of) (new-value (character base-character))
  (let ((max-energy (calculate-stat character :energy)))
    (setf (slot-value character 'energy) (cond ((< new-value 0)
                                                0)
                                               ((> new-value max-energy)
                                                max-energy)
                                               (t new-value)))))
(defmethod process-potty-dance ((character base-character) attack item reload (selected-target base-character))
  (declare (ignore item reload selected-target))
  (when (process-potty-dance-check character attack)
    (format t "~a is too busy doing a potty dance to fight~%" (name-of character))
    t))
#.`(progn ,@(iter (for i in '("BLADDER" "BOWELS"))
              (appending (iter (for j in '("CONTENTS-OF" "FILL-RATE-OF"))
                           (collect `(defmethod ,(a:format-symbol :yadfa "~a/~a" i j) ((object base-character))
                                       (declare (ignore object))
                                       0))
                           (collect `(defmethod (setf ,(a:format-symbol :yadfa "~a/~a" i j)) (newval (object base-character))
                                       (declare (ignore object newval))
                                       0))))
              (appending (iter (for j in '("NEED-TO-POTTY-LIMIT-OF" "POTTY-DANCE-LIMIT-OF" "POTTY-DESPERATE-LIMIT-OF" "MAXIMUM-LIMIT-OF"))
                           (collect `(defmethod ,(a:format-symbol :yadfa "~a/~a" i j) ((object base-character))
                                       (declare (ignore object))
                                       1))
                           (collect `(defmethod (setf ,(a:format-symbol :yadfa "~a/~a" i j)) (newval (object base-character))
                                       (declare (ignore object newval))
                                       1))))))
(defmethod toggle-onesie (onesie clothes user)
  (error 'invalid-user-input :format-control "That's not a onesie"))
(defmethod toggle-onesie ((onesie onesie/opened) clothes (user base-character))
  (if (and (car (onesie-thickness-capacity-of onesie))
           (cdr clothes)
           (> (total-thickness (cdr clothes)) (car (onesie-thickness-capacity-of onesie))))
      (error 'onesie-too-thick :clothes clothes :user user)
      (toggle-onesie% onesie)))
(defmethod toggle-onesie ((onesie onesie/closed) clothes (user base-character))
  (if (lockedp onesie)
      (error 'onesie-locked :clothes clothes :user user)
      (toggle-onesie% onesie)))
(defmethod get-babyish-padding ((user team-member))
  #.`(cond ,@(iter (for i in '(diaper pullup closed-bottoms))
               (collect `((filter-items (wear-of user) ',i)
                          ',i)))
           (t nil)))
(defmethod output-process-potty-text (user padding type action had-accident &key (stream *standard-output*))
  (declare (ignore user padding type action had-accident stream)))
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt '("You feel like your bladder is going to explode"
                          "You're doing a potty dance like a 5 year old"
                          "You feel like you're going to wet yourself"
                          "You whine as you hold yourself in desperation"
                          "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :desparate)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt '("You feel like your bladder is going to explode"
                          "You're doing a potty dance like a 5 year old"
                          "You feel like you're going to wet yourself"
                          "You whine as you hold yourself in desperation"
                          "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :need-to-potty)) had-accident &key (stream *standard-output*))
  (format stream "You need to pee~%"))
(defmethod output-process-potty-text ((user player) (padding (eql 'diaper)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (let ((j (a:switch ((getf (car had-accident) :accident) :test 'eq)
                     (:dribble `("You gasp in horror as a little leaks out"
                                 "You think you just leaked a little"
                                 ,(format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                          (a:random-elt '("groan" "whine")))))
                     (:some '("You gasp in horror as you flood yourself, but manage to stop yourself"))
                     (:all (let ((a `(,(format nil
                                               "LOOK EVERYBODY!!!! ~a IS WETTING ~a DIAPERS!!!!~%~%*~a eeps and hides ~a soggy padding in embarrassment*"
                                               (string-upcase (name-of user))
                                               (if (malep user) "HIS" "HER")
                                               (name-of user)
                                               (if (malep user) "his" "her"))
                                      "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                      "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                      "You cross your legs in a vain attempt to hold it in but fail miserably"
                                      "You gasp in embarrassment as you flood yourself like a toddler"
                                      "You let out a groan as your bladder empties itself"
                                      "You fall to your knees clutching the front of your diapers struggling to keep your diapers dry and flood yourself")))
                             (unless (malep user)
                               (push "You press your legs together while fidgeting and squirming until your flood your pamps like the baby girl you are" a))
                             (when (s:memq (car (tail-of user)) '(:medium :large :lizard))
                               "You clutch the front of your diaper with your legs crossed and your tail between your legs in vain as you flood your pamps")
                             a)))))
            (when (>= (getf (car had-accident) :wet-amount) 300)
              (push (format nil "Aww, the baby is using ~a diapers?" (if (malep user) "his" "her")) j))
            (a:random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format stream  "~a~%"
            (a:random-elt '("Your face turns red as you leak everywhere"
                            "Your diaper leaks all over the place, this is why you're supposed to change it"
                            "What's the point in having a diaper if you're just going to leak everywhere like you're doing now."
                            "Your diaper leaks. There goes the carpet."
                            "Heh, baby made a puddle"
                            "Your diapers sprung a leak"
                            "You eep as you make a puddle on the floor then hide your face in embarrassment")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'pullup)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (a:switch ((getf (car had-accident) :accident) :test 'eq)
                          (:dribble `(,(format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                               (a:random-elt '("groan" "whine")))
                                      "You gasp in horror as a little leaks out"
                                      "You think you just leaked a little"))
                          (:some '("You gasp in horror as you flood yourself, but manage to stop yourself"))
                          (:all `(,(format nil "Naughty ~a wetting your pullups. You know you're supposed to use the toilet like a big kid."
                                           (if (malep user) "boy" "girl"))
                                  ,(format nil "LOOK EVERYBODY!!!! ~A IS WETTING ~a PULLUPS!!!!!!~%~%*~a eeps and hides ~a soggy pullups in embarrassment*"
                                           (string-upcase (name-of user))
                                           (if (malep user) "HIS" "HER")
                                           (name-of user)
                                           (if (malep user) "his" "her"))
                                  "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                  "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                  "You cross your legs in a vain attempt to hold it in but fail miserably"
                                  "You gasp in embarrassment as you flood yourself like a toddler"
                                  "You let out a groan as your bladder empties itself"
                                  "You fall to your knees clutching the front of your pullups struggling to keep them dry and flood yourself"
                                  "The little pictures on the front of your pullups fade showing everyone what you did")))))
  (format stream "~a~%"
          (let ((out '("Your face turns red as you leak everywhere"
                       "Your pullups leak. There goes the carpet."
                       "Heh, baby made a puddle"
                       "Your pullups sprung a leak")))
            (when (filter-items (wear-of user) '(and pullup ab-clothing))
              (push "Your pullups leaks all over the place, You sure you're ready for those?" out))
            (a:random-elt out))))
(defmethod output-process-potty-text ((user player) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (a:switch ((getf (car had-accident) :accident) :test 'eq)
                          (:dribble `(,(format nil
                                               "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                               (a:random-elt '("groan" "whine")))
                                      "You gasp in horror as a little leaks out"
                                      "You think you just leaked a little"))
                          (:some '("You gasp in  horror as you flood yourself, but manage to stop yourself"))
                          (:all '("After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                  "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                  "You cross your legs in a vain attempt to hold it in but fail miserably"
                                  "You gasp in embarrassment as you flood yourself like a toddler"
                                  "You let out a groan as your bladder empties itself"
                                  "You fall to your knees holding your crotch struggling to keep your pants dry and flood yourself")))))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt `(,(format nil "Bad ~a! No going potty in the house!" (if (= (random 2) 0) (species-of user) (name-of user)))
                            ,(format nil "Heh, baby wet ~a pants" (if (malep user) "his" "her"))
                            ,(format nil "Bad ~a! Look what you did to your pants!"
                                     (if (= (random 2) 0) (species-of user) (name-of user)))
                            "Maybe you should start wearing diapers"
                            "A puddle appears on the floor"
                            "There goes the carpet"
                            "Heh, baby made a puddle"
                            "Your pants are ruined")))))
(defmethod output-process-potty-text ((user player) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (let
              ((j (a:switch ((getf (car had-accident) :accident) :test 'eq)
                    (:dribble `(,(format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                         (a:random-elt '("groan" "whine")))
                                "You gasp in horror as a little leaks out"
                                "You think you just leaked a little"))
                    (:some '("You gasp in  horror as you flood yourself, but manage to stop yourself"))
                    (:all '("After doing a potty dance like a 5 year old, you freeze and pee yourself"
                            "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                            "You cross your legs in a vain attempt to hold it in but fail miserably"
                            "You gasp in embarrassment as you flood yourself like a toddler"
                            "You let out a groan as your bladder empties itself")))))
            (a:random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt `(,(format nil "Bad ~a! No going potty in the house!" (if (= (random 2) 0) (species-of user) (name-of user)))
                            "Maybe you should start wearing diapers"
                            "A puddle appears on the floor"
                            "There goes the carpet"
                            "Heh, baby made a puddle")))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt '("You feel like you're gonna mess yourself"
                          "You clench hard trying to avoid messing"
                          "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :desparate)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt '("You feel like you're gonna mess yourself"
                          "You clench hard trying to avoid messing"
                          "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :need-to-potty)) had-accident &key (stream *standard-output*))
  (format stream "You need to poo~%"))
(defmethod output-process-potty-text ((user player) (padding (eql 'diaper)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (let ((j `(,(format nil "Reaching the breaking point, you instinctively squat down~a and mess"
                              (if (s:memq (car (tail-of user)) '(:medium :large))
                                  " with your tail up"
                                  ""))
                     ,(format nil "You instinctively squat down~a and mess your diapers, then hold the back of your diapers checking your load in embarrassment"
                              (if (s:memq (car (tail-of user)) '(:medium :large))
                                  " with your tail up"
                                  ""))
                     ,(format nil "Heh, the baby blorted ~a pamps." (if (malep user) "his" "her"))
                     "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                     "You end up messing your self"
                     "The back of your diaper expands as you accidentally mess yourself")))
            (when (filter-items (wear-of user) '(and diaper ab-clothing))
              (push (format nil "Aww, is the baby messing ~a diapers" (if (malep user) "his" "her")) j))
            (a:random-elt j)))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt '("Your face turns red as your mess falls out the leg guards"
                            "Your diaper leaks all over the place, this is why you're supposed to change it"
                            "What's the point in having a diaper if you're just going to leak everywhere like you're doing now."
                            "Your diaper leaks. There goes the carpet."
                            "Not on the carpet!!!"
                            "Blowout!!!!")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'pullup)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (let ((j `(,(format nil "Reaching the breaking point, you instinctively squat down~a and mess"
                              (if (s:memq (car (tail-of user)) '(:medium :large))
                                  " with your tail up"
                                  ""))
                     "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                     "You end up messing your self"
                     "The back of your pullups expands as you accidentally mess yourself")))
            (when (filter-items (wear-of user) '(or ab-clothing pullup))
              (push (format nil "Naughty ~a messing your pullups. You know you're supposed to use the toilet like a big kid"
                            (if (malep user) "boy" "girl"))
                    j))
            (a:random-elt j)))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt '("Your face turns red as your mess falls out the leg guards"
                            "Your pullups leaks all over the place, You sure you're ready for those?"
                            "Your pullups leak. There goes the carpet."
                            "Not on the carpet!!!")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt `(,(format nil "Reaching the breaking point, you instinctively squat down~a and mess"
                                   (if (s:memq (car (tail-of user)) '(:medium :large))
                                       " with your tail up"
                                       ""))
                          "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                          "You end up messing your self"
                          "a lump forms at the seat of your pants")))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt `(,(format nil "Bad ~a! No going potty in the house!"
                                     (if (= (random 2) 0) (species-of user) (name-of user)))
                            ,(format nil "Heh, baby messed ~a pants" (if (malep user) "his" "her"))
                            ,(format nil "Bad ~a! Look what you did to your pants!" (if (= (random 2) 0) (species-of user) (name-of user)))
                            "Maybe you should start wearing diapers"
                            "There goes the carpet"
                            "Heh, baby made a mess"
                            "Your pants are ruined")))))
(defmethod output-process-potty-text ((user player) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt `(,(format nil "Reaching the breaking point, you instinctively squat down~a and mess"
                                   (if (s:memq (car (tail-of user)) '(:medium :large))
                                       " with your tail up"
                                       ""))
                          "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                          "You end up messing your self")))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format stream "~a~%"
            (a:random-elt `(,(format nil "Bad ~a! No going potty in the house!"
                                     (if (= (random 2) 0) (species-of user) (name-of user)))
                            "Maybe you should start wearing diapers"
                            "There goes the carpet"
                            "Heh, baby made a mess")))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'diaper)) (type (eql :wet)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* (normal
         leak
         (user-name (name-of user))
         (male (malep user))
         (player-name (name-of (player-of *game*)))
         (hisher (if male "his" "her")))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" user-name)
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" player-name)
                       (format s "~a: NUUUUU!!!!~%~%" user-name)))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" user-name)
                     (format s "~a: Did you wet yourself?~%~%" player-name)
                     (format s "~a: *quietly* No ~%~%" user-name)
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a Your diaper's leaking~%~%" player-name)
                       (format s "~a: GAH!!!!~%~%" player-name)))
            normal leak))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "*~a has an accident*~%~%" user-name)
                     (format s "~a: Aww, did the baby wet ~a diapers~%~%" player-name hisher)
                     (format s "~a: *heavily blushing* No *tries to hide it with ~a paws~a*~%~%" user-name male
                             (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                 " and tail"
                                 ""))
                     (format s "*~a squishes ~a's diaper*~%~%" player-name user-name)
                     (format s "~a: Looks like it to me~%~%" player-name))
            normal)
          (do-push (with-output-to-string (s)
                     (format s "*~a has an accident*~%~%" user-name)
                     (format s "~a: Aww, did the baby wet ~a diapers~%~%" player-name hisher)
                     (format s "~a: *heavily blushing* No *tries to hide it with ~a paws~a*~%~%"
                             user-name
                             hisher
                             (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                 " and tail"
                                 ""))
                     (format s "~a: Aww, the poor baby made puddles~%~%" player-name)
                     (format s "*~a gasps with a horrified look on ~a face when ~a notices it.~%~%" user-name hisher
                             (if male "he" "she")))
            leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             user-name hisher hisher hisher)
                     (format s "~a soggy padding, blushes heavily and quickly covers ~a soggy padding with ~a paws~a hoping no one will notice*~%~%"
                             hisher hisher hisher
                             (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                 " and tail"
                                 "")))
            normal)
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             user-name hisher hisher hisher)
                     (format s "~a padding is leaking, blushes heavily and quickly covers ~a soggy padding with ~a paws~a hoping no one will notice*~%~%"
                             hisher hisher hisher
                             (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                 " and tail"
                                 "")))
            leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'pullup)) (type (eql :wet)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* (normal
         leak
         (male (malep user))
         (user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (hisher (if (malep user) "his" "her")))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" user-name)
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" player-name)
                       (format s "~a: NUUUUU!!!!~%~%" user-name)))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" user-name)
                     (format s "~a: Did you wet yourself?~%~%" player-name)
                     (format s "~a: *quietly* No ~%~%" user-name)
                     (cond
                       ((filter-items (wear-of user) '(and pullup ab-clothing))
                        (format s "~a: Those pictures on the front of your pullups better not fade~%~%"
                                player-name)
                        (format s "~a: They're not, honest ~%~%" user-name)
                        (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                          (format s "*~a checks ~a's pullups. Notices they're drenched~%~%" player-name user-name)
                          (format s "~a: Uh huh, sure~%~%" player-name)))))
            normal leak))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             user-name hisher hisher hisher)
                     (if (filter-items (wear-of user) '(and pullup ab-clothing))
                         (format s "the pictures on ~a pullups have faded, blushes heavily and quickly covers ~a soggy pullups with ~a paws~a hoping no one will notice*~%~%"
                                 hisher hisher hisher
                                 (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                     " and tail"
                                     ""))
                         (format s "that ~a wetted ~a pullups, blushes heavily and quickly covers ~a soggy pullups with ~a paws~a hoping no one will notice*~%~%"
                                 (if male "he" "she")
                                 hisher hisher hisher
                                 (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                     " and tail"
                                     ""))))
            normal leak)
          (push (with-output-to-string (s)
                  (format s "*~a has an accident*~%~%" user-name)
                  (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look what you did to your pullups~%~%"
                          player-name user-name (if male "boy" "girl")))
                normal)
          (when (filter-items (wear-of user) '(and pullup ab-clothing))
            (push (with-output-to-string (s)
                    (format s "*~a has an accident and leaks*~%~%"
                            user-name)
                    (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look at the mess you made on the floor~%~%"
                            player-name user-name (if male "boy" "girl")))
                  leak))))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* (normal
         leak
         (male (malep user))
         (user-name (name-of user))
         (player-name (name-of (player-of *game*))))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" user-name)
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" player-name)
                       (format s "~a: NUUUUU!!!!~%~%" user-name)))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" user-name)
                     (format s "~a: Did you wet yourself?~%~%" player-name)
                     (format s "~a: *quietly* No ~%~%" user-name))
            normal leak))
        (progn
          (do-push (with-output-to-string (s)
                     (let* ((male (if male "his" "her")))
                       (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices ~a soggy padding, blushes heavily and quickly covers ~a soggy padding with ~a paws~a hoping no one will notice*~%~%"
                               user-name male male male male male male
                               (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                                   (format nil " with ~a tail between ~a legs" male male)
                                   ""))))
            normal leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let (normal
        leak
        (user-name (name-of user))
        (player-name (name-of (player-of *game*)))
        (hisher (if (malep user) "his" "her")))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" user-name)
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" player-name)
                       (format s "~a: NUUUUU!!!!~%~%" user-name)))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" user-name)
                     (format s "~a: Did you wet yourself?~%~%" player-name)
                     (format s "~a: *quietly* No ~%~%" user-name))
            normal leak))
        (progn
          (do-push
              (with-output-to-string (s)
                (format s "*~a crosses ~a legs pressing ~a paws against ~a crotch as a puddle forms beneath ~a feet*~%~%"
                        user-name hisher hisher hisher hisher))
            normal leak)
          (do-push
              (with-output-to-string (s)
                (format s "*~a has an accident and makes a mess on the floor. " user-name)
                (format s "Then walks away heavily blushing hoping no one will notice*~%~%"))
            normal leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (male (malep user))
         (hisher (if male "his" "her"))
         (himher (if male "him" "her")))
    (format stream "~a"
            (a:random-elt (if (= (random 5) 0)
                              (list (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm fine *bounces up and down holding ~aself*~%~%" user-name himher))
                                    (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm ok *hops from foot to foot holding ~a crotch*~%~%" user-name hisher))
                                    (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm alright *moans with ~a legs twisted holding ~a crotch*~%~%"
                                              user-name hisher hisher)))
                              (list (with-output-to-string (s)
                                      (format s "*~a is doing a potty dance like a 5 year old*~%~%" user-name))
                                    (with-output-to-string (s)
                                      (format s "*~a is bouncing up and down with ~a knees knocked together holding ~aself*~%~%"
                                              user-name hisher himher))
                                    (with-output-to-string (s)
                                      (format s "*~a is hopping from foot to foot*~%~%" user-name))
                                    (with-output-to-string (s)
                                      (format s "*~a starts moaning with ~a legs crossed*~%~%" user-name hisher))))))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :wet)) (action (eql :desparate)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (male (malep user))
         (hisher (if male "his" "her"))
         (himher (if male "him" "her")))
    (format stream "~a"
            (a:random-elt
             (if (= (random 5) 0)
                 (list (with-output-to-string (s)
                         (format s "~a: ~a!!! I GOTTY POTTY!!! *bounces up and down holding ~aself*~%~%" user-name player-name himher))
                       (with-output-to-string (s)
                         (format s "~a: ~a!!! HURRY!!! I CAN'T HOLD IT MUCH LONGER!!! *hops from foot to foot holding ~a crotch*~%~%"
                                 user-name player-name hisher))
                       (with-output-to-string (s)
                         (format s "~a: ~a!!! PLEASE TAKE ME TO THE POTTY!!! I'M ABOUT TO WET MYSELF!!! *bounces up and down holding ~aself*~%~%"
                                 user-name player-name himher)))
                 (list (with-output-to-string (s)
                         (format s "*~a is doing a potty dance like a 5 year old*~%~%" user-name))
                       (with-output-to-string (s)
                         (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                                 user-name hisher himher))
                       (with-output-to-string (s)
                         (format s "*~a is hopping from foot to foot*~%~%" user-name))
                       (with-output-to-string (s)
                         (format s "*~a starts moaning with ~a legs crossed*~%~%" user-name hisher))))))))
(defmethod output-process-potty-text ((user ally-rebel-potty-training) padding (type (eql :wet)) (action (eql :had-accident)) had-accident &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (male (malep user))
         (hisher (if male "his" "her")))
    (cond ((and
            (car had-accident)
            (> (getf (car had-accident) :leak-amount) 0))
           (format stream "~a"
                   (a:random-elt
                    (list (with-output-to-string (s)
                            (format s "*~a stops in his tracks*~%~%" user-name)
                            (format s "~a: Is something the matter?~%~%" player-name)
                            (format s "~a: what do you mean? *a soft hiss can be heard coming from the front of ~a diaper, then yellow streams flow down ~a legs from the leg guards and a puddle starts forming at ~a feet*~%~%"
                                    user-name hisher hisher hisher)
                            (format s "~a: You're making a puddle~%~%" player-name)
                            (format s "~a: Oh No!!!~%" user-name))
                          (with-output-to-string (s)
                            (format s "*~a floods ~a nappies, then leaks and leaves puddles*~%~%" user-name hisher))
                          (with-output-to-string (s)
                            (format s "*~a floods his nappies, then gets an expression of horror on ~a face when ~a diaper leaks and a puddle forms, then starts waddling with ~a legs spread apart*~%~%"
                                    user-name hisher hisher hisher))
                          (with-output-to-string (s)
                            (format s "*~a decides to flood ~a already waterlogged diaper, then acts all surprised when it leaks*~%~%"
                                    user-name hisher))
                          (with-output-to-string (s)
                            (format s "*~a floods his diapers and starts leaving a puddle, then freaks and waddles towards ~a with ~a legs spread apart like a 5 year old who didn't make it*~%~%"
                                    user-name player-name hisher)
                            (format s "~a: Umm ~a, I think I need a change.~%~%" user-name player-name)
                            (format s "~a: No shit~%~%" player-name))))))
          ((and (car had-accident)
                (> (getf (car had-accident) :wet-amount) 0))
           (format stream "~a"
                   (a:random-elt (list (with-output-to-string (s)
                                         (format s "*~a stops in his tracks*~%~%" user-name)
                                         (format s "~a: Is something the matter?~%~%" player-name)
                                         (format s "~a: what do you mean? *a soft hiss can be heard coming from the front of ~a diaper*~%~%"
                                                 user-name hisher)
                                         (format s "~a: Oh, never mind~%~%" player-name))
                                       (with-output-to-string (s)
                                         (format s "*~a pauses and floods ~a diapers*~%~%" user-name hisher))
                                       (with-output-to-string (s)
                                         (format s "*~a floods ~a diapers*~%~%" user-name hisher)))))))))
(defmethod output-process-potty-text ((user ally-no-potty-training) padding (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (male (malep user))
         (hisher (if male "his" "her")))
    (cond ((and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
           (format stream "~a"
                   (a:random-elt (list (with-output-to-string (s)
                                         (format s "*~a floods ~a nappies, then leaks and leaves puddles*~%~%" user-name hisher))
                                       (with-output-to-string (s)
                                         (format s "*~a floods his nappies, then gets an expression of horror on ~a face when ~a diaper leaks and a puddle forms, then starts waddling with ~a legs spread apart*~%~%"
                                                 user-name hisher hisher hisher))
                                       (with-output-to-string (s)
                                         (format s "*~a decides to flood ~a already waterlogged diaper, then acts all surprised when it leaks*~%~%"
                                                 user-name hisher))
                                       (with-output-to-string (s)
                                         (format s "*~a floods his diapers and starts leaving a puddle, then freaks and waddles towards ~a with ~a legs spread apart like a 5 year old who didn't make it*~%~%"
                                                 user-name player-name hisher)
                                         (format s "~a: Umm ~a, I think I need a change.~%~%" user-name player-name)
                                         (format s "~a: No shit~%~%" player-name))))))
          ((and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
           (format stream "*~a floods ~a diapers*~%~%" user-name hisher)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql :diaper)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* (normal
         leak
         (user-name (name-of user))
         (player-name (name-of (player-of *game*)))
         (male (malep user))
         (hisher (if male "his" "her")))
    (do-push (with-output-to-string (s)
               (format s "*~a has an accident*~%~%" user-name)
               (format s "~a: Aww, did the baby mess ~a diapers~%~%" player-name hisher)
               (format s "~a: *heavily blushing* No *tries to hide it with ~a paws~a*~%~%" user-name hisher
                       (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                           " and tail"
                           ""))
               (format s "*~a pats the back of ~a's diaper causing ~a to scrunch ~a face*~%~%" player-name user-name user-name hisher)
               (format s "~a: Looks like it to me~%~%" player-name))
      normal)
    (do-push (with-output-to-string (s)
               (format s "*~a has an accident*~%~%" user-name)
               (format s "~a: Aww, did the baby mess ~a diapers~%~%" player-name hisher)
               (format s "~a: *heavily blushing* No *tries to hide it with ~a paws~a*~%~%" user-name hisher
                       (if (s:memq (car (tail-of user)) '(:medium :large :lizard))
                           " and tail"
                           ""))
               (format s "~a: Aww, the poor baby made a mess on the floor~%~%" player-name)
               (apply #'format s "*~a gasps with a horrified look on ~a face when ~a notices it.~%~%" user-name
                      (if male
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a realized what ~a did*~%~%"
                      user-name
                      (if male
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then holds the back of ~a diapers checking ~a load in embarrassment*~%~%"
                      user-name
                      (if male
                          '("his" "his" "his" "his")
                          '("her" "her" "her" "her"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                      user-name
                      (if male
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql :pullup)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let ((normal ())
        (leak ()))
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a realized what ~a did*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (when (filter-items (wear-of user) '(and ab-clothing pullup))
      (do-push (with-output-to-string (s)
                 (format s "*~a has an accident and leaks*~%~%"
                         (name-of user))
                 (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look at the mess you made on the floor~%~%"
                         (name-of (player-of *game*))
                         (name-of user)
                         (if (malep user) "boy" "girl")))
        leak))
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let ((normal ())
        (leak ()))
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pants then gasps in horror when ~a realized what ~a did*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let ((normal ())
        (leak ()))
    (do-push
        (with-output-to-string (s)
          (format s "*~a accidentally messes ~aself as it falls on the floor*~%~%"
                  (name-of user)
                  (if (malep user) "his" "her"))
          (format s "~a: Bad ~a!!! No going potty on the floor!!!~%~%"
                  (name-of (player-of *game*))
                  (name-of user))
          (format s "~a: I didn't mean to!!!~%~%"
                  (name-of user))
          (format s "~a: A likely story~%~%"
                  (name-of (player-of *game*))))
      normal leak)
    (do-push (with-output-to-string (s)
               (format s "*~a has an accident and makes a mess on the floor. " (name-of user))
               (format s "Then walks away heavily blushing hoping no one will notice*~%~%"))
      normal leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format stream "~a" (a:random-elt leak))
        (format stream "~a" (a:random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident
                                      &key (stream *standard-output*))
  (let* ((player-name (name-of (player-of *game*)))
         (user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~a"
            (a:random-elt (if (= (random 5) 0)
                              (list (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm fine *bounces up and down holding ~aself*~%~%" user-name (if male "him" "her")))
                                    (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm ok *hops from foot to foot*~%~%" user-name))
                                    (with-output-to-string (s)
                                      (format s "~a: ~a, do you need to potty?~%~%" player-name user-name)
                                      (format s "~a: No, I'm alright *moans with ~a legs twisted*~%~%" user-name hisher)))
                              (list (with-output-to-string (s)
                                      (format s "*~a is doing a potty dance like a 5 year old*~%~%" user-name))
                                    (with-output-to-string (s)
                                      (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                                              user-name hisher (if male "him" "her")))
                                    (with-output-to-string (s)
                                      (format s "*~a is hopping from foot to foot*~%~%" user-name))
                                    (with-output-to-string (s)
                                      (format s "*~a starts moaning with ~a legs crossed*~%~%" user-name hisher))))))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :mess)) (action (eql :desparate)) had-accident
                                      &key (stream *standard-output*))
  (let* ((player-name (name-of (player-of *game*)))
         (user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~a"
            (a:random-elt (if (= (random 5) 0)
                              (list (with-output-to-string (s)
                                      (format s "~a: ~a!!! I GOTTY POTTY!!! *bounces up and down holding ~aself*~%~%" user-name player-name
                                              (if male "him" "her")))
                                    (with-output-to-string (s)
                                      (format s "~a: ~a!!! HURRY!!! I CAN'T HOLD IT MUCH LONGER!!! *hops from foot to foot holding ~a crotch*~%~%"
                                              user-name player-name hisher)))
                              (progn (with-output-to-string (s)
                                       (format s "*~a is doing a potty dance like a 5 year old*~%~%" user-name))
                                     (with-output-to-string (s)
                                       (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                                               user-name hisher (if male "him" "her")))
                                     (with-output-to-string (s)
                                       (format s "*~a is hopping from foot to foot*~%~%" user-name))
                                     (with-output-to-string (s)
                                       (format s "*~a starts moaning with ~a legs crossed*~%~%" user-name hisher))))))))
(defmethod output-process-potty-text ((user ally-rebel-potty-training) padding (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
           (format stream "*~a squats down and pushes a big load into ~a already loaded diaper, then predictably has a blowout*~%~%" user-name hisher))
          ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
           (format stream "~a"
                   (a:random-elt (list (with-output-to-string (s)
                                         (format s "*~a squats down and pushes a big load into ~a diaper like an infant*~%~%" user-name hisher))
                                       (with-output-to-string (s)
                                         (apply #'format s "*~a squats down and pushes a big load into ~a diaper then holds the back of ~a diaper checking ~a new load as if giving ~aself a diaper check*~%~%"
                                                user-name
                                                (if male
                                                    '("his" "his" "his" "him")
                                                    '("her" "her" "her" "her")))))))))))
(defmethod output-process-potty-text ((user ally-no-potty-training) padding (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (hisher (if (malep user) "his" "her")))
    (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
           (format stream "~a"
                   (with-output-to-string (s)
                     (format s "*~a squats down and pushes a big load into ~a already loaded diaper, then predictably has a blowout*~%~%"
                             user-name hisher))))
          ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
           (format stream "*~a squats down and pushes a big load into ~a diaper like an infant*~%~%"
                   user-name hisher)))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~a~%"
            (a:random-elt (let ((a (list (format nil "*~a is doing a potty dance like a 5 year old*" user-name)
                                         (format nil "*~a hops from foot to foot holding ~a crotch*" user-name hisher)
                                         (format nil "*~a bounces up and down holding ~aself*" user-name hisher))))
                            (unless male
                              (push (format nil "~a fidgets and squirms while pressing her legs together" user-name) a))
                            a)))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :wet)) (action (eql :desparate)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user)))
    (format stream "~a~%"
            (a:random-elt (let ((a (list (format nil "*~a is doing a potty dance like a 5 year old*" user-name)
                                         (format nil "*~a hops from foot to foot holding ~a crotch*" user-name (if male "his" "her"))
                                         (format nil "*~a bounces up and down holding ~aself*" user-name (if male "him" "her"))
                                         (apply #'format nil "*~a whines as ~a hold ~aself in desperation*"
                                                user-name (if male
                                                              '("he" "him")
                                                              '("she" "her"))))))
                            (unless male
                              (push (format nil "~a fidgets, squirms, and bounces while pressing her legs together" user-name)
                                    a))
                            a)))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'diaper)) (type (eql :wet)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                          user-name
                                          (if male
                                              "him"
                                              "her"))
                                  (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~a diapers like an infant*"
                                         user-name
                                         (if male
                                             '("his" "he" "his")
                                             '("her" "she" "her")))
                                  (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*" user-name hisher)
                                  (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                         user-name
                                         (if male
                                             '("he" "him")
                                             '("she" "her")))
                                  (apply #'format nil "~a falls to ~a knees clutching the front of ~a diaper in a desperate attempt to keep ~a diapers dry but ends up flooding ~a diapers"
                                         user-name
                                         (let (a)
                                           (dotimes (i 4 a)
                                             (push hisher a)))))))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                          user-name
                                          (if male "he" "she"))
                                  (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                          user-name
                                          (if male "he" "she"))
                                  (format nil "*~a's diapers sprung a leak*"
                                          user-name)
                                  (format nil "~a: Aww, looks like ~a's diapers sprung a leak~%~%*~a blushes heavily at the embarrassing comment*"
                                          (name-of (player-of *game*)) user-name user-name)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'pullup)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                          user-name
                                          (if male
                                              "him"
                                              "her"))
                                  (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                         user-name
                                         (if male
                                             '("his" "he" "him")
                                             '("her" "she" "her")))
                                  (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                          user-name
                                          hisher)
                                  (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                         user-name
                                         (if male
                                             '("he" "him")
                                             '("she" "her")))
                                  (apply #'format nil "~a falls to ~a knees clutching the front of ~a pullups in a desperate attempt to keep the pictures on the front of ~a pullups from fading but ends up flooding ~a pullups"
                                         user-name
                                         (let (a)
                                           (dotimes (i 4 a)
                                             (push hisher a)))))))))
              (when (>= (getf (car had-accident) :wet-amount) 300)
                (push (format nil "*The little pictures on the front of ~a's pullups fade showing everyone what ~a did*"
                              user-name
                              (if male "he" "she"))
                      j))
              (a:random-elt j)))
    (format stream "~a~%"
            (a:random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                        user-name
                                        (if male "he" "she"))
                                (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                        user-name
                                        (if male "he" "she"))
                                (format nil "*~a's pullups sprung a leak*"
                                        user-name))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her"))
         (player-name (name-of (player-of *game*))))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                   user-name
                                                   (if male
                                                       "him"
                                                       "her"))
                                           (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                                  user-name
                                                  (if male
                                                      '("his" "he" "him")
                                                      '("her" "she" "her")))
                                           (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                   user-name
                                                   hisher)
                                           (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                                  user-name
                                                  (if male
                                                      '("he" "him")
                                                      '("she" "her")))
                                           (apply #'format nil "~a falls to ~a knees holding ~a crotch in a desperate attempt to keep ~a pants dry but ends up flooding ~a pants"
                                                  user-name
                                                  (let (a)
                                                    (dotimes (i 4 a)
                                                      (push hisher a)))))))
                              (unless male
                                (push (format nil "~a struggles to hold it in while pressing her legs together before wetting her pants"
                                              user-name)
                                      a))
                              a)))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                       player-name user-name player-name user-name)
                              ,(format nil "~a: Heh, baby ~a made a puddle" player-name user-name)
                              ,(format nil "~a's pants are ruined" user-name)
                              ,(format nil "~a: Heh, baby ~a wet ~a pants" player-name user-name hisher)
                              ,(format nil "~a: Bad ~a! Look what you did to your pants!" player-name user-name)
                              "A puddle appears on the floor"
                              "There goes the carpet"))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her"))
         (player-name (name-of (player-of *game*))))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                   user-name
                                                   (if male
                                                       "him"
                                                       "her"))
                                           (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                                  user-name
                                                  (if male
                                                      '("his" "he" "him")
                                                      '("her" "she" "her")))
                                           (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                   user-name hisher)
                                           (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                                  user-name
                                                  (if male
                                                      '("he" "him")
                                                      '("she" "her")))
                                           (apply #'format nil "~a falls to ~a knees holding ~a crotch in a desperate attempt to keep from wetting ~aself but ends up wetting ~a pants anyway"
                                                  user-name
                                                  (if male
                                                      '("his" "his" "him" "his")
                                                      '("her" "her" "her" "her"))))))
                              (unless male
                                (push
                                 (format nil "~a struggles to hold it in while pressing her legs together until urine starts flowing down her legs"
                                         user-name)
                                 a))
                              a)))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                       player-name user-name player-name user-name)
                              ,(format nil "~a: Heh, baby ~a made a puddle"
                                       player-name user-name)
                              "A puddle appears on the floor"
                              "There goes the carpet"))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                              (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                     (name-of user)
                                     (if (malep user)
                                         '("his" "him")
                                         '("her" "her")))
                              (format nil "*~a is hopping from foot to foot holding the ~a*"
                                      (name-of user)
                                      (funcall (if (malep user)
                                                   #'car
                                                   #'cdr)
                                               (getf '(diaper ("seat of his diapers" . "seat of her diapers")
                                                       pullup ("seat of his pullups" . "seat of her pullups")
                                                       closed-bottoms ("seat of his pants" . "seat of her pants")
                                                       nil ("back of himself" . "back of herself"))
                                                     padding)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :mess)) (action (eql :desparate)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                              (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                     (name-of user)
                                     (if (malep user)
                                         '("his" "him")
                                         '("her" "her")))
                              (format nil "*~a is hopping from foot to foot holding the ~a*"
                                      (name-of user)
                                      (funcall
                                       (if (malep user)
                                           #'car
                                           #'cdr)
                                       (getf '(diaper ("seat of his diapers" . "seat of her diapers")
                                               pullup ("seat of his pullups" . "seat of her pullups")
                                               closed-bottoms ("seat of his pants" . "seat of her pants")
                                               nil ("back of himself" . "back of herself"))
                                             padding)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'diaper)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~{~a~}~%"
            (let ((a (list (a:random-elt (list (format nil "*~a instinctively squats down~a and mess ~a diapers*"
                                                       user-name hisher
                                                       (if (s:memq (car (tail-of user)) '(:medium :large))
                                                           (format nil " with ~a tail up" hisher)
                                                           ""))
                                               (apply #'format nil
                                                      "*The back of ~a's diaper expands as ~a accidentally messes ~aself*"
                                                      user-name
                                                      (if male
                                                          '("he" "him")
                                                          '("she" "her")))
                                               (format nil "*~a instinctively squats down~a and messes ~a diapers then holds the back of ~a diapers checking ~a load in embarrassment*~%~%"
                                                       user-name
                                                       (if (s:memq (car (tail-of user)) '(:medium :large))
                                                           (format nil " with ~a tail up"
                                                                   hisher)
                                                           "")
                                                       hisher hisher hisher)))))
                  (b (a:random-elt `(,(format nil "~%~%~a: Heh, baby ~a blorted ~a pamps."
                                              (name-of (player-of *game*))
                                              user-name
                                              hisher)
                                     nil))))
              (when b (push b (cdr (last a))))))
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
      (format stream "*~a*~%"
              (a:random-elt `(,(format nil "~a face turns red as ~a mess falls out the leg guards"
                                       user-name
                                       hisher)
                              "Blowout!!!!"))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'pullup)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~{~a~}~%"
            (let ((a (list (a:random-elt (list (format nil "*~a instinctively squats down~a and mess ~a pullups*"
                                                       user-name
                                                       (if (s:memq (car (tail-of user)) '(:medium :large))
                                                           (format nil " with ~a tail up"
                                                                   hisher)
                                                           "")
                                                       hisher)
                                               (apply #'format nil "*The back of ~a's pullups expands as ~a accidentally messes ~aself*"
                                                      user-name
                                                      (if male
                                                          '("he" "him")
                                                          '("she" "her")))))))
                  (b (a:random-elt `(,(format nil "~%~%~a: Bad ~a!!! You know you're supposed to use the toilet like a big kid"
                                              (name-of (player-of *game*))
                                              user-name)
                                     nil))))
              (when b (push b (cdr (last a))))))
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
      (format stream "*~a*~%"
              (a:random-elt (list (format nil "~a face turns red as ~a mess falls out the leg guards"
                                          user-name hisher)
                                  (format nil "~a pullups leak all over the place" user-name)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident))
                                      had-accident &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her"))
         (player-name (name-of (player-of *game*))))
    (format stream "*~a*~%"
            (a:random-elt (list (format nil "~a instinctively squats down~a and messes ~a pants"
                                        user-name
                                        (if (s:memq (car (tail-of user)) '(:medium :large))
                                            (format nil " with ~a tail up"
                                                    hisher)
                                            "")
                                        hisher)
                                (apply #'format nil "a lump forms at the seat of ~a's pants"
                                       user-name))))
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                          player-name user-name player-name user-name)
                                  (format nil "*~a's pants are ruined*" user-name)
                                  (format nil "*~a makes a mess on the floor*" user-name)
                                  (format nil "~a: Heh, baby ~a messed ~a pants" player-name user-name hisher)
                                  (format nil "~a: Bad ~a! Look what you did to your pants!" (name-of (player-of *game*)) (name-of user))))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (player-name (name-of (player-of *game*))))
    (format stream "*~a*~%"
            (a:random-elt (list (format nil "Reaching the breaking point, ~a instinctively squats down~a and messes"
                                        user-name
                                        (if (s:memq (car (tail-of user)) '(:medium :large))
                                            (format nil " with ~a tail up" (if (malep user) "his" "her"))
                                            ""))
                                (format nil "~a has an accident and makes a mess on the floor" user-name))))
    (let ((a (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                      player-name user-name player-name user-name)
                             nil))))
      (when a
        (format stream "~a~%" a)))))


(defmethod output-process-potty-text ((user ally-feral) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident &key (stream *standard-output*))
  (let ((user-name (name-of user)))
    (format stream "~a~%"
            (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" user-name)
                                (format nil "*~a hops from foot to foot*" user-name)
                                (format nil "*~a runs in circles like a dog needing to potty*" user-name)
                                (format nil "~a fidgets and squirms while pressing ~a legs together"
                                        (if (malep user) "his" "her")
                                        user-name))))))
(defmethod output-process-potty-text ((user ally-feral) padding (type (eql :wet)) (action (eql :desparate)) had-accident &key (stream *standard-output*))
  (let ((user-name (name-of user)))
    (format stream "~a~%"
            (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" user-name)
                                (format nil "*~a hops from foot to foot*" user-name)
                                (format nil "*~a runs in circles like a dog needing to potty*" user-name)
                                (format nil "~a fidgets and squirms while pressing ~a legs together"
                                        (if (malep user) "his" "her")
                                        user-name))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'diaper)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (male (malep user))
        (player-name (name-of (player-of *game*))))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                          user-name
                                          (if male
                                              "him"
                                              "her"))
                                  (apply #'format nil "*~a pauses and blushes as ~a flood ~a diapers like an infant*"
                                         user-name
                                         (if male
                                             '("his" "he" "his")
                                             '("her" "she" "her")))
                                  (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                          user-name
                                          (if male "his" "her"))
                                  (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                         user-name
                                         (if male
                                             '("he" "him")
                                             '("she" "her"))))))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                          user-name
                                          (if male "he" "she"))
                                  (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                          user-name
                                          (if male "he" "she"))
                                  (format nil "*~a's diapers sprung a leak*"
                                          user-name)
                                  (format nil "~a: Aww, looks like ~a's diapers sprung a leak~%~%*~a blushes heavily at the embarrassing comment*"
                                          player-name user-name user-name)))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'pullup)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (male (malep user)))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                          user-name
                                          (if male
                                              "him"
                                              "her"))
                                  (apply #'format nil "*~a pauses and blushes as ~a flood ~aself like an infant*"
                                         user-name
                                         (if male
                                             '("his" "he" "him")
                                             '("her" "she" "her")))
                                  (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                          user-name
                                          (if male
                                              "his"
                                              "her"))
                                  (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                         user-name
                                         (if male
                                             '("he" "him")
                                             '("she" "her"))))))))
              (when (>= (getf (car had-accident) :wet-amount) 300)
                (push (format nil "*The little pictures on the front of ~a's pullups fade showing everyone what ~a did*"
                              user-name
                              (if male "he" "she"))
                      j))
              (a:random-elt j)))
    (format stream "~a~%"
            (a:random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                        user-name
                                        (if male "he" "she"))
                                (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                        user-name
                                        (if male "he" "she"))
                                (format nil "*~a's pullups sprung a leak*"
                                        user-name))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (player-name (name-of (player-of *game*)))
         (hisher (if male "his" "her")))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                   user-name
                                                   (if male
                                                       "him"
                                                       "her"))
                                           (apply #'format nil "*~a  pauses and blushes as ~a flood ~aself like an infant*"
                                                  user-name
                                                  (if male
                                                      '("his" "he" "him")
                                                      '("her" "she" "her")))
                                           (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                   user-name
                                                   hisher)
                                           (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                                  user-name
                                                  (if male
                                                      '("he" "him")
                                                      '("she" "her")))
                                           (format nil "~a struggles to hold it in while pressing ~a legs together before wetting ~a pants"
                                                   user-name
                                                   hisher
                                                   hisher))))
                              a)))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                       player-name
                                       user-name
                                       player-name
                                       user-name)
                              ,(format nil "~a: Heh, baby ~a made a puddle"
                                       player-name
                                       user-name)
                              ,(format nil "~a's pants are ruined"
                                       user-name)
                              ,(format nil "~a: Heh, baby ~a wet ~a pants"
                                       player-name
                                       user-name
                                       hisher)
                              ,(format nil "~a: Bad ~a! Look what you did to your pants!"
                                       player-name
                                       user-name)
                              "A puddle appears on the floor"
                              "There goes the carpet"))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (male (malep user))
        (player-name (name-of (player-of *game*))))
    (format stream "~a~%"
            (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                            (list (format nil "*~a gasps in horror as a little leaks out*" user-name)
                                  (format nil "*~a's bladder just leaked a little*" user-name)))
                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                            (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                         user-name
                                         (if male
                                             '("he" "him" "him")
                                             '("she" "her" "he")))))
                           ((> (getf (car had-accident) :wet-amount) 300)
                            (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                   user-name
                                                   (if male
                                                       "him"
                                                       "her"))
                                           (apply #'format nil "*~a pauses and blushes as ~a flood ~aself like an infant*"
                                                  user-name
                                                  (if male
                                                      '("his" "he" "him")
                                                      '("her" "she" "her")))
                                           (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                   user-name
                                                   (if male
                                                       "his"
                                                       "her"))
                                           (apply #'format nil "~a gasps in embarrassment as ~a floods ~aself like a toddler"
                                                  user-name
                                                  (if male
                                                      '("he" "him")
                                                      '("she" "her")))
                                           (apply #'format nil "~a struggles to hold it in while pressing ~a legs together until urine starts flowing down ~a legs"
                                                  user-name
                                                  (if male
                                                      '("his" "his")
                                                      '("her" "her"))))))
                              a)))))
              (a:random-elt j)))
    (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                       player-name
                                       user-name
                                       player-name
                                       user-name)
                              ,(format nil "~a: Heh, baby ~a made a puddle"
                                       player-name
                                       user-name)
                              "A puddle appears on the floor"
                              "There goes the carpet"))))))
(defmethod output-process-potty-text ((user ally-feral) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                              (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                     (name-of user)
                                     (if (malep user)
                                         '("his" "him")
                                         '("her" "her")))
                              (format nil "*~a hunches down with ~a legs arched*" (name-of user) (if (malep user) "his" "her"))))))
(defmethod output-process-potty-text ((user ally-feral) padding (type (eql :mess)) (action (eql :desparate)) had-accident &key (stream *standard-output*))
  (format stream "~a~%"
          (a:random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                              (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                     (name-of user)
                                     (if (malep user)
                                         '("his" "him")
                                         '("her" "her")))
                              (format nil "*~a hunches down with ~a legs arched*" (name-of user) (if (malep user) "his" "her"))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'diaper)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let* ((user-name (name-of user))
         (male (malep user))
         (hisher (if male "his" "her")))
    (format stream "~{~a~}~%"
            (let ((a (list (a:random-elt (list (format nil "*~a instinctively squats down~a and mess ~a diapers*"
                                                       user-name
                                                       (if male
                                                           "his" "her")
                                                       (if (s:memq (car (tail-of user)) '(:medium :large))
                                                           (format nil " with ~a tail up"
                                                                   hisher)
                                                           ""))
                                               (apply #'format nil
                                                      "*The back of ~a's diaper expands as ~a accidentally messes ~aself*"
                                                      user-name
                                                      (if male
                                                          '("he" "him")
                                                          '("she" "her")))))))
                  (b (a:random-elt `(,(format nil "~%~%~a: Heh, baby ~a blorted ~a pamps."
                                              (name-of (player-of *game*))
                                              user-name
                                              hisher)
                                     nil))))
              (when b (push b (cdr (last a))))))
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
      (format stream "*~a*~%"
              (a:random-elt `(,(format nil "~a face turns red as ~a mess falls out the leg guards"
                                       user-name
                                       hisher)
                              "Blowout!!!!"))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'pullup)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (format stream "~{~a~}~%"
          (let ((a (list (a:random-elt (list (format nil "*~a instinctively squats down~a and mess ~a pullups*"
                                                     (name-of user)
                                                     (if (s:memq (car (tail-of user)) '(:medium :large))
                                                         (format nil " with ~a tail up"
                                                                 (if (malep user)
                                                                     "his" "her"))
                                                         "")
                                                     (if (malep user)
                                                         "his" "her"))
                                             (apply #'format nil "*The back of ~a's pullups expands as ~a accidentally messes ~aself*"
                                                    (name-of user)
                                                    (if (malep user)
                                                        '("he" "him")
                                                        '("she" "her")))))))
                (b (a:random-elt `(,(format nil "~%~%~a: Bad ~a!!! You know you're supposed to use the toilet like a big kid"
                                            (name-of (player-of *game*))
                                            (name-of user))
                                   nil))))
            (when b (push b (cdr (last a))))))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format stream "*~a*~%"
            (a:random-elt (list (format nil "~a face turns red as ~a mess falls out the leg guards"
                                        (name-of user)
                                        (if (malep user)
                                            "his"
                                            "her"))
                                (format nil "~a pullups leak all over the place" (name-of user)))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (player-name (name-of (player-of *game*)))
        (hisher (if (malep user) "his" "her")))
    (format stream "*~a*~%"
            (a:random-elt (list (format nil "~a instinctively squats down~a and messes ~a pants"
                                        user-name
                                        (if (s:memq (car (tail-of user)) '(:medium :large))
                                            (format nil " with ~a tail up"
                                                    hisher)
                                            "")
                                        hisher)
                                (apply #'format nil "a lump forms at the seat of ~a's pants"
                                       user-name))))
    (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
      (format stream "~a~%"
              (a:random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                          player-name
                                          user-name
                                          player-name
                                          user-name)
                                  (format nil "*~a's pants are ruined*" user-name)
                                  (format nil "*~a makes a mess on the floor*" user-name)
                                  (format nil "~a: Heh, baby ~a messed ~a pants"
                                          player-name
                                          user-name
                                          hisher)
                                  (format nil "~a: Bad ~a! Look what you did to your pants!" player-name user-name)))))))
(defmethod output-process-potty-text ((user ally-feral) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident
                                      &key (stream *standard-output*))
  (let ((user-name (name-of user))
        (player-name (name-of (player-of *game*))))
    (format stream "*~a*~%"
            (a:random-elt (list (format nil "Reaching the breaking point, ~a instinctively squats down~a and messes"
                                        user-name
                                        (if (s:memq (car (tail-of user)) '(:medium :large))
                                            (format nil " with ~a tail up"
                                                    (if (malep user)
                                                        "his" "her"))
                                            ""))
                                (format nil "~a has an accident and makes a mess on the floor" (name-of user)))))
    (let ((a (a:random-elt `(,(format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                      player-name user-name player-name user-name)
                             nil))))
      (when a
        (format stream "~a~%" a)))))
(defmethod calculate-damage ((target base-character) (user base-character) (attack real))
  "Figures out the damage dealt, we use the formula

 @mathjax{\\left({\\left({2 \\times level \\over 5}+2\\right) \\times attackbase \\times {attack \\over defense} \\over 50}+2\\right) \\times {randomrange \\over 100}}

 which is the same as Pokèmon

level is @code{(level-of @var{user})}

attackbase is @var{attack-base}

attack is @code{(calculate-stat @var{user} :attack)}

defense is @code{(calculate-stat @var{user} :defense)}

randomrange is @code{(random-from-range 85 100)}"
  (round (u:$ (u:$ (u:$ (u:$ (u:$ (u:$ (u:$ 2 * (level-of user)) / 5) + 2) * attack * (u:$ (calculate-stat user :attack) / (calculate-stat target :defense)))
                        / 50)
                   + 2)
              * (u:$ (random-from-range 85 100) / 100))))
(defmethod calculate-damage ((target base-character) (user base-character) (attack move))
  "Figures out the damage dealt, we use the formula

 @mathjax{\\left({\\left({2 \\times level \\over 5}+2\\right) \\times attackbase \\times {attack \\over defense} \\over 50}+2\\right) \\times {randomrange \\over 100}}

 which is the same as Pokèmon

level is @code{(level-of @var{user})}

attackbase is @var{attack-base}

attack is @code{(calculate-stat @var{user} :attack)}

defense is @code{(calculate-stat @var{user} :defense)}

randomrange is @code{(random-from-range 85 100)}"
  (let ((attack-element-types (element-types-of attack))
        (target-element-types (element-types-of target))
        (user-element-types (element-types-of user)))
    (s:mvlet ((type-effectiveness magnitude (effective-type-effectiveness attack-element-types target-element-types)))
      (round (u:$ (u:$ (u:$ (u:$ (u:$ (u:$ (u:$ 2 * (level-of user)) / 5) + 2) * (power-of attack) *
                                 (u:$ (calculate-stat user :attack) / (calculate-stat target :defense)))
                            / 50)
                       + 2)
                  * (* (u:$ (random-from-range 85 100) / 100)
                       (if (eq type-effectiveness :no-effect)
                           0
                           (expt 2 magnitude))
                       (if (intersection user-element-types attack-element-types
                                         :key (lambda (o)
                                                (class-of (coerce-element-type o)))
                                         :test 'subtypep)
                           1.5
                           1)))))))

(defmethod describe-diaper-wear-usage (item))
(defmethod describe-diaper-inventory-usage (item))
(defmethod describe-diaper-usage (item))
(defmethod describe-diaper-inventory-usage ((item closed-bottoms))
  (iter (for (a b) on (wet-text-of item) by #'cddr)
    (when (>= (sogginess-of item) a)
      (f:fmt* t #\Space b #\Newline)
      (finish)))
  (iter (for (a b) on (mess-text-of item) by #'cddr)
    (when (>= (messiness-of item) a)
      (f:fmt* t #\Space b #\Newline)
      (finish))))
(defmethod describe-diaper-wear-usage ((item closed-bottoms))
  (iter (for (a b) on (wear-wet-text-of item) by #'cddr)
    (when (>= (sogginess-of item) a)
      (f:fmt* t #\Space b #\Newline)
      (finish)))
  (iter (for (a b) on (wear-mess-text-of item) by #'cddr)
    (when (>= (messiness-of item) a)
      (f:fmt* t #\Space b #\Newline)
      (finish)))
  (iter (for (a b) on (bulge-text-of item) by #'cddr)
    (when (>= (total-thickness item) a)
      (f:fmt* t #\Space b #\Newline)
      (finish))))
(defmethod describe-diaper-usage ((item closed-bottoms))
  (f:fmt t
         "Sogginess: " (sogginess-of item) #\Newline
         "Sogginess Capacity: " (sogginess-capacity-of item) #\Newline
         "Messiness: " (messiness-of item) #\Newline
         "Messiness Capacity: " (messiness-capacity-of item) #\Newline))
(defmethod process-battle-turn ((character npc) attack item reload selected-target)
  (when (handle-status-effects character t)
    (return-from process-battle-turn))
  (cond ((process-battle-accident character attack item reload selected-target)
         nil)
        ((iter (for j in (status-conditions-of character))
           (when (blocks-turn-of j)
             (leave t))))
        ((process-potty-dance character attack item reload selected-target) t)
        ((and (wield-of character)
              (ammo-type-of (wield-of character))
              (list-length->= 0 (ammo-of (wield-of character)))
              (> (ammo-capacity-of (wield-of character)) 0)
              (ammo-type-of (wield-of character))
              (iter (for i in (inventory-of character))
                (when (typep i (ammo-type-of (wield-of character)))
                  (leave t))))
         (format t "~a reloaded ~a ~a"
                 (name-of character)
                 (if (malep character)
                     "his"
                     "her")
                 (name-of (wield-of character)))
         (iter (with count = 0)
           (for item in (inventory-of character))
           (when (or (list-length-<= (ammo-capacity-of (wield-of character)) (ammo-of (wield-of character)))
                     (and (reload-count-of (wield-of character)) (>= count (reload-count-of (wield-of character)))))
             (leave t))
           (when (typep item (ammo-type-of (wield-of character)))
             (incf count)
             (push item (ammo-of (wield-of character)))
             (a:deletef item (inventory-of character) :count 1))))
        (t
         (battle-script character (a:random-elt (if (typep character 'enemy)
                                                    (team-of *game*)
                                                    (enemies-of *battle*)))))))
(defmethod process-battle-turn ((character base-character) attack item reload selected-target)
  (when (handle-status-effects character t)
    (return-from process-battle-turn))
  (cond ((process-battle-accident character attack item reload selected-target)
         nil)
        ((iter (for j in (status-conditions-of character))
           (when (blocks-turn-of j)
             (leave t))))
        ((process-potty-dance character attack item reload selected-target) t)
        (item
         (format t "~a used ~a ~a on ~a~%"
                 (name-of character)
                 (if (malep character) "his" "her")
                 (name-of (nth item (inventory-of (player-of *game*))))
                 (name-of selected-target))
         (use-item% item character :target selected-target))
        (reload (format t "~a reloaded ~a ~a"
                        (name-of character)
                        (if (malep character)
                            "his"
                            "her")
                        (name-of (wield-of character)))
                (iter (with count = 0)
                  (for item in (inventory-of (player-of *game*)))
                  (when (or
                         (list-length-<= (ammo-capacity-of (wield-of character))
                                         (ammo-of (wield-of character)))
                         (and
                          (reload-count-of (wield-of character))
                          (>=
                           count
                           (reload-count-of (wield-of character)))))
                    (leave t))
                  (when (and (typep item reload) (typep item (ammo-type-of (wield-of character))))
                    (incf count)
                    (push item (ammo-of (wield-of character)))
                    (a:deletef item (inventory-of (player-of *game*)) :count 1))))
        ((eq attack t)
         (if (wield-of character)
             (progn (attack selected-target character (wield-of character))
                    (when (ammo-of (wield-of character))
                      (pop (ammo-of (wield-of character)))))
             (attack selected-target character nil)))
        (attack
         (attack selected-target character (get-move attack character))
         (decf (energy-of character) (energy-cost-of attack)))))
