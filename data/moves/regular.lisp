;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-moves"; coding: utf-8-unix; -*-
(in-package :yadfa-moves)
(defclass mush (move) ()
  (:default-initargs
   :name "Mush"
   :description "Mush the target's diaper"))
(defmethod attack ((target base-character) (user base-character) (attack mush))
  (declare (ignore attack))
  (if (filter-items (wear-of user) 'incontinence-product)
      (progn
        (format t "~a mushes the back of ~a's diaper!~%" (name-of user) (name-of target))
        (if (<= (getf (calculate-diaper-usage target) :messiness) 0)
            (format t "But it had no effect!~%")
            (progn (format t "~a's diaper has been mushed~%" (name-of target))
                   (set-status-condition 'yadfa-status-conditions:mushed target))))
      (f:fmt t "it has no effect on " (name-of target) #\Newline)))
(defclass pants (move) ()
  (:default-initargs
   :name "Pants"
   :description "Pants the enemy"))
(defmethod attack ((target base-character) (user base-character) (self pants))
  (declare (ignore self))
  (format t "~a tries to pants ~a~%" (name-of user) (name-of target))
  (format t "The attack has no effect on ~a~%" (name-of target)))
(defmethod attack ((target pantsable-character) (user base-character) (self pants))
  (declare (ignore self))
  (let* ((pants (filter-items (wear-of target) '(or pants skirt dress)))
         (stat
           (when pants
             (iter (for i in (wear-of target))
               (when (typep i '(or diaper pullup))
                 (let ((severity (cond ((and (> (sogginess-of i) 300) (> (messiness-of i) 4000))
                                        'both)
                                       ((> (messiness-of i) 4000)
                                        'messy)
                                       ((> (sogginess-of i) 300)
                                        'soggy)))
                       (padding i))
                   (leave `(padding ,padding severity ,severity)))))))
         (old-condition (find 'yadfa-status-conditions:pantsed (getf (status-conditions-of *battle*) target)
                              :test (lambda (o e)
                                      (typep e o)))))
    (if stat
        (progn
          (cond ((filter-items pants '(or pants skirt))
                 (format t "~a pantses ~a~%"
                         (name-of user)
                         (name-of target)))
                ((filter-items pants 'dress)
                 (format t "~a raises ~a's ~a~%"
                         (name-of user)
                         (name-of target)
                         (name-of (car (filter-items pants 'dress))))))
          (unless old-condition
            (push (make-instance 'yadfa-status-conditions:pantsed) (getf (status-conditions-of *battle*) target)))
          (format t "~a gets a horrified look on ~a face as ~a ~a is exposed to the world~%"
                  (name-of target)
                  (if (malep target) "his" "her")
                  (if (malep target) "his" "her")
                  (cond ((getf stat 'both)
                         (format nil "soggy mushy padding"))
                        ((getf stat 'messy)
                         "messy padding")
                        ((getf stat 'soggy)
                         "soggy padding")
                        (t "padding")))
          (let ((audience (iter (for i in (if (typep target 'enemy)
                                              (enemies-of *battle*)
                                              (team-of *game*)))
                            (unless (eq target i)
                              (collect i)))))
            (when audience
              (format t (if (> (list-length audience) 1)
                            "~a's team mates start laughing at ~a~%"
                            "~a's team mate starts laughing at ~a~%")
                      (name-of target)
                      (if (malep target)
                          "him"
                          "her"))
              (unless old-condition
                (iter (for i in audience)
                  (set-status-condition 'yadfa-status-conditions:laughing i))))))
        (progn
          (format t "~a tries to pants ~a~%" (name-of user) (name-of target))
          (format t "The attack has no effect on ~a~%" (name-of target))))))
(defclass spray (move) ()
  (:default-initargs
   :name "Spray"
   :description "Spray the target with skunk spray. Also fills your pamps with skunk spray while you're at it."
   :energy-cost 5))
(defmethod attack ((target base-character) (user base-character) (attack spray))
  (format t "~a used ~a~%" (name-of user) (name-of attack))
  (let ((amount 50))
    (iter (while (> amount 0))
      (for i in (reverse (wear-of user)))
      (when (typep i 'closed-bottoms)
        (cond ((> amount (- (sogginess-capacity-of i) (sogginess-of i)))
               (decf amount (- (sogginess-capacity-of i) (sogginess-of i)))
               (setf (sogginess-of i) (sogginess-capacity-of i)))
              ((> amount 0)
               (incf (sogginess-of i) amount)
               (setf amount 0))))))
  (let ((clothing (filter-items (wear-of user) 'closed-bottoms)))
    (cond
      ((filter-items clothing 'incontinence-product)
       (format t "~a tries to spray the enemy, but ends up spraying in ~a pamps instead~%"
               (name-of user)
               (if (malep user) "his" "her")))
      (clothing
       (format t "~a tries to spray the enemy, but ends up spraying in ~a pants instead~%"
               (name-of user)
               (if (malep user) "his" "her")))
      (t
       (format t "~a sprays the enemy~%"
               (name-of user)))))
  (format t "~a is grossed out by the smell~%" (name-of target))
  (set-status-condition 'yadfa-status-conditions:skunked target))
(defclass boop (move) ()
  (:default-initargs
   :name "Boop"
   :description "Boops da target on da snoot"
   :energy-cost 5))
(defmethod attack ((target base-character) (user base-character) (attack boop))
  (let ((user-name (name-of user))
        (target-name (name-of target)))
    (f:fmt t target-name " blushes as " user-name " boops " target-name " on da snoot :3" #\Newline)))
(defmethod attack ((target yadfa-enemies:skunk-boop-mixin) (user base-character) (attack boop))
  (let* ((user-name (name-of user))
         (target-name (name-of target))
         (target-male-p (malep target)))
    (f:fmt t target-name " blushes as " user-name " boops " target-name " on da snoot :3" #\Newline
           target-name " immediately squats down and messes " (if target-male-p "his" "her") " pamps." #\Newline
           "It's like a mess button." #\Newline)
    (mess :force-fill-amount (bowels/maximum-limit-of target))
    (set-status-condition 'yadfa-status-conditions:messing target)))
(defclass fire-breath (move) ()
  (:default-initargs
   :name "Fire Breath"
   :energy-cost 5
   :power 60
   :description "Breathes fire at the enemy"))
(defmethod attack ((target base-character) (user base-character) (self fire-breath))
  (let ((a (calculate-damage target user (power-of self))))
    (format t "~a used ~a~%" (name-of user) (name-of self))
    (decf (health-of target) a)
    a))
(defclass face-sit (mess-move-mixin) ()
  (:default-initargs
   :name "Face Sit"
   :energy-cost 3
   :power 40
   :description "Sits on the enemy's face and messes"))
(defmethod attack ((target base-character) (user base-character) (self face-sit))
  (format t "~a used ~a~%" (name-of user) (name-of self))
  (let* ((m (mess :messer user))
         (c (calculate-diaper-usage user))
         (a (calculate-damage target user (power-of self))))
    (if (> (getf m :mess-amount) 0)
        (format t "~a sits on ~a's face and messes~%" (name-of user) (name-of target))
        (format t "~a sits on ~a's face~%" (name-of user) (name-of target)))
    (when (>= (getf c :messiness) 2000)
      (format t "~a is grossed out by the smell~%" (name-of target))
      (set-status-condition 'yadfa-status-conditions:skunked target))
    (format t "~a is damaged by the impact~%" (name-of target))
    (decf (health-of target) a)
    a))
(defclass teleporting-flood (wet-move-mixin) ()
  (:default-initargs
   :name "Teleporting Flood"
   :description "Flood your diapers, but enchants the diaper so it all teleports into someone else's diaper."))
(defmethod attack ((target base-character) (user base-character) (self teleporting-flood))
  (format t "~a used ~a~%" (name-of user) (name-of self))
  (if (< (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (wet :wetter user :clothes (wear-of target))
             (format t "~a gets a freaked expression on ~a face as ~a floods ~a's pamps~%" (name-of target) (if (malep target) "his" "her")
                     (name-of user) (name-of target)))))
(defclass teleporting-mess (mess-move-mixin) ()
  (:default-initargs
   :name "Teleporting Mess"
   :description "Mess your diapers, but enchants the diaper so it all teleports into someone else's diaper."))
(defmethod attack ((target base-character) (user base-character) (self teleporting-mess))
  (format t "~a used ~a~%" (name-of user) (name-of self))
  (if (< (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
      (format t "But it failed~%")
      (progn (mess :messer user :clothes (wear-of target))
             (format t "~a gets a freaked expression on ~a face as ~a messes ~a's pamps~%" (name-of target) (if (malep target) "his" "her")
                     (name-of user) (name-of target)))))
(defclass fart (mess-move-mixin) ()
  (:default-initargs
   :name "fart"
   :description "Grosses out the enemies with gas. If poisoned or if desperate, you may end up messing yourself instead."
   :energy-cost 10))
(defmethod attack ((target base-character) (user base-character) (attack fart))
  (f:fmt t (name-of user) " used " (name-of attack) #\Newline
         "But it failed." #\Newline))
(defmethod attack ((target base-character) (user potty-character) (attack fart))
  (f:fmt t (name-of user) " squats down and tries to use " (name-of attack) #\Newline)
  (cond
    ((and (>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
          (find 'yadfa-status-conditions:poisoned (getf (status-conditions-of *battle*) user)
                :test (lambda (o e)
                        (typep e o))))
     (mess :messer user)
     (let* ((padding (get-babyish-padding user))
            (malep (malep user))
            (name (name-of user))
            (his/her (if malep "his" "her"))
            (he/she (if malep "he" "she"))
            (himherself (if malep "himself" "herself")))
       (f:fmt t "*SPLORCH*" #\Newline
              name " grabs the back of " (case padding
                                               (diaper (f:fmt nil his/her " diaper"))
                                               (pullup (f:fmt nil his/her " pullups"))
                                               (closed-bottoms (f:fmt nil his/her " pants"))
                                               (t (f:fmt nil himherself)))
              " with a bright red blush on " (if malep "his" "her") " face when " he/she " realized " he/she " just messed " himherself #\Newline)
       (iter (for i in (if (typep user 'team-member)
                           (enemies-of *battle*)
                           (team-of *game*)))
         (set-status-condition 'yadfa-status-conditions:laughing i)
         (f:fmt* t (name-of i) " is laughing at " name #\Newline))))
    ((and (>= (bowels/contents-of user) (bowels/potty-desperate-limit-of user)))
     (mess :messer user)
     (let* ((padding (get-babyish-padding user))
            (name (name-of user))
            (malep (malep user))
            (his/her (if malep "his" "her"))
            (he/she (if malep "he" "she"))
            (himherself (if malep "himself" "herself")))
       (f:fmt t name " grabs the back of " (case padding
                                                 (diaper (f:fmt nil his/her " diaper"))
                                                 (pullup (f:fmt nil his/her " pullups"))
                                                 (closed-bottoms (f:fmt nil his/her " pants"))
                                                 (t (f:fmt nil himherself)))
              " with a bright red blush on " (if malep "his" "her") " face when " he/she " realized " he/she " just messed " himherself #\Newline)
       (iter (for i in (if (typep user 'team-member)
                           (enemies-of *battle*)
                           (team-of *game*)))
         (set-status-condition 'yadfa-status-conditions:laughing i)
         (f:fmt* t (name-of i) " is laughing at " name #\Newline))))
    ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
     (f:fmt t "FRRRT" #\Newline
            (name-of user) " sighs with relief" #\Newline)
     (iter (for i in (if (typep user 'team-member)
                         (enemies-of *battle*)
                         (team-of *game*)))
       (set-status-condition 'yadfa-status-conditions:skunked i)
       (f:fmt* t (name-of i) " is grossed out by the smell" #\Newline)))
    (t (f:fmt t "Nothing happened" #\Newline))))
(defclass spank (move) ()
  (:default-initargs
   :name "Spank"
   :energy-cost 5
   :power 10
   :description "Breathes fire at the enemy"))
(defmethod attack ((target base-character) (user base-character) (self spank))
  (let ((a (calculate-damage target user (power-of self)))
        (times (random 10)))
    (f:fmt t
           (name-of user) " bends " (name-of target) " over " (if (malep user) "his" "her") " knee and gives " (name-of target) " a spanking" #\Newline
           (:times "whap " times) #\Newline
           "Hit " times " times" #\Newline)
    (decf (health-of target) (* a times))
    a))
