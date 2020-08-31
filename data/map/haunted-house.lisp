;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 haunted-house)
             :name "Haunted House Entrance"
             :description "You're at a entrance of a haunted house"
             :enter-text "You walk to the entrance of the haunted house"
             :direction-attributes (list 'haunted-forest (list :exit-text "You exit the haunted house"))
             :warp-points (list 'haunted-forest '(6 -2 0 haunted-forest)))
(ensure-zone (1 0 0 haunted-house)
             :name "Haunted House Hallway"
             :description "You're in a spooky haunted house."
             :enter-text "You're wandering around the haunted house")
(ensure-zone (-1 0 0 haunted-house)
             :name "Haunted House Hallway"
             :description "You're in a spooky haunted house."
             :enter-text "You're wandering around the haunted house")
(ensure-zone (-1 -1 0 haunted-house)
             :name "Haunted House Stairwell"
             :description "You're in a spooky haunted house."
             :enter-text "You're wandering around the haunted house"
             :stairs (list :up)
             :direction-attributes (list :up (list :exit-text "You head down the stairs")))
(ensure-zone (-1 -1 1 haunted-house)
             :name "Haunted House Stairwell"
             :description "You're in a spooky haunted house."
             :enter-text "You're wandering around the haunted house"
             :stairs (list :down)
             :direction-attributes (list :down (list :exit-text "You head up the stairs")))
(ensure-zone (1 -1 0 haunted-house)
             :name "Haunted Kitchen"
             :description "You're in a spooky haunted house."
             :enter-text "You enter the kitchen")
(defun highchairfunction% (prop &rest keys &key &allow-other-keys)
  (declare (type prop prop) (ignore prop keys))
  (write-string #.(with-output-to-string (s)
                    (format s "Strange voice: Looks like da babies are hungy~%~%")
                    (format s "*Before you have a chance to react a bunch of ghost hands pick your team up like an infants and places them in the high chairs*~%~%")
                    (format s "*You struggle in the chair while a ghost hand starts force feeding you baby food glop and a bottle.~%~%")))
  (let* (wet mess
             (team (cons (player-of *game*) (allies-of *game*))))
    (declare (type list team wet mess))
    (iter (for c in team)
          (setf (health-of c) (calculate-stat c :health))
          (setf (energy-of c) (calculate-stat c :energy))
          (incf (bladder/contents-of c) 500)
          (incf (bowels/contents-of c) 300)
          (when (>= (bladder/contents-of c)
                    (bladder/maximum-limit-of c))
            (lappendf wet (list c (wet :wetter c))))
          (when (>= (bowels/contents-of c)
                    (bowels/maximum-limit-of c))
            (lappendf mess (list c (mess :messer c)))
            (setf mess (append (list c (mess :messer c)) mess)))
          (let (a b c e f
                  (d (iter (for (k v) on mess by 'cddr)
                           (when (> (getf v :mess-amount) 0)
                             (collect (name-of k) at start)))))
            (declare (type list a b c d e f))
            (iter (for i in team)
                  (push (name-of i) e)
                  (when (filter-items (wear-of i) 'padding)
                    (push (name-of i) f)))
            (iter (for (k v) on wet by 'cddr)
                  (cond ((> (getf v :leak-amount) 300)
                         (push (name-of k) a))
                        ((> (getf v :leak-amount) 100)
                         (push (name-of k) b))
                        ((> (getf v :wet-amount) 0)
                         (push (name-of k) c))))
            (flet ((temp (array control plural male female)
                     (when array
                       (apply 'format control array (cond ((cdr array) plural)
                                                          ((malep (car array)) male)
                                                          (t female))))))
              (temp a "*~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} presses ~a legs together as ~a bladder~a gives out and a yellow stream flows down ~a highchair~a*~%~%"
                    '("their" "their" "s" "their" "s")
                    '("his" "his" "" "his" "")
                    '("her" "her" "" "her" ""))
              (temp b "*~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} presses ~a legs together as ~a bladder~a gives out and a puddle forms in ~a seat~a*~%~%"
                    '("their" "their" "s" "their" "s")
                    '("his" "his" "" "his" "")
                    '("her" "her" "" "her" ""))
              (temp c "*~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} presses ~a legs together as ~a bladder~a gives out and flood~a ~a pamps*~%~%"
                    '("their" "their" "s" "" "their")
                    '("his" "his" "" "s" "his")
                    '("her" "her" "" "s" "her"))
              (temp d "*~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} presses ~a legs together as a loud blort is heard as ~a mess~a ~aself*~%~%"
                    '("their" "they" "" "themselves")
                    '("his" "his" "es" "himself")
                    '("her" "her" "es" "herself"))
              (temp e "*The ghost hands picks ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} up out of the highchair~s and sets ~a on the floor*~%"
                    '("s" "them")
                    '("" "him")
                    '("" "her"))
              (temp f "*then gives ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} pats on ~a pamps*~%~%"
                    '("their")
                    '("his")
                    '("her")))))))
(ensure-zone (2 -1 0 haunted-house)
             :name "Haunted Dining Room"
             :description "You're in a spooky haunted house."
             :enter-text "You enter the dining room"
             :props (list :highchair (make-instance 'prop
                                                    :name "High Chairs"
                                                    :description "Several high chairs that is big enough to hold an adult"
                                                    :actions (list :use (make-action :documentation "Look at the high chair" :lambda 'highchairfunction%)))))
(ensure-zone (2 0 0 haunted-house)
             :name "Haunted Living Room"
             :description "You're in a spooky haunted house."
             :enter-text "You enter the living room")
