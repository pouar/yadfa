;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(uiop:define-package #:peachs-castle-wannabe
  (:export
   #:blank-area
   #:pokemon-area
   #:thwomp-area
   #:race-area
   #:eggman-area))
(defun can-potty (prop &key wet mess pants-down user)
  (declare (ignorable prop wet mess pants-down user))
  (not (when (and (typep prop '(not yadfa-props:toilet)) (or pants-down (filter-items (wear-of user) 'incontinence-product)))
         (format t "STOP!!! THE SIGN SAYS ~A ISN'T ALLOWED TO DO THAT HERE!!!!! Just hold it all in.~%~%" (string-upcase (name-of user)))
         (when (or (>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)) (>= (bowels/contents-of user) (bowels/potty-dance-limit-of user)))
           (format t "*~a whines and continues ~a embarrassing potty dance while the public watches and giggles*~%~%"
                   (name-of user)
                   (if (malep user)
                       "his"
                       "her")))
         t)))
(defun can-potty-peachs-castle-wannabe (prop &key wet mess pants-down user)
  (declare (ignorable prop wet mess pants-down user))
  (not (when (or pants-down (filter-items (wear-of user) 'incontinence-product))
         (format t "STOP!!! ~A CAN'T DO THAT HERE!!!! THERE ARE LIKE KIDS HERE!!!!! Just hold it all in~%~%" (string-upcase (name-of user)))
         (when (or (>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)) (>= (bowels/contents-of user) (bowels/potty-dance-limit-of user)))
           (format t "*~a whines and continues ~a embarrassing potty dance while the public watches and giggles*~%~%"
                   (name-of user)
                   (if (malep user)
                       "his"
                       "her")))
         t)))
(defun potty-trigger-peachs-castle-wannabe (had-accident user)
  (when (or (and (getf (car had-accident) :leak-amount) (> (getf (car had-accident) :leak-amount) 0))
            (and (getf (cdr had-accident) :leak-amount) (> (getf (cdr had-accident) :leak-amount) 0)))
    (format t "Some kid: HEY LOOK!!!! THAT GUY JUST ~A ~ASELF!!!!!~%~%"
            (if (and (getf (cdr had-accident) :leak-amount) (> (getf (cdr had-accident) :leak-amount) 0))
                "MESSED"
                "WET")
            (if (malep user)
                "HIM"
                "HER"))
    (apply #'format t "*everybody points and laughs at ~a while ~a hides ~a face with ~a paws in embarrassment*~%~%"
           (name-of user)
           (if (malep user)
               '("he" "his" "his")
               '("she" "her" "her")))))
(defun trigger-diaper-police (had-accident user)
  (when (or (and (getf (car had-accident) :leak-amount) (> (getf (car had-accident) :leak-amount) 0))
            (and (getf (cdr had-accident) :leak-amount) (> (getf (cdr had-accident) :leak-amount) 0)))
    (format t "*Seems ~a's mess got the attention of the diaper police.*~%~%" (name-of user))
    (if (filter-items (wear-of user) 'padding)
        (progn (format t "Diaper Police: Seems this ~a needs a diaper change. Better change ~a~%~%" (species-of user) (if (malep user) "him" "her"))
               (format t "~a: Hey!!! Don't change me here!!! Everyone can see me!!!~%~%" (name-of user)))
        (progn (format t "Seems this ~a isn't potty trained, better diaper ~a~%~%" (species-of user) (if (malep user) "him" "her"))
               (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%" (name-of user))))
    (yadfa::change-the-baby user 'yadfa-items:kurikia-thick-diaper :locked t)
    (format t "*The diaper police straps the squirmy and heavily blushy ~a down on a public changing table, strips all of ~a's soggy clothes off (and all the clothes that won't fit over the new diaper), and puts a thick diaper on ~a. All while the local bystanders watch, snicker, giggle, and point*~%~%"
            (name-of user)
            (if (malep user) "he" "she")
            (if (malep user) "him" "her"))
    (format t "Diaper Police: And to be sure the baby keeps ~a diapers on~%~%" (if (malep user) "his" "her"))
    (format t "*The diaper police locks the diaper on to prevent ~a from removing it*~%~%" (name-of user))
    (format t "*The diaper police unstrap ~a from the table. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
            (name-of user)
            (name-of user)
            (if (malep user) "him" "her"))
    (format t "Diaper Police: Aww, it looks like the baby is learning how to walk for the first time~%~%")
    (format t "*~a whines and covers ~a face with ~a paws in embarrassment*~%~%"
            (name-of user) (if (malep user) "his" "her") (if (malep user) "his" "her"))
    (trigger-event 'yadfa-events:get-diaper-locked-1)))
(defevent initialize-enemy-spawn-list
  :lambda '(lambda (self)
            (declare (ignore self))
            (serapeum:dict*
             (enemy-spawn-list-of *game*)
             'bandits-way '((:max-random 8
                             :enemies ((yadfa-enemies:female-diapered-raccoon-bandit .
                                        `(:level ,(random-from-range 2 5)))))
                            (:max-random 8
                             :enemies ((yadfa-enemies:rookie-diapered-raccoon-bandit .
                                        `(:level ,(random-from-range 2 5))))))
             'bandits-cove '((:max-random 10
                              :enemies ((yadfa-enemies:rookie-diapered-raccoon-bandit .
                                         `(:level ,(random-from-range 2 5)
                                           :wear ,(list
                                                   (make-instance 'yadfa-items:lower-bandit-swim-diaper-cover)
                                                   (make-instance 'yadfa-items:bandit-diaper
                                                                  :sogginess (random 1000)
                                                                  :messiness (random 6000)))))))
                             (:max-random 10
                              :enemies ((yadfa-enemies:diapered-raccoon-bandit .
                                         `(:level ,(random-from-range 2 5)
                                           :wear ,(list (make-instance 'yadfa-items:bandit-swimsuit/closed)
                                                   (make-instance 'bandit-swim-diaper-cover)
                                                   (make-instance 'yadfa-items:bandit-diaper))))))
                             (:max-random 10
                              :enemies ((yadfa-enemies:female-diapered-raccoon-bandit .
                                         `(:level ,(random-from-range 2 5)
                                           :wear ,(list
                                                   (make-instance 'yadfa-items:bandit-uniform-sports-bikini-top)
                                                   (make-instance 'yadfa-items:female-bandit-swim-diaper-cover)
                                                   (make-instance 'yadfa-items:bandit-female-diaper
                                                                  :sogginess (random 1000)
                                                                  :messiness (random 6000))))))))
             'rpgmaker-dungeon '((:max-random 20
                                  :enemies ((yadfa-enemies:rookie-diapered-raccoon-bandit .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:padded-fursuiter-servant .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:fursuiter-servant .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:navy-officer .
                                             `(:level ,(random-from-range 2 5)))
                                            (yadfa-enemies:navy-officer* .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:diaper-pirate .
                                             `(:level ,(random-from-range 2 5)))
                                            (yadfa-enemies:thickly-diaper-pirate .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:diapered-raccoon-bandit .
                                             `(:level ,(random-from-range 2 5)))
                                            (yadfa-enemies:rookie-diapered-raccoon-bandit .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:diapered-raccoon-bandit .
                                             `(:level ,(random-from-range 2 5)))
                                            (yadfa-enemies:female-diapered-raccoon-bandit .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:diapered-kobold .
                                             `(:level ,(random-from-range 2 5)))
                                            (yadfa-enemies:diapered-kobold .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 25
                                  :enemies ((yadfa-enemies:diapered-dragon .
                                             `(:level ,(random-from-range 4 10)))
                                            (yadfa-enemies:diapered-kobold .
                                             `(:level ,(random-from-range 2 5)
                                               :wear ,(list (make-instance 'yadfa-items:kurikia-thick-diaper))))))
                                 (:max-random 25
                                  :enemies ((yadfa-enemies:diapered-dragon* .
                                             `(:level ,(random-from-range 4 10)))
                                            (yadfa-enemies:diapered-kobold .
                                             `(:level ,(random-from-range 2 5)
                                               :wear ,(list (make-instance 'yadfa-items:kurikia-thick-diaper))))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:diapered-skunk .
                                             `(:level ,(random-from-range 2 5)))))
                                 (:max-random 20
                                  :enemies ((yadfa-enemies:dergy .
                                             `(:level ,(random-from-range 2 5)))))))))
(trigger-event 'initialize-enemy-spawn-list)
