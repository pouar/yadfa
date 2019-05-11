(in-package :yadfa-zones)
(defun can-potty
    (prop &key wet mess pants-down user)
    (declare (ignorable prop wet mess pants-down user))
    (not
        (when (or
                  (typep prop '(not toilet))
                  pants-down
                  (not (filter-items (wear-of user) 'incontinence-product)))
            (format t "STOP!!! THE SIGN SAYS ~A ISN'T ALLOWED TO DO THAT HERE!!!!! Just hold it all in.~%~%"
                (string-upcase (name-of user)))
            (when (or
                      (>=
                          (bladder/contents-of user)
                          (bladder/potty-dance-limit-of user))
                      (>=
                          (bowels/contents-of user)
                          (bowles/potty-dance-limit-of user))))
            (format t "*~a whines and continues ~a embarrassing potty dance while the public watches and giggles*~%~%"
                (name-of user)
                (if (malep user)
                    "his"
                    "her"))
            t)))
(defun trigger-diaper-police (had-accident user)
    (when (or
              (and (getf (car had-accident) :leak-amount) (> (getf (car had-accident) :leak-amount) 0))
              (and (getf (cdr had-accident) :leak-amount) (> (getf (cdr had-accident) :leak-amount) 0)))
        (format t "*Seems ~a's mess got the attention of the diaper police.*~%~%"
            (name-of user))
        (if (filter-items (wear-of user) 'padding)
            (progn
                (format t "Diaper Police: Seems this ~a needs a diaper change. Better change ~a~%~%"
                    (species-of user)
                    (if (malep user) "him" "her"))
                (format t "~a: Hey!!! Don't change me here!!! Everyone can see me!!!~%~%"
                    (name-of user)))
            (progn
                (format t "Seems this ~a isn't potty trained, better diaper ~a~%~%"
                    (species-of user)
                    (if (malep user) "him" "her"))
                (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%"
                    (name-of user))))
        (yadfa::change-the-baby user 'yadfa-items:kurikia-thick-diaper :locked t)
        (format t "*The diaper police straps the squirmy and heavily blushy ~a down on a public changing table, strips all of ~a's soggy clothes off (and all the clothes that won't fit over the new diaper), and puts a thick diaper on ~a. All while the local bystandards watch, snicker, giggle, and point*~%~%"
            (name-of user)
            (if (malep user) "he" "she")
            (if (malep user) "him" "her"))
        (format t "Diaper Police: And to be sure the baby keeps ~a diapers on~%~%"
            (if (malep user) "his" "her"))
        (format t "*The diaper police locks the diaper on to prevent ~a from removing it*~%~%"
            (name-of user))
        (format t "*The diaper police unstrap ~a from the table. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
            (name-of user)
            (name-of user)
            (if (malep user) "him" "her"))
        (format t
            "Diaper Police: Aww, it looks like the baby is learning how to walk for the first time~%~%")
        (format t "*~a whines and covers ~a face with ~a paws in embarrassment*~%~%"
            (name-of user)
            (if (malep user) "his" "her")
            (if (malep user) "his" "her"))
        (trigger-event 'yadfa-events:get-diaper-locked-1)))
