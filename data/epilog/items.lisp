;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defmethod catch-method ((item enemy-catcher) (target yadfa-enemies:catchable-enemy))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (out (name-of item) " can't hold anymore enemies" :% :%))
    ((not (< (random 1.0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance-of target)))))
     (out "You failed to catch the " (name-of target) :% :%)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (out "You caught the " (name-of target) :% :%)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)
     (remf (status-conditions-of *battle*) target)

     ;; these may break the save file since functions don't serialize very well
     ;; and they will never get called after the enemy is caught, so just delete these
     (setf (yadfa-enemies:catch-chance-of target) nil
           (battle-script-of target) nil
           (default-attack-of target) nil
           (process-battle-accident-of target) nil
           (process-potty-dance-of target) nil)

     (push target (contained-enemies-of item))
     (when (getf (special-actions-of item) :take-items)
       (setf (getf (special-actions-of item) :take-items)
             '(lambda (item user &key &allow-other-keys)
               (declare (ignore user))
               (setf (inventory-of (player-of *game*))
                (append (iter (for enemy in (contained-enemies-of item))
                          (dolist (item (inventory-of enemy))
                            (collect item))
                          (dolist (item (wear-of enemy))
                            (collect item))
                          (setf (inventory-of enemy) nil
                                (wear-of enemy) nil))
                 (inventory-of (player-of *game*)))))))
     (when (getf (special-actions-of item) :adopt-enemies)
       (setf (getf (special-actions-of item) :adopt-enemies)
             '(lambda (item user &allow-other-keys :enemies enemies)
               (if (iter (for i in (contained-enemies-of item))
                     (when (typep (class-of i) 'yadfa-enemies:adoptable-enemy)
                       (return t)))
                (progn
                  (setf enemies
                        (typecase enemies
                          (null (accept-with-effective-frame
                                  (clim:accepting-values (*query-io*  :resynchronize-every-pass t)
                                    (setf enemies (clim:accept `(clim:subset-alist ,(iter (for enemy in (contained-enemies-of item))
                                                                                      (when (typep (class-of i) 'yadfa-enemies:adoptable-enemy)
                                                                                        (collect (cons (name-of enemy) enemy)))))
                                                               :prompt "Enemies to adopt"
                                                               :stream *query-io*
                                                               :view clim:+check-box-view+)))))
                          (type-specifier (iter (for enemy in (contained-enemies-of item))
                                            (when (typep i enemies)
                                              (collect i))))
                          (list (iter
                                  (for enemy in (contained-enemies-of item))
                                  (generate current in enemies)
                                  (for index upfrom 0)
                                  (cond ((typep current '(not unsigned-byte))
                                         (error "ENEMIES must be a list of unsigned-bytes"))
                                        ((eql index current)
                                         (collect enemy)
                                         (next current)))))
                          (t (error "ENEMIES must either be a list of unsigned-bytes or a type specifier"))))
                  (alexandria:removef (contained-enemies-of item) enemies
                                      :test (lambda (o e)
                                              (member e o)))
                  (alexandria:appendf (allies-of *game*) (iter (for i in enemies)
                                                           (write-line (yadfa-enemies:change-class-text i))
                                                           (collect (change-class i (get (class-name i) 'yadfa-enemies:change-class-target))))))
                (format t "No enemies in there to adopt"))))))))
(defmethod catch-method ((item enemy-catcher) (target yadfa-enemies:ghost))
  (out "You failed to catch " (name-of target) :% :%)
  (cond ((eq (device-health-of item) t) nil)
        ((<= (device-health-of item) 1)
         (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
        (t (decf (device-health-of item)))))
(defmethod catch-method ((item ghost-catcher) (target yadfa-enemies:ghost))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (out (name-of item) " can't hold anymore enemies" :% :%))
    ((not (< (random 1.0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance-of target)))))
     (out "You failed to catch the " (name-of target) :% :%)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (out "You caught the " (name-of target) :% :%)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)
     (remf (status-conditions-of *battle*) target)

     ;; these may break the save file since functions don't serialize very well
     ;; and they will never get called after the enemy is caught, so just delete these
     (setf (yadfa-enemies:catch-chance-of target) nil
           (battle-script-of target) nil
           (default-attack-of target) nil
           (process-battle-accident-of target) nil
           (process-potty-dance-of target) nil)

     (push target (contained-enemies-of item)))))
(defunassert (yadfa-battle-commands:catch-enemy (&optional (target 'yadfa-enemies:catchable-enemy) (item 'enemy-catcher))
                                                "Catches an enemy using. @var{ITEM} which is a type specifier. @var{TARGET} is an index or type specifier of an enemy in battle or a type specifier")
    (item type-specifier
          target (or unsigned-byte type-specifier))
  (let ((selected-item (find item (inventory-of (player-of *game*))
                             :test (lambda (type-specifier obj)
                                     (and (typep obj `(and enemy-catcher ,type-specifier))
                                          (< (list-length (contained-enemies-of item))
                                             (contained-enemies-max-length-of item))))))
        (selected-target (let ((a (typecase target
                                    (unsigned-byte (nth target (enemies-of *battle*)))
                                    (type-specifier (find target (enemies-of *battle*)
                                                          :test (lambda (type-specifier obj)
                                                                  (typep obj `(and yadfa-enemies:catchable-enemy ,type-specifier))))))))
                           (or a
                               (progn
                                 (write-line "That target doesn't exist")
                                 (return-from yadfa-battle-commands:catch-enemy))))))
    (cond ((not item)
           (format t "You don't have an item with that type specifier that can catch that enemy~%")
           (return-from yadfa-battle-commands:catch-enemy))
          ((typep selected-target '(not yadfa-enemies:catchable-enemy))
           (format t "That enemy can't be caught~%")
           (return-from yadfa-battle-commands:catch-enemy)))
    (process-battle
     :item selected-item
     :selected-target selected-target)))
(defunassert (yadfa-world-commands:loot-caught-enemies (&optional item)
                                                       "Loots the enemies you caught. @var{ITEM} is either a type specifier or an unsiged-byte of the item. Don't specify if you want to loot the enemies of all items")
    (item (or null unsigned-byte type-specifier))
  (cond ((null item)
         (iter (for item in (inventory-of *game*))
           (when (typep item 'enemy-catcher)
             (funcall (coerce (action-lambda (getf (special-actions-of item) :take-items)) 'function)
                      item (player-of *game*) :action :take-items))))
        ((typep item 'unsigned-byte)
         (let* ((inventory-length (list-length (inventory-of (player-of *game*))))
                (selected-item (and (< item inventory-length) (nth item (inventory-of (player-of *game*))))))
           (cond ((>= item inventory-length)
                  (out "You only have " inventory-length " items" :%))
                 ((not (typep selected-item 'enemy-catcher))
                  (out "That item isn't an enemy catcher" :%))
                 (t (funcall (coerce (action-lambda (getf (special-actions-of selected-item) :take-items)) 'function)
                             selected-item (player-of *game*) :action :take-items)))))
        (t (let ((selected-item (find item (inventory-of *game*) :test (lambda (specifier item)
                                                                         (typep item `(and enemy-catcher ,specifier))))))
             (if selected-item
                 (funcall (coerce (action-lambda (getf (special-actions-of selected-item) :take-items)) 'function)
                          selected-item (player-of *game*) :action :take-items)
                 (out "Either you don't have that item or it isn't an enemy catcher" :%))))))
